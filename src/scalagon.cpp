#include "scalagon.h"

// There is no scalagon_implementation here, this is done in psel-par.cpp and psel-par-top.cpp

// Helper for getting random numbers (R specific),
// should not be in the worker thread, as this accesses the R API (unif_rand()).
std::vector<int> get_sample(int ntuples)
{
  if (ntuples < scalagon::scalagon_min_tuples) return std::vector<int>(); // no sample needed, not enough tuples
  std::vector<int> res;
  res.reserve(scalagon::sample_size);
  ntuples--;
  for (int i = 0; i < scalagon::sample_size; i++) {
    res.push_back(floor(ntuples * unif_rand()));
  }
  return res;
}

// --------------------------------------------------------------------------------------------------------------------------------

// Main class of the Scalagon Algorithm
//
// See "Scalagon: An Efficient Skyline Algorithm for all Seasons",
// M.Endres, P.Roocks, W.Kießling,
// 20th International Conference on Database Systems for Advanced Applications(DASFAA 2015), Hanoi, Vietnam,
// 10.1007/978-3-319-18123-3_18.


// --------------------------------------------------------------------------------------------------------------------------------


// Constructors / Destructors
scalagon::scalagon(bool sample_precalc) : sample_precalc(sample_precalc) {}

// Put all pareto/intersection preferences of a tree into a std::vector
// Returns true if successful, false if not (found non-productpref), results are in m_prefs
bool scalagon::get_prefs(const ppref& p)
{
  // Try to cast to productpref (pareto or intersection)
  std::shared_ptr<productpref> pref = std::dynamic_pointer_cast<productpref>(p); 
  
  if (pref == 0) { 
    // Now we assume a scorepref
    std::shared_ptr<scorepref> spref = std::dynamic_pointer_cast<scorepref>(p); 
    if (spref != 0) {
      m_prefs.push_back(spref);
      return true;
    } else {
      return false; // no score preference => not purely pareto/intersection!
    }
  } else {
    // Recursively call other prefs 
    return get_prefs(pref->p1) && get_prefs(pref->p2);
  }
}

// Interface to Scalagon/BNl for non-top k - top-k has other return type! (pair_vector)
std::vector<int> scalagon::run(const std::vector<int>& v, const ppref& p, double alpha)
{
  if (init(v, p, alpha)) { // return false if input does not suit
    
    // *** Domination phase
    dominate(std::vector<int>(), p); // empty vecor - no index needed for non-topk Scalagon
    
    // **** Filtering: Add all not dominated tuples to filtered result (outliers are already in, done by init!)
    const int scount = m_stuples_v.size();
    
    for (int i = 0; i < scount; i++) {
      int btg_ind = get_index_tuples(i); // get from stuples [0,...,scount-1]
      if (!m_btg[btg_ind]) {
        m_filt_res.push_back(v[m_stuples_v[i]]);
      }
    }
    
    // Run BNL on filtered data set
    return bnl_alg.run(m_filt_res, p);
    
  } else { 
    // Run just BNL on original data set
    return bnl_alg.run(v, p);
  }
}

// ------------------------------------------------------------------------------------------------------------------------------------------------------


// scalagon top k with/without levels
flex_vector scalagon::run_topk(const std::vector<int>& v, const ppref& p, const topk_setting& ts, double alpha, bool show_levels)
{

  if (ts.is_simple) {
    //return std scalagon WITHOUT levels - finalt_result_pair_vector is empty!
    if (!show_levels) return flex_vector(run(v, p, alpha), pair_vector()); 
    else              return flex_vector(std::vector<int>(), bnl_alg.add_level(run(v, p, alpha), 1)); // ... WITH LEVELS, final_result_vector is empty
  }
  
  std::vector<int> final_result_vector;
  pair_vector final_result_pair_vector; // Pairs of level and tuple index

  const int ntuples = v.size();  
  if (show_levels) final_result_pair_vector.reserve(ntuples);
  else             final_result_vector.reserve(ntuples);

  if (init(v, p, alpha)) { // use Scalagon

    const int s_ind_count = m_stuples_v.size(); // Number of center tuples / scaled tuples (non-outliers), in the loop: tuples still in s_indices set

    // Vectors for Std BNL, make pairs to store the s-index
    pair_vector remainder_pairs;
    pair_vector index_pairs;
    remainder_pairs.reserve(ntuples);
    index_pairs.reserve(ntuples);

    // Add outliers to pair_vector
    for (int u : m_filt_res) {
      index_pairs.push_back(std::pair<int, int>(u, -1)); // outliers => they have no s-index
    }
    m_filt_res.clear(); // this is not needed anymore!
    
    // indices for scalagon
    std::vector<int> s_indices(s_ind_count);
    for (int i = 0; i < s_ind_count; i++) s_indices[i] = i;

    // Level and result set counter
    int nres = 0;
    int level = 1;

    while (true) {

      // *** Domination phase
      dominate(s_indices, p);

      // **** Filtering: Add all not dominated tuples to filtered result (outliers are already in!)
      for (int cur_s_sind : s_indices) {
        int btg_ind = get_index_tuples(cur_s_sind); // get from stuples [0,...,scount-1]
        if (!m_btg[btg_ind]) {
          // Add v-number and s-index to the index_pair std::vector
          index_pairs.push_back(std::pair<int, int>(v[m_stuples_v[cur_s_sind]], cur_s_sind));
        } else {
          // not in Scalagon filtered tuples => add to remainder
          remainder_pairs.push_back(std::pair<int, int>(v[m_stuples_v[cur_s_sind]], cur_s_sind));
        }
      }

      // **** Run BNL on filtered set

      // gets index_pairs and remainder_pairs as reference
      pair_vector res = bnl_alg.run_remainder_paired(index_pairs, remainder_pairs, p);

      // increment counter
      const int rsize = res.size();
      if (rsize == 0) break; // no more tuples
      nres += rsize;

      // Add to final results
      for (const std::pair<int,int>& u : res) {
        if (show_levels) final_result_pair_vector.push_back(std::pair<int, int>(level, u.first));
        else             final_result_vector     .push_back(u.first);
      }

      // ** Break?
      if (ts.do_break(level, nres)) break;
      level++;

      // ** Prepare for next step

      index_pairs.clear();
      s_indices.clear();

      // Add all outliers from the remainder to index_pairs and all center-tuples to s_indices
      // this are all subsets, hence there is enough space
      // start with HIGHEST indices, as these are the better dominating tuples!
      for (auto i = remainder_pairs.rbegin(); i != remainder_pairs.rend(); ++i) { 
        if (i->second == -1) { //outlier => add directly to index_pairs
          index_pairs.push_back(*i);
        } else { // not outlier => first run scalagon, then add to index_pairs (at begin of loop)
          s_indices.push_back(i->second);
        }
      }
      
      remainder_pairs.clear();
    }

    // Cut in the top-k case
    if (show_levels) ts.cut(final_result_pair_vector);
    else             ts.cut(final_result_vector);

  } else { // use Standard BNL
    if (show_levels) final_result_pair_vector = bnl_alg.run_topk_lev(v, p, ts);
    else             final_result_vector      = bnl_alg.run_topk(    v, p, ts);
  }

  return flex_vector(final_result_vector, final_result_pair_vector);
}



// ------------------------------------------------------------------------------------------------------------------------------------------------------

// Helper functions for prefiltering

int scalagon::get_index_tuples(int ind)
{
  int res = m_stuples[0][ind];
  for (int i = 1; i < m_dim; i++) res += m_weights[i] * m_stuples[i][ind];
  return res;
}

int scalagon::get_index_pt(const std::vector<int>& pt)
{
  int res = pt[0];
  for (int i = 1; i < m_dim; i++) res += m_weights[i] * pt[i];
  return res;
}

// Calculate the scale std::vector for a given domain size and btg size
std::vector<int> scalagon::iterated_scaling(const std::vector<int>& domain_size, double btg_size)
{
  std::vector<bool> reached_domain_limit;
  
  // Initialization
  std::vector<int> scale_fct(m_dim);
  for (int k = 0; k < m_dim; k++) {
    // Use ceil (not floor) to avoid that 1.9 is floored to 1 - Lattice calculation would become senseless!
    scale_fct[k] = (int)ceil(pow(btg_size, 1.0 / m_dim));
  }
  
  reached_domain_limit = std::vector<bool>(m_dim);
  
  int num_reached = 0;
  
  while (true) {
    // Check for dimensions where scale_fct is larger than estimated domain size
    bool found_new = false;
    double prod_reached = 1;
    num_reached = 0;
    for (int k = 0; k < m_dim; k++) {
      if (!reached_domain_limit[k] && domain_size[k] < scale_fct[k]) {
        // domain limit reached for this dimension
        scale_fct[k] = domain_size[k]; // this is an integer!
        reached_domain_limit[k] = true;
        prod_reached *= scale_fct[k];
        num_reached++;
        found_new = true;
      }
    }
    if (!found_new) break;
    
    // Rescale all dimensions, where domain limit is not reached
    int scale_val = (int)ceil(pow(btg_size / prod_reached, 1.0 / (m_dim - num_reached)));
    for (int k = 0; k < m_dim; k++) {
      if (!reached_domain_limit[k]) scale_fct[k] = scale_val; // increases scale factor
    }
    // after increasing scale factors, re-check if domain limit is reached
  }
  
  return scale_fct;
}


// --------------------------------------------------------------------------------------------------------------------------------------------------------


/* Scalagon Prefiltering initialization with

* Rejection of unsuited input (domain too small, too few tuples)
* sample based bounding box, where only the center of the tuples are considered
* Scalagon iterated scaling calculation (according to the paper)

* RETURNS
- false, if input is rejected (use BNL then!)
- true, if the initialization was successful
*/

bool scalagon::init(const std::vector<int>& v, const ppref& p, double alpha)
{
  // consts for sampling
  const int lower_quantile = 19; // 2 % and 98 % quantile
  const int upper_quantile = 979;
  const double add_spread_fct = 0.2; // Add to lower/upper quantiles 
  
  // **** Get preferences / preliminary checks
  
  if (alpha <= 0) return false; // reject alpha=-1, alpha=0
  m_prefs.clear(); // clear preference list
  // Get preferences, check if all pareto and at least two preferences
  if (!get_prefs(p) || m_prefs.size() < 2) return false;
  
  // **** Precalculations for Scalagon
  
  const int ntuples = v.size();
  if (ntuples < scalagon_min_tuples) return false; // not enough tuples, DO NOT USE Scalagon, use BNL
  m_dim = m_prefs.size();
  
  // Calc samples
  if (!sample_precalc) sample_ind = get_sample(ntuples);
  
  // lower and upper bounds for the "center" where most tuples are expected
  std::vector<double> upper_bound(m_dim);
  std::vector<double> lower_bound(m_dim);
  
  // estimation of domain size
  std::vector<int> est_domain_size(m_dim);
  
  // Calculate upper/lower bound by considering the sample in each dimension
  // Note that sample_ind is already calculated (is given to the constructor)
  for (int k = 0; k < m_dim; k++) {
    
    // Set for calculating domain size
    std::set<double> sample_set;
    std::set<double>::iterator it;
    
    // Vector for calculating quantiles
    std::vector<double> sample(sample_size);
    
    // Pick sample
    for (int i = 0; i < sample_size; i++) {
      double val = m_prefs[k]->data[v[sample_ind[i]]];
      sample[i] = val;
      sample_set.insert(val);
    }
    
    // Sort sample to calculate quantiles
    std::sort(sample.begin(), sample.end());
    double add_dist = (sample[upper_quantile] - sample[lower_quantile]) * add_spread_fct;
    lower_bound[k] = sample[lower_quantile] - add_dist;
    upper_bound[k] = sample[upper_quantile] + add_dist;
    
    // Heuristic for domain size estimation: distinct sample set is larger then 3/4 of sample size => Assume continuous domain
    int dom_size = sample_set.size();
    if (dom_size > 3 * sample_size / 4) {
      est_domain_size[k] = ntuples; // Asumme domain size is "very large"
    } else {
      est_domain_size[k] = dom_size; // Small dom_size: Assume dom_size is the correct domain size
      if (est_domain_size[k] == 1) return false; // If domain size is 1, DO NOT USE Scalagon - use BNL!
    }
  }
  
  // **** Get scaling
  
  const double dst_btg_size = 1.0 * ntuples / alpha;
  
  m_scale_fct = iterated_scaling(est_domain_size, dst_btg_size);
  
  // ***** Run scalagon with given scaling
  
  m_filt_res.clear();
  m_filt_res.reserve(ntuples);
  
  // Calculate downscaling factor for "center" of tuples (lower/upper bound) 
  // (everything will be scaled to [0, ..., scale_fct-1])
  std::vector<double> fct(m_dim);
  for (int k = 0; k < m_dim; k++) {
    fct[k] = 1.0 * m_scale_fct[k] / (upper_bound[k] - lower_bound[k]);
  }
  
  // **** Scaling
  
  // Prepare vectors for scaled tuples
  m_stuples = std::vector<std::vector<int>>(m_dim); // class variable
  for (int k = 0; k < m_dim; k++) m_stuples[k] = std::vector<int>(ntuples);
  m_stuples_v = std::vector<int>(ntuples); // local variable for v-indices
  
  // Do the scaling
  int scount = 0;
  for (int i = 0; i < ntuples; i++) {
    m_stuples_v[scount] = i; // v-index of scaled variable (will be overwritten if scount not incremented)
    for (int k = 0;; k++) {
      int val = (int)floor(fct[k] * (m_prefs[k]->data[v[i]] - lower_bound[k]));
      if (val < 0 || val >= m_scale_fct[k]) {
        m_filt_res.push_back(v[i]);
        break;
      } else {
        m_stuples[k][scount] = val; // Scaled value (will be overwritten if scount is not incremented!)
        if (k == m_dim - 1) {
          scount++;
          break;
        }
      }
    }
  }
  
  // Cut away filtered tuples (outliers)
  for (int k = 0; k < m_dim; k++) m_stuples[k].resize(scount);
  m_stuples_v.resize(scount);
  
  // **** Lattice preparation
  
  // Calculate actual btg size
  m_btg_size = 1;
  for (int k = 0; k < m_dim; k++) m_btg_size *= m_scale_fct[k];
  
  // Calculate weights
  m_weights = std::vector<int>(m_dim);
  m_weights[0] = 1;
  for (int k = 1; k < m_dim; k++) {
    m_weights[k] = m_scale_fct[k - 1] * m_weights[k - 1];
  }
  
  // Initialization successful
  return true;
}

// --------------------------------------------------------------------------------------------------------------------------------------------------------


/*  Scalagon domination phase with

* creating the BTG
* marking all dominated nodes with ones
*/

// mark all dominated nodes with true in m_btg
void scalagon::dominate(const std::vector<int>& s_ind, const ppref& p)
{
  // Create BTG and fill with zeros
  m_btg = std::vector<bool>(m_btg_size);
  
  // Number of scaled tuples
  
  bool use_ind = !s_ind.empty(); // is only true if s_ind is set - just for top-k!
  const int scount = use_ind ? s_ind.size() : m_stuples_v.size();
  
  // Helper variables for domination
  std::vector<int> start_dom(m_dim);
  std::vector<int> pt(m_dim);       // temporary point
  std::vector<int> stop_dom(m_dim); // stop coordinates (may be outside of btg!)
  std::vector<int> domcount(m_dim); // Counter 
  std::vector<int> domsteps(m_dim); // Steps to dominate
  
  // **** Run productorder hexagon
  for (int i = 0; i < scount; i++) {
    
    [&]() {
      // Get index
      const int cur_s_ind = use_ind ? s_ind[i] // use index (topk)
                                    : i; // don't use index (no top k)
      
      // ** Read tuple and preliminary checks
      int ind = get_index_tuples(cur_s_ind); // get from stuples [0,...,scount-1]
      if (m_btg[ind]) return; // already dominated, skip tuple
      
      // ** Search for start-domination point (left upper corner)
      int start_dom_ind = ind;
      for (int k = 0; k < m_dim; k++) {
        start_dom[k] = m_stuples[k][cur_s_ind] + 1;
        start_dom_ind += m_weights[k];
        if (start_dom[k] == m_scale_fct[k]) return; // outside btg - nothing to dominate!
      }
      
      if (m_btg[start_dom_ind]) return; // Domination area already dominated, skip tuple
      
      // ** Search for stop-domination point (right lower corner)
      for (int k = 0; k < m_dim; k++) pt[k] = start_dom[k];
      for (int k = 0; k < m_dim; k++) {
        // Set stops for stop domination search (optimization trick for for-loop)
        pt[k] = m_scale_fct[k] - 1;
        int tind = get_index_pt(pt);
        pt[k] = start_dom[k];
        if (m_btg[tind] == false) {
          stop_dom[k] = m_scale_fct[k]; // Stop at border (point is outside btg!)
        } else {
          // Calculate the stop point (stop before border!)
          stop_dom[k] = start_dom[k];
          for (int l = start_dom_ind;; l += m_weights[k]) {
            if (m_btg[l]) break;
            stop_dom[k]++;
          }
        }
        
        // domination steps to do
        domsteps[k] = stop_dom[k] - start_dom[k];
        if (domsteps[k] == 0) return; // empty rectangle, skip tuple
        
        // set counter back to 0
        domcount[k] = 0;
      }
      
      // ** Do the domination
      int cind = get_index_pt(pt); // Counter for domination phase
      int stop0 = cind + domsteps[0]; // Stop number for dimension 0
      
      while (true) {
        m_btg[cind] = true;
        cind++; // increment in dimension 0
        if (cind == stop0) { // Reached end of line
          // Line jump
          cind -= domsteps[0];
          for (int k = 1;; k++) { // Dimension 0 is already incremented above
            domcount[k]++; // increment in next dimension
            cind += m_weights[k];
            if (domcount[k] == domsteps[k]) {
              // Reached limit in all dimensions?
              if (k == m_dim - 1) return; // Done with domination, next tuple
              // Prepare for next dimension
              domcount[k] = 0;
              cind -= domsteps[k] * m_weights[k];
            } else {
              break;
            }
          }
          stop0 = cind + domsteps[0];
        }
      }
    }();
  }
}
