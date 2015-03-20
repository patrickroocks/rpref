#include <Rcpp.h>
using namespace Rcpp;


#include "scalagon.h"


// There is no scalagon_implementation here, this is done in psel-par.cpp and psel-par-top.cpp


// Helper for getting random numbers (R specific)
// should not be in the worker thread, as this accesses the R API (unif_rand())
std::vector<int> get_sample(int ntuples) {
  if (ntuples < scalagon::scalagon_min_tuples) return(std::vector<int>()); // no sample needed, not enough tuples
  std::vector<int> res(scalagon::sample_size);
  ntuples--;
  for (int i = 0; i < scalagon::sample_size; i++)
  	res[i] = std::floor(ntuples * unif_rand());
	return(res);
}


// --------------------------------------------------------------------------------------------------------------------------------
// From here from VS
// --------------------------------------------------------------------------------------------------------------------------------


// --------------------------------------------------------------------------------------------------------------------------------

// Main class of the Scalagon Algorithm
//
// See "Scalagon: An Efficient Skyline Algorithm for all Seasons",
// M.Endres, P.Roocks, W.Kie?ling
// 20th International Conference on Database Systems for Advanced Applications(DASFAA 2015), Hanoi, Vietnam

// --------------------------------------------------------------------------------------------------------------------------------


// Constructors / Destructors
scalagon::scalagon(bool sample_precalc) : sample_precalc(sample_precalc) {}
scalagon::~scalagon() {}


// Put all pareto/intersection preferences of a tree into a vector
// Returns true if successful, false if not (found non-productpref), results are in m_prefs
bool scalagon::get_prefs(pref* p) {

  // Try to cast to productpref (pareto or intersection)
	productpref *pref = dynamic_cast<productpref*>(p);
	if (pref == 0) { 
		// Now we assume a scorepref
		scorepref *spref = dynamic_cast<scorepref*>(p);
		if (spref != 0) {
			m_prefs.push_back(spref);
			return(true);
		} else return(false); // no score preference => not purely pareto/intersection!
	} else {
		// Recursively call other prefs 
		return(get_prefs(pref->p1) && get_prefs(pref->p2));
	}
}

// Interface to Scalagon/BNl for non-top k - top-k has other return type! (pair_list)
std::list<int> scalagon::run_scalagon(std::vector<int>& v, pref* p, double alpha) {

	if (scalagon_init(v, p, alpha)) { // return false if input does not suit

		// *** Domination phase
		std::vector<int> v_empty; // Create empty vector to allow reference
		dominate(v_empty, p); // empty vecor - no index needed for non-topk scalagon

		// **** Filtering: Add all not dominated tuples to filtered result (outliers are already in, done by init!)
		int scount = m_stuples_v.size();
		for (int i = 0; i < scount; i++) {
			int btg_ind = get_index_tuples(i); // get from stuples [0,...,scount-1]
			if (!m_btg[btg_ind]) {
				m_filt_res[m_filt_count] = v[m_stuples_v[i]];
				m_filt_count++;
			}
		}

		// Run BNL on filtered data set
		m_filt_res.resize(m_filt_count);
		return bnl_internal(m_filt_res, p);
	
	} else { 
		// Run just BNL on original data set
		return bnl_internal(v, p);
	}
}


// ------------------------------------------------------------------------------------------------------------------------------------------------------


// scalagon top k with/without levels
flex_list scalagon::run_scalagon_topk(std::vector<int>& v, pref* p, topk_setting& ts, double alpha, bool show_levels) {

	std::list<int> final_result_list;
	pair_list final_result_pair_list; // Pairs of level and tuple index

	if (ts.is_simple) {
		if (!show_levels) return(flex_list(run_scalagon(v, p, alpha), final_result_pair_list)); //return std scalagon WITHOUT levels
		else              return(flex_list(final_result_list,         add_level(run_scalagon(v, p, alpha), 1))); // ... WITH LEVELS
	}

	if (scalagon_init(v, p, alpha)) { // use Scalagon

		int ntuples = v.size(); // Entire number of tuples
		int s_ind_count = m_stuples_v.size(); // Number of center tuples / scaled tuples (non-outliers), in the loop: tuples still in s_indices set

		// Vectors for Std BNL, make pairs to store the s-index
		pair_vector remainder_pairs(ntuples);
		pair_vector index_pairs(ntuples);

		// Add outliers to pair_list
		for (int i = 0; i < m_filt_count; i++) index_pairs[i] = std::pair<int, int>(m_filt_res[i], -1); // outliers => they have no s-index
		m_filt_res.clear(); // this is not needed anymore!
		int paircount = m_filt_count;

		// indices for scalagon
		std::vector<int> s_indices(s_ind_count);
		for (int i = 0; i < s_ind_count; i++) s_indices[i] = i;

		// Level and result set counter
		int nres = 0;
		int level = 1;

		// Count of remainder tuples
		int remcount = 0;

		while (true) {

			// *** Domination phase
			dominate(s_indices, p);

			// **** Filtering: Add all not dominated tuples to filtered result (outliers are already in!)
			for (int i = 0; i < s_ind_count; i++) {
				int cur_s_sind = s_indices[i];
				int btg_ind = get_index_tuples(cur_s_sind); // get from stuples [0,...,scount-1]
				if (!m_btg[btg_ind]) {
					// Add v-number and s-index to the index_pair vector
					index_pairs[paircount] = std::pair<int, int>(v[m_stuples_v[cur_s_sind]], cur_s_sind);
					paircount++;
				} else {
					// not in Scalagon filtered tuples => add to remainder
					remainder_pairs[remcount] = std::pair<int, int>(v[m_stuples_v[cur_s_sind]], cur_s_sind);
					remcount++;
				}
			}

			// **** Run BNL on filtered set

			// gets index_pairs and remainder_pairs as reference. does not resize remainder_pairs. returns remcount via reference
			pair_list res = bnl_internal_remainder_paired(index_pairs, paircount, remainder_pairs, remcount, p);

			// increment counter
			int rsize = res.size();
			if (rsize == 0) break; // no more tuples
			nres += rsize;

			// Add list to final results
			for (pair_list::iterator j = res.begin(); j != res.end(); ++j)
				if (show_levels) final_result_pair_list.push_back(std::pair<int, int>(level, j->first)); // Add level
				else final_result_list.push_back(j->first); // Add level

			// Reset counters
			paircount = 0;
			s_ind_count = 0;

			// Add all outliers from the remainder to index_pairs and all center-tuples to s_indices
			// this are all subsets, hence there is enough space
			for (int i = 0; i < remcount; i++) {
				if (remainder_pairs[i].second == -1) { //outlier => add directly to index_pairs
					index_pairs[paircount] = remainder_pairs[i];
					paircount++;
				} else { // not outlier => first run scalagon, then add to index_pairs (at begin of loop)
					s_indices[s_ind_count] = remainder_pairs[i].second;
					s_ind_count++;
				}
			}

			// clean up, make some space (index_pairs cannot be cleaned here!)
			s_indices.resize(s_ind_count); // s_indices which are still available, cannot grow anymore
			remainder_pairs.resize(ntuples - nres); // maximal possible remainder in the next step

			// Reset counter
			remcount = 0;

			if (ts.do_break(level, nres)) break;
			level++;
		}

		// Cut in the top-k case
		if (show_levels) ts.cut(final_result_pair_list, nres);
		else             ts.cut(final_result_list, nres);

	} else { // use Standard BNL
		if (show_levels) final_result_pair_list = bnl_topk_internal_levels(v, p, ts);
		else final_result_list = bnl_topk_internal(v, p, ts);
	}

	return(flex_list(final_result_list, final_result_pair_list));
}


// ------------------------------------------------------------------------------------------------------------------------------------------------------

// Helper functions for prefiltering

inline int scalagon::get_index_tuples(int &ind) {
	int res = m_stuples[0][ind];
	for (int i = 1; i < m_dim; i++) res += m_weights[i] * m_stuples[i][ind];
	return(res);
}

inline int scalagon::get_index_pt(std::vector<int> &pt) {
	int res = pt[0];
	for (int i = 1; i < m_dim; i++) res += m_weights[i] * pt[i];
	return(res);
}

// Calculate the scale vector for a given domain size and btg size
std::vector<int> scalagon::iterated_scaling(std::vector<int>& domain_size, const double btg_size) {

	std::vector<bool> reached_domain_limit;

	// Initialization
	std::vector<int> scale_fct(m_dim);
	for (int k = 0; k < m_dim; k++) {
		// Use ceil (not floor) to avoid that 1.9 is floored to 1 - Lattice calculation would become senseless!
		scale_fct[k] = (int)std::ceil(std::pow(btg_size, 1.0 / m_dim));
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
		int scale_val = (int)std::ceil(std::pow(btg_size / prod_reached, 1.0 / (m_dim - num_reached)));
		for (int k = 0; k < m_dim; k++) {
			if (!reached_domain_limit[k]) scale_fct[k] = scale_val; // increases scale factor
		}
		// after increasing scale factors, re-check if domain limit is reached
	}

	return(scale_fct);
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

bool scalagon::scalagon_init(std::vector<int>& v, pref* p, double alpha) {

	// consts for sampling
	const int lower_quantile = 19; // 2 % and 98 % quantile
	const int upper_quantile = 979;
	const double add_spread_fct = 0.2; // Add to lower/upper quantiles 


	// **** Get preferences / priliminary checks

	if (alpha <= 0) return(false); // reject alpha=-1, alpha=0
	m_prefs.clear(); // clear preference list
	// Get preferences, check if all pareto and at least two preferences
	if (!get_prefs(p) || m_prefs.size() < 2) return(false);


	// **** Precalculations for Scalagon

	int ntuples = v.size();
	if (ntuples < 10 * sample_size) return(false); // not enough tuples, DO NOT USE Scalagon, use BNL
	m_dim = m_prefs.size();
  
  // Calc samples
	if (!sample_precalc) sample_ind = get_sample(ntuples);

	// lower and upper bounds for the "center" where most tuples are expected
	std::vector<double> upper_bound(m_dim);
	std::vector<double> lower_bound(m_dim);

	// estimation of domain size
	std::vector<int> est_domain_size(m_dim);

	// Cacluclate upper/lower bound by considering the sample in each dimension
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

		// Heuristic for domain size estimation: distinct sample set is larger then 3/4 of sample size => Assume continous domain
		int dom_size = sample_set.size();
		if (dom_size > 3 * sample_size / 4) est_domain_size[k] = ntuples; // Asumme domain size is "very large"
		else {
			est_domain_size[k] = dom_size; // Small dom_size: Assume dom_size is the correct domain sitze
			if (est_domain_size[k] == 1) return(false); // If domain size is 1, DO NOT USE Scalagon - use BNL!
		}
	}

	// **** Get scaling

	double dst_btg_size = 1.0 * ntuples / alpha;

	m_scale_fct = iterated_scaling(est_domain_size, dst_btg_size);

	// ***** Run scalgon with given scaling

	m_filt_res = std::vector<int>(ntuples);
	m_filt_count = 0;

	// Calculate Downscalingfactor for "center" of tuples (lower/upper bound) 
	// (everything will be scaled to [0, ..., scale_fct-1])
	std::vector<double> fct(m_dim);
	for (int k = 0; k < m_dim; k++)
		fct[k] = 1.0 * m_scale_fct[k] / (upper_bound[k] - lower_bound[k]);

	// **** Scaling

	// Prepare vectors for scaled tuples
	m_stuples = std::vector< std::vector<int> >(m_dim); // class variable
	for (int k = 0; k < m_dim; k++) m_stuples[k] = std::vector<int>(ntuples);
	m_stuples_v = std::vector<int>(ntuples); // local variable for v-indices

	// Do the scaling
	int scount = 0;
	for (int i = 0; i < ntuples; i++) {
		m_stuples_v[scount] = i; // v-index of scaled variable (will be overwritten if scount not incremented)
		for (int k = 0;; k++) {
			int val = (int)std::floor(fct[k] * (m_prefs[k]->data[v[i]] - lower_bound[k]));
			if (val < 0 || val >= m_scale_fct[k]) {
				m_filt_res[m_filt_count] = v[i];
				m_filt_count++;
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
	for (int k = 0; k < m_dim; k++)
		m_btg_size *= m_scale_fct[k];

	// Calculate weights
	m_weights = std::vector<int>(m_dim);
	m_weights[0] = 1;
	for (int k = 1; k < m_dim; k++)
		m_weights[k] = m_scale_fct[k - 1] * m_weights[k - 1];

	// Initialization successful
	return(true);
}

// --------------------------------------------------------------------------------------------------------------------------------------------------------


/*  Scalagon domination phase with
   
 * creating the BTG
 * marking all dominated nodes with ones
*/

// mark all dominated nodes with true in m_btg
void scalagon::dominate(std::vector<int>& s_ind, pref* p) {
	
	// Create BTG and fill with zeros
	m_btg = std::vector<bool>(m_btg_size);

	// Number of scaled tuples
	int tcount = s_ind.size();
	bool use_ind = true; // is only true if s_ind is set - just for topk!
	if (tcount == 0) {
		tcount = m_stuples_v.size();
		use_ind = false;
	}

	// Helper variables for domination
	std::vector<int> start_dom(m_dim);
	std::vector<int> pt(m_dim); // temporary point
	std::vector<int> stop_dom(m_dim); // stop coordinates (may be outside of btg!)
	std::vector<int> domcount(m_dim); // Counter 
	std::vector<int> domsteps(m_dim); // Steps to dominate

	int cind;  // Counter for domination phase
	int stop0; // Stop number for dimension 0

	int cur_s_ind;

	// **** Run productorder hexagon
	for (int i = 0; i < tcount; i++) {

		// Get index
		if (use_ind) cur_s_ind = s_ind[i]; // use index (topk)
		else cur_s_ind = i; // dont use index (no top k)

		// ** Read tuple and preliminary checks
		int ind = get_index_tuples(cur_s_ind); // get from stuples [0,...,scount-1]
		if (m_btg[ind]) continue; // already dominated, skip tuple

		// ** Search for start-domination point (left upper corner)
		int start_dom_ind = ind;
		for (int k = 0; k < m_dim; k++) {
			start_dom[k] = m_stuples[k][cur_s_ind] + 1;
			start_dom_ind += m_weights[k];
			if (start_dom[k] == m_scale_fct[k]) goto next_tuple; // outside btg - nothing to dominate!
		}

		if (m_btg[start_dom_ind]) continue; // Domination area already dominated, skip tuple

		// ** Search for stop-domination point (right lower corner)
		for (int k = 0; k < m_dim; k++) pt[k] = start_dom[k];
		for (int k = 0; k < m_dim; k++) {
			// Set stops for stop domination search (optimization trick for for-loop)
			pt[k] = m_scale_fct[k] - 1;
			int tind = get_index_pt(pt);
			pt[k] = start_dom[k];
			if (m_btg[tind] == false) stop_dom[k] = m_scale_fct[k]; // Stop at border (point is outside btg!)
			else {
				// Calculate the stop point (stop before border!)
				stop_dom[k] = start_dom[k];
				for (int l = start_dom_ind;; l += m_weights[k]) {
					if (m_btg[l]) break;
					stop_dom[k]++;
				}
			}

			// domination steps to do
			domsteps[k] = stop_dom[k] - start_dom[k];
			if (domsteps[k] == 0) goto next_tuple; // empty rectangle, skip tuple

			// set counter back to 0
			domcount[k] = 0;
		}

		// ** Do the domination
		cind = get_index_pt(pt);
		stop0 = cind + domsteps[0];

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
						if (k == m_dim - 1) goto next_tuple; // Done with domination, next tuple
						// Prepare for next dimension
						domcount[k] = 0;
						cind -= domsteps[k] * m_weights[k];
					} else break;
				}
				stop0 = cind + domsteps[0];
			}
		}
		// Jump position for leaving the outer while-loop within the the inner for-loop
		next_tuple:;
	}
}
