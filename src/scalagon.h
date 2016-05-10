

// Include BNL as it is needed for the final steps AND iterations steps
// this also includes "pref-classes.h"

// includes also pref-classes
#include "bnl.h"


// --------------------------------------------------------------------------------------------------------------------------------
// From here from VS
// --------------------------------------------------------------------------------------------------------------------------------



// outside of Scalagon class because C++ random generator is not allowed in R
vector<int> get_sample(int ntuples);


class scalagon {
public:
  
  // set sample_precalc = true if NO random generator should be called from Scalagon
  // use the sample_ind vector instead
  scalagon(bool sample_precalc = false);
  ~scalagon();
  
  // sample of random numbers 
  // (to be calculated outside from the worker thread when Scalagon is used in parallel computation)
  vector<int> sample_ind;
  
  // run Scalagon prefiltering together with BNL
  list<int> run(vector<int>& v, ppref& p, double alpha = 10);
  
  // Scalagon with and without top-k
  // flex_list is either a index list or a <index,level> list
  flex_list run_topk(vector<int>& v, ppref& p, topk_setting& ts, double alpha, bool show_levels);
  
  // consts for sampling
  static const int sample_size = 1000; // public and static to access it before class is constructed
  static const int scalagon_min_tuples = 10000;
  
  
private:
  
  bnl bnl_alg;
  
  // set by the constructor: true if sample indices will be precalculated and assigned to the public sample_ind
  // this is important if this class is used in a parallel worker thread, where the random generator from the R API may not be called!
  bool sample_precalc;
  
  int m_dim; // Number of dimensions
  
  // All pareto / product order preferences
  vector< shared_ptr<scorepref> > m_prefs;
  
  // convert preferences from tree into vector
  bool get_prefs(ppref& p);
  
  // filtered result
  vector<int> m_filt_res; 
  int m_filt_count;
  
  // weights for product order domination phase
  vector<int> m_weights;
  
  // scaled tuples, filtered to "center"
  vector<int> m_stuples_v;
  vector< vector<int> > m_stuples;
  
  // calculate index (according to weights) of tuple
  int get_index_pt(vector<int>& pt);
  int get_index_tuples(int& ind);
  
  vector<int> iterated_scaling(vector<int>& domain_size, const double btg_size);
  vector<int> m_scale_fct;
  vector<bool> m_btg;	
  int m_btg_size;
  
  
  // init Scalagon, returns TRUE if successful, FALSE if not
  // (preference not solely pareto, or domain not suited!)
  bool init(vector<int>& v, ppref& p, double alpha);
  
  // Domination phase, while scaling is fixed
  void dominate(vector<int>& s_ind, int scount, ppref & p);
  
  
};

