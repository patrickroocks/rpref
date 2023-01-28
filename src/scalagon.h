#pragma once

// Include BNL as it is needed for the final steps AND iterations steps
// this also includes "pref-classes.h"

// includes also pref-classes
#include "bnl.h"

// outside of Scalagon class because C++ random generator is not allowed in R
std::vector<int> get_sample(int ntuples);

class scalagon
{
public:
  
  // set sample_precalc = true if NO random generator should be called from Scalagon
  // use the sample_ind vector instead
  scalagon(bool sample_precalc = false);
  
  // sample of random numbers 
  // (to be calculated outside from the worker thread when Scalagon is used in parallel computation)
  std::vector<int> sample_ind;
  
  // run Scalagon prefiltering together with BNL
  std::vector<int> run(const std::vector<int>& v, const ppref& p, double alpha = 10);
  
  // Scalagon with and without top-k
  flex_vector run_topk(const std::vector<int>& v, const ppref& p, const topk_setting& ts, double alpha, bool show_levels);
  
  // consts for sampling
  static const int sample_size = 1000; // public and static to access it before class is constructed
  static const int scalagon_min_tuples = 10000;
  
private:
  
  bnl bnl_alg;
  
  // set by the constructor: true if sample indices will be precalculated and assigned to the public sample_ind
  // this is important if this class is used in a parallel worker thread, where the random generator from the R API may not be called!
  const bool sample_precalc;
  
  int m_dim = 0; // Number of dimensions
  
  // All pareto / product order preferences
  std::vector<std::shared_ptr<scorepref>> m_prefs;
  
  // convert preferences from tree into vector
  bool get_prefs(const ppref& p);
  
  // filtered result
  std::vector<int> m_filt_res; 
  
  // weights for product order domination phase
  std::vector<int> m_weights;
  
  // scaled tuples, filtered to "center"
  std::vector<int> m_stuples_v;
  std::vector<std::vector<int>> m_stuples;
  
  // calculate index (according to weights) of tuple
  int get_index_pt(const std::vector<int>& pt);
  int get_index_tuples(int ind);
  
  std::vector<int> iterated_scaling(const std::vector<int>& domain_size, double btg_size);
  std::vector<int> m_scale_fct;
  std::vector<bool> m_btg;
  int m_btg_size;
  
  // init Scalagon, returns TRUE if successful, FALSE if not
  // (preference not solely pareto, or domain not suited!)
  bool init(const std::vector<int>& v, const ppref& p, double alpha);
  
  // Domination phase, while scaling is fixed
  void dominate(const std::vector<int>& s_ind, const ppref& p);
  
};
