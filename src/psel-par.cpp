// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
using namespace RcppParallel;

#include "scalagon.h" // Includes BNL, pref classes and Scalagon

using namespace Rcpp;

// Scalagon/BNL Wrapper for parallel and non-parallel. 

// for non-grouping / grouping and non-topk
// Adding functionality for top-k does not make sense - top-k returns a pairlist!
class Psel_worker : public Worker {
public:
  // input
  const std::vector<std::vector<int>>& vs;
  ppref p;
  double alpha;
  
  std::vector<std::vector<int>> results;
  std::vector<std::vector<int>> samples_ind;
  
  // initialize from Rcpp input and output matrixes (the RMatrix class
  // can be automatically converted to from the Rcpp matrix type)
  Psel_worker(std::vector<std::vector<int>>& vs, ppref p, int N, double alpha, std::vector<std::vector<int>>& samples_ind) : 
    vs(vs), p(p), alpha(alpha), results(N), samples_ind(samples_ind) {}
   
   // function call operator that work for the specified range (begin/end)
  void operator()(std::size_t begin, std::size_t end)
  {
    for (std::size_t k = begin; k < end; k++) {
      scalagon scal_alg(true);
      scal_alg.sample_ind = samples_ind[k];
      results[k] = scal_alg.run(vs[k], p, alpha);
    }
  }
};

// --------------------------------------------------------------------------------------------------------------------------------


// Parallel preference selection (without top-k)
// subdivide dataset in N parts

// [[Rcpp::export]]
NumericVector pref_select_impl(const DataFrame& scores, const List& serial_pref, int N, double alpha)
{
  NumericVector col1 = scores[0];
  const int ntuples = col1.size();
  if (ntuples == 0) return NumericVector();
  std::vector<int> res;
  res.reserve(ntuples);
  
  // De-Serialize preference
  const ppref p = CreatePreference(serial_pref, scores);
  
  // Scalagon instance for non-parallel run or final run in parallel case
  scalagon scal_alg;
  
  // Execute algorithm for non-parallel case
  if (N == 1) {
    
    // Create index vector
    std::vector<int> v(ntuples);
    for (int i = 0; i < ntuples; i++) v[i] = i;
    
    res = scal_alg.run(v, p, alpha);
  
  } else { // N > 1, parallel case
  
    // Tuples per partition
    int tuples_part = std::ceil(1.0 * ntuples / N);
    
    // Actual number of partitions (N_parts < N for very small numbers of ntuples like ntuples = 5)
    int N_parts = std::ceil(1.0 * ntuples / tuples_part);
  
    // Create N_parts index vectors (for parallelization)
    std::vector<std::vector<int>> vs(N_parts);
    std::vector<std::vector<int>> samples_ind(N_parts);
  
    int count = 0;
    for (int k = 0; k < N_parts; k++) {
      int local_n;
      if (k == N_parts - 1) local_n = ntuples - count;
      else                  local_n = tuples_part;
      
      samples_ind[k] = get_sample(local_n);
      vs[k] = std::vector<int>(local_n);
      for (int i = 0; i < local_n; i++) {
        vs[k][i] = count;
        count++;
      }
    }
    
    // Create worker and execute parallel
    Psel_worker worker(vs, p, N_parts, alpha, samples_ind);
    parallelFor(0, N_parts, worker);
    
    // Clue together
    for (int k = 0; k < N_parts; k++) res += worker.results[k];
    
    // execute (top k) BNL again
    res = scal_alg.run(res, p, alpha);
  }
  
  // Return result
  return NumericVector(res.begin(), res.end());
}


// --------------------------------------------------------------------------------------------------------------------------------

// Parallel grouped preference selection

// Grouped preference evaluation, based on Groups from dplyr
// We assume that the attribute "indices" of a grouped data frame stores the grouping information!

// [[Rcpp::export]]
NumericVector grouped_pref_sel_impl(const List& indices, const DataFrame& scores, const List& serial_pref, int N, double alpha) {
  
  const int nind = indices.length();
  std::vector<int> res;
  res.reserve(nind);
  
  if (nind == 0) return NumericVector();
  
  const ppref p = CreatePreference(serial_pref, scores);

  if (N > 1) { // parallel case
  
    // Compose indices
    std::vector<std::vector<int>> vs(nind);
    std::vector<std::vector<int>> samples_ind(nind);
    for (int i = 0; i < nind; i++) {
      vs[i] = as<std::vector<int>>(indices[i]);
      samples_ind[i] = get_sample(vs[i].size()); // Sample indices for this partition
    }
  
    // Create worker
    Psel_worker worker(vs, p, nind, alpha, samples_ind); 
    
    // Execute parallel
    parallelFor(0, nind, worker);
    
    // Clue together
    for (int i = 0; i < nind; i++) res += worker.results[i];
    
  } else { // non parallel case
  
    scalagon scal_alg;
  
    for (int i = 0; i < nind; i++) {
      std::vector<int> group_indices = as<std::vector<int>>(indices[i]);
      res += scal_alg.run(group_indices, p, alpha);
    }
  }
  
  return NumericVector(res.begin(), res.end());
}
