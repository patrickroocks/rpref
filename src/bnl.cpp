#include "bnl.h"

std::vector<int> bnl::run(const std::vector<int>& indices, const ppref& p)
{
  const int ntuples = indices.size();
  if (ntuples == 0) return std::vector<int>();
  
  std::vector<int> window;
  std::vector<int> window_next;
  
  window.reserve(ntuples);
  window_next.reserve(ntuples);
  
  for (int u : indices) {
    
    bool dominated = false;
    for (int v : window) {
      if (p->cmp(v, u)) { // v (window element) is better
        dominated = true;
        break;
      } else if (!p->cmp(u, v)) { // u (picked element) is NOT better
        window_next.push_back(v);
      }
    }
    if (!dominated) {
      std::swap(window, window_next);
      window.push_back(u);
    }
    window_next.clear();
  }
  
  return window;
}


// --------------------------------------------------------------------------------------------------------------------------------

// Standard BNL with remainder, for top(level) k calculation WITHOUT using Scalagon
std::vector<int> bnl::run_remainder(const std::vector<int>& vec, std::vector<int>& remainder, const ppref& p)
{
  const int ntuples = vec.size();
  if (ntuples == 0) return std::vector<int>();
  
  std::vector<int> window;
  std::vector<int> window_next;
  window.reserve(ntuples);
  window_next.reserve(ntuples);
  
  for (int u : vec) {
    bool dominated = false;
    for (int v : window) {
      if (p->cmp(v, u)) { // v (window element) is better
        dominated = true;
        break;
      } else if (p->cmp(u, v)) { // u (picked element) is better
        remainder.push_back(v);
      } else {
        window_next.push_back(v);
      }
    }
    if (!dominated) {
      std::swap(window, window_next);
      window.push_back(u);
    } else {
      remainder.push_back(u);
    }
    window_next.clear();
  }
  
  return window;
}


// Helper function: Add levels to result
pair_vector bnl::add_level(const std::vector<int>& vec, int level)
{
  pair_vector res;
  res.reserve(vec.size());
  for (int u : vec) res.push_back(std::pair<int, int>(level, u));
  return res;
}

// --------------------------------------------------------------------------------------------------------------------------------


// Internal top-k BNL (v is NOT a reference, will be edited!) returning NO LEVELS
// special cases (level=1, no topk) are handled by scalagon!
std::vector<int> bnl::run_topk(std::vector<int> v, const ppref& p, const topk_setting& ts)
{
  const int ntuples = v.size();
  int nres = 0;
  
  std::vector<int> final_result;
  std::vector<int> remainder;
  final_result.reserve(ntuples);
  remainder.reserve(ntuples);
  
  int level = 1;
  while (true) {
    std::vector<int> res = run_remainder(v, remainder, p);
    const int rsize = res.size();
    if (rsize == 0) break; // no more tuples
    nres += rsize;
    final_result += res;
    std::swap(v, remainder);
    remainder.clear();
    if (ts.do_break(level, nres)) break;
    level++;
  }
  
  ts.cut(final_result);
  return final_result;
}

// Internal top-k BNL (v is NOT a reference, will be edited!) returning levels
// special cases (level=1, no topk) are handled before!
pair_vector bnl::run_topk_lev(std::vector<int> vec, const ppref& p, const topk_setting& ts)
{
  const int ntuples = vec.size();
  
  std::vector<int> remainder;
  pair_vector final_result;
  
  final_result.reserve(ntuples);
  remainder.reserve(ntuples);
  
  int level = 1;
  while (true) {
    pair_vector res = add_level(run_remainder(vec, remainder, p), level);
    if (res.empty()) break; // no more tuples
    final_result += res;
    std::swap(vec, remainder);
    remainder.clear();
    if (ts.do_break(level, final_result.size())) break;
    level++;
  }
  
  ts.cut(final_result);
  return final_result;
}


// --------------------------------------------------------------------------------------------------------------------------------

// BNL for top-k calculation with remainder and additional index std::vector for scalagon
// add remainder beginnung at remcount
pair_vector bnl::run_remainder_paired(const pair_vector& index_pairs, pair_vector& remainder_pairs, const ppref& p)
{
  const int ntuples = index_pairs.size();
  if (ntuples == 0) return pair_vector();
  
  pair_vector window;
  pair_vector window_next;
  window.reserve(ntuples);
  window_next.reserve(ntuples);
  
  for (const std::pair<int,int>& u : index_pairs) {
    
    bool dominated = false;
    for (const std::pair<int,int>& v : window) {
      if (p->cmp(v.first, u.first)) { // v (window element) is better
        dominated = true;
        break;
      } else if (p->cmp(u.first, v.first)) { // u (picked element) is better
        remainder_pairs.push_back(v);
      } else {
        window_next.push_back(v);
      }
    }
    if (!dominated) {
      std::swap(window, window_next);
      window.push_back(u);
    } else {
      remainder_pairs.push_back(u);
    }
    window_next.clear();
  }

  return window;
}
