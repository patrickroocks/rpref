

#include "bnl.h"


// --------------------------------------------------------------------------------------------------------------------------------
// From here from VS
// --------------------------------------------------------------------------------------------------------------------------------


bnl::bnl()
{
}


bnl::~bnl()
{
}


list<int> bnl::run(vector<int>& indices, ppref& p) {
  
  bool dominated;
  int ntuples = indices.size();
  
  if (ntuples == 0) return list<int>();
  
  list<int> window;
  list<int>::iterator j;
  
  window.push_back(indices[0]);
  
  for (int i = 1; i<ntuples; ++i) {
    
    dominated = false;
    for (j = window.begin(); j != window.end(); ) {
      if (p->cmp(*j, indices[i])) { // *j (window element) is better
        dominated = true;
        break;
      }
      else if (p->cmp(indices[i], *j)) { // indices[i] (picked element) is better
        // delete j
        j = window.erase(j);
        if (j == window.end()) break;
        else continue;
      }
      ++j;
    }
    if (!dominated) {
      window.push_back(indices[i]);
    }
  }
  
  // return Window as a numeric list
  return window;
  
}


// --------------------------------------------------------------------------------------------------------------------------------

// Standard BNL with remainder, for top(level) k calculation WITHOUT using Scalagon
list<int> bnl::run_remainder(vector<int>& v, vector<int>& remainder, ppref& p) {
  
  bool dominated;
  int count = 0;
  int ntuples = v.size();
  
  if (ntuples == 0) return list<int>();
  
  list<int> window;
  list<int>::iterator j;
  
  window.push_back(v[0]);
  
  for (int i = 1; i<ntuples; ++i) {
    
    dominated = false;
    for (j = window.begin(); j != window.end();) {
      if (p->cmp(*j, v[i])) { // *j (window element) is better
        dominated = true;
        break;
      } else if (p->cmp(v[i], *j)) { // v[i] (picked element) is better
        remainder[count] = *j;
        count++;
        
        // delete j
        j = window.erase(j);
        if (j == window.end()) break;
        else continue;
      }
      ++j;
    }
    if (!dominated) {
      window.push_back(v[i]);
    } else {
      remainder[count] = v[i];
      count++;
    }
  }
  remainder.resize(count);
  
  // return Window as a numeric list
  return window;
}


// Helper function: Add levels to result
pair_list bnl::add_level(const list<int>& lst, const int level) {
  pair_list res;
  for (list<int>::const_iterator j = lst.begin(); j != lst.end(); ++j)
    res.push_back(pair<int, int>(level, *j));
  return(res);
}

// --------------------------------------------------------------------------------------------------------------------------------




// Internal top-k BNL (v is NOT a reference, will be edited!) returning NO LEVELS
// special cases (level=1, no topk) are handled by scalagon!
list<int> bnl::run_topk(vector<int> v, ppref& p, topk_setting& ts) {
  
  list<int> final_result;
  
  int ntuples = v.size();
  int nres = 0;
  
  vector<int> remainder(ntuples);
  
  int level = 1;
  while (true) {
    list<int> res = run_remainder(v, remainder, p);
    int rsize = res.size();
    if (rsize == 0) break; // no more tuples
    nres += rsize;
    final_result.splice(final_result.end(), res);
    swap(v, remainder);
    if (ts.do_break(level, nres)) break;
    level++;
  }
  
  ts.cut(final_result, nres);
  
  return final_result;
}

// Internal top-k BNL (v is NOT a reference, will be edited!) returning levels
// special cases (level=1, no topk) are handled before!
pair_list bnl::run_topk_lev(vector<int> v, ppref& p, topk_setting& ts) {
  
  pair_list final_result;
  
  int ntuples = v.size();
  int nres = 0;
  
  vector<int> remainder(ntuples);
  
  int level = 1;
  while (true) {
    pair_list res = add_level(run_remainder(v, remainder, p), level);
    int rsize = res.size();
    if (rsize == 0) break; // no more tuples
    nres += rsize;
    final_result.splice(final_result.end(), res);
    swap(v, remainder);
    if (ts.do_break(level, nres)) break;
    level++;
  }
  
  ts.cut(final_result, nres);
  
  return final_result;
}


// --------------------------------------------------------------------------------------------------------------------------------

// BNL for top-k calculation with remainder and additional index vector for scalagon
// add remainder beginnung at remcount
pair_list bnl::run_remainder_paired(pair_vector& index_pairs, int paircount, 
                                    pair_vector& remainder_pairs, int& remcount, ppref& p) {
  
  if (paircount == 0) return pair_list();
  
  bool dominated;
  pair_list window;
  pair_list::iterator j;
  
  window.push_back(index_pairs[0]);
  
  for (int i = 1; i < paircount; ++i) {
    
    dominated = false;
    for (j = window.begin(); j != window.end();) {
      if (p->cmp(j->first, index_pairs[i].first)) { // *j (window element) is better
        dominated = true;
        break;
      } else if (p->cmp(index_pairs[i].first, j->first)) { // v[i] (picked element) is better
        remainder_pairs[remcount] = *j;
        remcount++;
        
        // delete j
        j = window.erase(j);
        if (j == window.end()) break;
        else continue;
      }
      ++j;
    }
    if (!dominated) {
      window.push_back(index_pairs[i]);
    } else {
      remainder_pairs[remcount] = index_pairs[i];
      remcount++;
    }
  }
  // do not resize reminder! 
  // return remcount via reference
  // return Window as a numeric list
  return window;
}
