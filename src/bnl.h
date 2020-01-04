#pragma once

#include "topk-setting.h"
#include "pref-classes.h"

// ----------------------------------------------------------------------------------------------------------------------------------------

template <typename T>
std::vector<T>& operator+=(std::vector<T>& vector1, const std::vector<T>& vector2)
{
  vector1.insert(vector1.end(), vector2.begin(), vector2.end());
  return vector1;
}

// ----------------------------------------------------------------------------------------------------------------------------------------

// Vector of pairs (for <level, v-index> or for <v-index, s-index>)
typedef std::vector< std::pair<int, int> > pair_vector;

// List containing v-indices OR v-indices together with levels
typedef std::pair< std::vector<int>, pair_vector > flex_vector;


// ----------------------------------------------------------------------------------------------------------------------------------------

// Block Nested Loop (BNL) algorithm for comparison based preference selection
// plus some variants for Scalagon

class bnl
{
public:
  static std::vector<int> run(const std::vector<int>& indices, const ppref& p);
  
  // BNL top(level) k without levels
  static std::vector<int> run_topk(std::vector<int> v, const ppref& p, const topk_setting& ts);
  
  // BNL top(level) k with levels (do not use flexlist here, code is quite small!)
  static pair_vector run_topk_lev(std::vector<int> v, const ppref& p, const topk_setting& ts);
  
  // Helper function: Add levels to result
  static pair_vector add_level(const std::vector<int>& lst, int level);
  
  // internal BNL variant for BNL top-k
  static std::vector<int> run_remainder(const std::vector<int>& v, std::vector<int>& remainder, const ppref& p);
  
  // ** BNL for Scalagon
  
  // Remainder (non-optimal optimal tuples) is added to remainder_pairs,
  // for each added tuple, remcount is incremented
  
  // special top-k BNL variant for Scalagon filtering step
  static pair_vector run_remainder_paired(const pair_vector& index_pairs, pair_vector& remainder_pairs, const ppref& p);

};

