
#include "topk_setting.h"

#include "pref-classes.h"


// --------------------------------------------------------------------------------------------------------------------------------
// From here from VS
// --------------------------------------------------------------------------------------------------------------------------------


// ----------------------------------------------------------------------------------------------------------------------------------------

// List of pairs (for <level, v-index> or for <v-index, s-index>
typedef list< pair<int, int> > pair_list;

// Vector of pairs, same usage as pair_list
typedef vector< pair<int, int> > pair_vector;

// List containing v-indices OR v-indices together with levels
typedef pair< list<int>, pair_list > flex_list;


// ----------------------------------------------------------------------------------------------------------------------------------------

// Block Nested Loop (BNL) algorithm for comparison based preference selection
// plus some variants for Scalagon

class bnl
{
public:
  bnl();
  ~bnl();
  
  // Standard-BNL
  list<int> run(vector<int>& indices, ppref& p);
  
  // BNL top(level) k without levels
  list<int> run_topk(vector<int> v, ppref& p, topk_setting& ts);
  
  // BNL top(level) k with levels (do not use flexlist here, code is quite small!)
  pair_list run_topk_lev(vector<int> v, ppref& p, topk_setting& ts);
  
  // Helper function: Add levels to result
  pair_list add_level(const list<int>& lst, const int level);
  
  // internal BNL variant for BNL top-k
  list<int> run_remainder(vector<int>& v, vector<int>& remainder, ppref& p);
  
  // ** BNL for Scalagon
  
  // Remainder (non-optimal optimal tuples) is added to remainder_pairs,
  // for each added tuple, remcount is incremented
  
  // special top-k BNL variant for Scalagon filtering step
  pair_list run_remainder_paired(pair_vector& index_pairs, int paircount, 
                                 pair_vector& remainder_pairs, int& remcount, ppref& p);
  

};

