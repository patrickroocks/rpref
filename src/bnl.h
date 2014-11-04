
#include "pref-classes.h"



// --------------------------------------------------------------------------------------------------------------------------------
// From here from VS
// --------------------------------------------------------------------------------------------------------------------------------


// ----------------------------------------------------------------------------------------------------------------------------------------

// List of pairs (for <level, v-index> or for <v-index, s-index>
typedef std::list< std::pair<int, int> > pair_list;

// Vector of pairs, same usage as pair_list
typedef std::vector< std::pair<int, int> > pair_vector;

// List containing v-indices OR v-indices together with levels
typedef std::pair< std::list<int>, pair_list > flex_list;


// ----------------------------------------------------------------------------------------------------------------------------------------


// TOP-K Settings, C-internal (not visible to R)
// the best level has level == 1 !
class topk_setting {
public:
  int topk;
	int at_least;
	int toplevel;
	bool and_connected;
	bool is_simple; // top-level == 1 and no other things set (i.e. non-top-k evaluation)

	topk_setting(int topk = -1, int at_least = -1, int toplevel = -1, bool and_connected = true) : 
		topk(topk), at_least(at_least), toplevel(toplevel), and_connected(and_connected),
		is_simple(topk == -1 && at_least == -1 && toplevel == 1) {}

	// for top-k algorithms
	bool do_break(int level, int ntuples);
	bool do_cut();
};


// ----------------------------------------------------------------------------------------------------------------------------------------

// Standard -BNL
std::list<int> bnl_internal(std::vector<int>&, pref*);

// BNL top(level) k without levels
std::list<int> bnl_topk_internal(std::vector<int> v, pref* p, topk_setting& ts);

// BNL top(level) k with levels (do not use flexlist here, code is quite small!)
pair_list bnl_topk_internal_levels(std::vector<int> v, pref* p, topk_setting& ts);

// Helper function: Add levels to result
pair_list add_level(const std::list<int>& lst, const int level);

// ----------------------------------------------------------------------------------------------------------------------------------------

// internal BNL variant BNL top-k
std::list<int> bnl_internal_remainder(std::vector<int>& v, std::vector<int>& remainder, pref* p);

// special top-k BNL variant for Scalagon filtering step
pair_list bnl_internal_remainder_paired(pair_vector& index_pairs, int paircount, pair_vector& remainder_pairs, int& remcount, pref* p);
