

// Include BNL as it is needed for the final steps AND iterations steps
// this also includes "pref-classes.h"

// includes also pref-classes
#include "bnl.h"


std::vector<int> get_sample(int ntuples);


// --------------------------------------------------------------------------------------------------------------------------------
// From here from VS
// --------------------------------------------------------------------------------------------------------------------------------

// ------------------------------------------------------------------------------------------------------------------------------------------------------

class scalagon {
public:
	scalagon(bool sample_precalc = false);
	~scalagon();

	// run Scalagon prefiltering together with BNL
	std::list<int> run_scalagon(std::vector<int>& v, pref* p, double alpha = 1);
	
	// with and without top-k
	flex_list run_scalagon_topk(std::vector<int>& v, pref* p, topk_setting& ts, double alpha, bool show_levels);
	
  // consts for sampling
	static const int sample_size = 1000; // public and static to access it before class is constructed
  static const int scalagon_min_tuples = 10000;
  
  // sample of random numbers (to be calculated outside from the worker thread)
  std::vector<int> sample_ind;
  
private:

  // set by the constructor: true if sample indices will be precalculated and assigned to the public sample_ind
  // this is important if this class is used in a parallel worker thread, where the random generator from the R API may not be called!
  bool sample_precalc;

	int m_dim; // Number of dimensions

	// All pareto / product order preferences
	std::vector<scorepref*> m_prefs;

	// convert preferences from tree into vector
	bool get_prefs(pref* p);

	// filtered result
	std::vector<int> m_filt_res; 
	int m_filt_count;

	// weights for product order domination phase
	std::vector<int> m_weights;

	// scaled tuples, filtered to "center"
	std::vector<int> m_stuples_v;
	std::vector< std::vector<int> > m_stuples;

	// calculate index (according to weights) of tuple
	int get_index_pt(std::vector<int>& pt);
	int get_index_tuples(int& ind);

	std::vector<int> iterated_scaling(std::vector<int>& domain_size, const double btg_size);
	std::vector<int> m_scale_fct;
	std::vector<bool> m_btg;
	int m_btg_size;
	
	//Init Scalagon, returns TRUE if successful, FALSE if not (preference not solely pareto, or domain not suited!)
	bool scalagon_init(std::vector<int>& v, pref* p, double alpha);

	// Domination phase, while scaling is fixed
	void dominate(std::vector<int>& s_ind, pref* p);

};
