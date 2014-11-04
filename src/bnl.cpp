#include <Rcpp.h>
using namespace Rcpp;

#include "bnl.h"


// --------------------------------------------------------------------------------------------------------------------------------
// From here from VS
// --------------------------------------------------------------------------------------------------------------------------------


// --------------------------------------------------------------------------------------------------------------------------------

// the best level has level == 1 !

bool topk_setting::do_cut() {
  // cut if topk is set and {we have and AND-connection OR if topk is the only value}
	return(topk != -1 && (and_connected || (toplevel == -1 && at_least == -1)));
}

bool topk_setting::do_break(int level, int ntuples) { // gets level of this iteration!
	if (and_connected) {
		// Take intersection, break if one limit is reached
		if ((topk     != -1 && ntuples >= topk)     || 
			(at_least != -1 && ntuples >= at_least) || 
			                   level   == toplevel) return(true);
	} else {
		// Take union, break if all limits are reached (or limits are not set)
		if ((topk     == -1 || ntuples >= topk)      && 
			(at_least == -1 || ntuples >= at_least)  && 
			(toplevel == -1 || level   == toplevel)) return(true);
	}
	return(false);
}

// --------------------------------------------------------------------------------------------------------------------------------


// Standard-BNL
std::list<int> bnl_internal(std::vector<int>& v, pref* p) {

	bool dominated;
	int ntuples = v.size();

	if (ntuples == 0) return std::list<int>();

	std::list<int> window;
	std::list<int>::iterator j;

	window.push_back(v[0]);

	for (int i = 1; i<ntuples; ++i) {

		dominated = false;
		for (j = window.begin(); j != window.end(); ) {
			if (p->cmp(*j, v[i])) { // *j (window element) is better
				dominated = true;
				break;
			} else if (p->cmp(v[i], *j)) { // v[i] (picked element) is better
				// delete j
				j = window.erase(j);
				if (j == window.end()) break;
				else continue;
			}
			++j;
		}
		if (!dominated) {
			window.push_back(v[i]);
		}
	}

	// return Window as a numeric list
	return window;

}

// --------------------------------------------------------------------------------------------------------------------------------

// Standard BNL with remainder, for top(level) k calculation WITHOUT using Scalagon
std::list<int> bnl_internal_remainder(std::vector<int>& v, std::vector<int>& remainder, pref* p) {

	bool dominated;
	int count = 0;
	int ntuples = v.size();

	if (ntuples == 0) return std::list<int>();

	std::list<int> window;
	std::list<int>::iterator j;

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
pair_list add_level(const std::list<int>& lst, const int level) {
	pair_list res;
	for (std::list<int>::const_iterator j = lst.begin(); j != lst.end(); ++j)
		res.push_back(std::pair<int, int>(level, *j));
	return(res);
}

// --------------------------------------------------------------------------------------------------------------------------------


// Internal top-k BNL (v is NOT reference, will be edited!) returning NO LEVELS
// special cases (level=1, no topk) are handled by scalagon!
std::list<int> bnl_topk_internal(std::vector<int> v, pref* p, topk_setting& ts) {

	std::list<int> final_result;

	int ntuples = v.size();
	int nres = 0;

	std::vector<int> remainder(ntuples);

	int level = 1;
	while (true) {
		std::list<int> res = bnl_internal_remainder(v, remainder, p);
		int rsize = res.size();
		if (rsize == 0) break; // no more tuples
		nres += rsize;
		final_result.splice(final_result.end(), res);
		std::swap(v, remainder);
		if (ts.do_break(level, nres)) break;
		level++;
	}

	if (ts.do_cut()) final_result.resize(ts.topk); // Cut 
	return final_result;
}

// Internal top-k BNL (v is NOT reference, will be edited!) returning levels
// special cases (level=1, no topk) are handled by scalagon!
pair_list bnl_topk_internal_levels(std::vector<int> v, pref* p, topk_setting& ts) {

	pair_list final_result;

	int ntuples = v.size();
	int nres = 0;

	std::vector<int> remainder(ntuples);
	
	int level = 1;
	while (true) {
		pair_list res = add_level(bnl_internal_remainder(v, remainder, p), level);
		int rsize = res.size();
		if (rsize == 0) break; // no more tuples
		nres += rsize;
		final_result.splice(final_result.end(), res);
		std::swap(v, remainder);
		if (ts.do_break(level, nres)) break;
		level++;
	}

	if (ts.do_cut()) final_result.resize(ts.topk); // Cut 
	return final_result;
}


// --------------------------------------------------------------------------------------------------------------------------------

// BNL for top-k calculation with remainder and additional index vector for scalagon
pair_list bnl_internal_remainder_paired(pair_vector& index_pairs, int paircount, pair_vector& remainder_pairs, int& remcount, pref* p) {

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
	// do not resize reminder! return remcount via reference
	// return Window as a numeric list
	return window;
}
