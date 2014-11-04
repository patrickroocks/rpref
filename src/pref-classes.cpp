#include <Rcpp.h>
using namespace Rcpp;

#include "pref-classes.h"

// --------------------------------------------------------------------------------------------------------------------------------
// BEGIN VS block
// --------------------------------------------------------------------------------------------------------------------------------

// Methods of preferences classes and de-serialization of preferences

complexpref::~complexpref() {
  if (p1 != 0) delete p1;
	if (p2 != 0) delete p2;
}

reversepref::~reversepref() {
	if (p != 0) delete p;
}

bool scorepref::cmp(int i, int j) {
  return(data[i] < data[j]);
}

bool scorepref::eq(int i, int j) {
	return(data[i] == data[j]);
}

bool reversepref::cmp(int i, int j) {
	return(p->cmp(j, i));
}

bool reversepref::eq(int i, int j) {
	return(p->eq(i, j));
}

bool complexpref::eq(int i, int j) {
	return(p1->eq(i, j) && p2->eq(i, j));
}

bool prior::cmp(int i, int j) {
	return(p1->cmp(i, j) || (p1->eq(i, j) && p2->cmp(i, j)));
}

bool pareto::cmp(int i, int j) {
	return((p1->cmp(i, j) && (p2->cmp(i, j) || p2->eq(i, j))) ||
		(p2->cmp(i, j) && (p1->cmp(i, j) || p1->eq(i, j))));
}

bool intersectionpref::cmp(int i, int j) {
	return(p1->cmp(i, j) && p2->cmp(i, j));
}

bool unionpref::cmp(int i, int j) {
	return(p1->cmp(i, j) || p2->cmp(i, j));
}

// --------------------------------------------------------------------------------------------------------------------------------
// END VS block
// --------------------------------------------------------------------------------------------------------------------------------


// Only internal: Recursively create preference
std::pair<pref*, int> DoCreatePreference(const List& pref_lst, const DataFrame& scores, int next_id) {
    
  char pref_kind = as<char>(pref_lst["kind"]);
  std::pair<pref*, int> pair_res;
  
  if (pref_kind == '*' || pref_kind == '&' || pref_kind == '|' || pref_kind == '+') {
    
    // Binary complex preference
    // Note that a unique_ptr does not allow upcasts, hence we use standard pointers!
    complexpref* res = NULL;
    
    switch(pref_kind) {
      case '*': res = new pareto();           break;
      case '&': res = new prior();            break;
      case '|': res = new intersectionpref(); break;
      case '+': res = new unionpref();        break;
    }

    pair_res = DoCreatePreference(as<List>(pref_lst["p1"]), scores, next_id);
    res->p1 = pair_res.first;
    pair_res = DoCreatePreference(as<List>(pref_lst["p2"]), scores, pair_res.second);
    res->p2 = pair_res.first;
    return(std::pair<pref*, int>(res, pair_res.second));
    
  } else if (pref_kind == '-') {
    
    reversepref* res = new reversepref();
    pair_res = DoCreatePreference(as<List>(pref_lst["p"]), scores, next_id);
    res->p = pair_res.first;
    return(std::pair<pref*, int>(res, pair_res.second));
    
  } else if (pref_kind == 's') {
    
    // Score (base) preference, scorevector is const!
    scorepref* res = new scorepref(as<NumericVector>(scores[next_id]));
    next_id++;
    return(std::pair<pref*, int>(res, next_id));
    
  } 
  
  stop("Error during de-serialization of preference: Unexpected preference!");
  return (std::pair<pref*, int>(0,0));
}


// Interface to be called in the ..._impl function (psel-par(-top))
pref* CreatePreference(const List& pref_lst, const DataFrame& scores) {
  return(DoCreatePreference(pref_lst, scores, 0).first);
}
