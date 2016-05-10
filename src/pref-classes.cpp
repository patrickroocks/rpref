#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

#include "pref-classes.h"


// Special score preference
// ------------------------

// Scorepref and maker
// -------------------

scorepref::scorepref(const NumericVector& data_) : data(data_) {}

scorepref::~scorepref() {}

ppref scorepref::make(const NumericVector& data_) {
  return (ppref)(make_shared<scorepref>(data_));
}


// --------------------------------------------------------------------------------------------------------------------------------
// BEGIN VS block
// --------------------------------------------------------------------------------------------------------------------------------


// Reversepref and maker
// ---------------------

reversepref::reversepref(ppref p_) : p(p_) {}

reversepref::~reversepref() {}

ppref reversepref::make(ppref p_) {
  return (ppref)(make_shared<reversepref>(p_));
}


// Complexpref, subclasses, and makers
// -----------------------------------

// General constructor for 2-ary complex preferences
complexpref::complexpref(ppref p1_, ppref p2_) : p1(p1_), p2(p2_) {}
complexpref::~complexpref() {}

// Sub-Constructors (instead of inheriting via "using")
unionpref::unionpref(              ppref p1_, ppref p2_) : complexpref(p1_, p2_) {}
prior::prior(                      ppref p1_, ppref p2_) : complexpref(p1_, p2_) {}
productpref::productpref(          ppref p1_, ppref p2_) : complexpref(p1_, p2_) {}
pareto::pareto(                    ppref p1_, ppref p2_) : productpref(p1_, p2_) {}
intersectionpref::intersectionpref(ppref p1_, ppref p2_) : productpref(p1_, p2_) {}


ppref pareto::make(ppref p1_, ppref p2_) {
  return (ppref)(make_shared<pareto>(p1_, p2_));
}

ppref prior::make(ppref p1_, ppref p2_) {
  return (ppref)(make_shared<prior>(p1_, p2_));
}

ppref intersectionpref::make(ppref p1_, ppref p2_) {
  return (ppref)(make_shared<intersectionpref>(p1_, p2_));
}

ppref unionpref::make(ppref p1_, ppref p2_) {
  return (ppref)(make_shared<unionpref>(p1_, p2_));
}



// Compare/Equality functions
// --------------------------

const bool scorepref::cmp(int i, int j) {
  return(data[i] < data[j]);
}

const bool scorepref::eq(int i, int j) {
  return(data[i] == data[j]);
}

const bool reversepref::cmp(int i, int j) {
  return(p->cmp(j, i));
}

const bool reversepref::eq(int i, int j) {
  return(p->eq(i, j));
}

const bool complexpref::eq(int i, int j) {
  return(p1->eq(i, j) && p2->eq(i, j));
}

const bool prior::cmp(int i, int j) {
  return(p1->cmp(i, j) || (p1->eq(i, j) && p2->cmp(i, j)));
}

const bool pareto::cmp(int i, int j) {
  return((p1->cmp(i, j) && (p2->cmp(i, j) || p2->eq(i, j))) ||
         (p2->cmp(i, j) && (p1->cmp(i, j) || p1->eq(i, j))));
}

const bool intersectionpref::cmp(int i, int j) {
  return(p1->cmp(i, j) && p2->cmp(i, j));
}

const bool unionpref::cmp(int i, int j) {
  return(p1->cmp(i, j) || p2->cmp(i, j));
}


// --------------------------------------------------------------------------------------------------------------------------------
// END VS block
// --------------------------------------------------------------------------------------------------------------------------------




// Only internal: Recursively create preference
pprefnum DoCreatePreference(const List& pref_lst, const DataFrame& scores, int next_id) {
  
  char pref_kind = as<char>(pref_lst["kind"]);
  pprefnum pair_res1, pair_res2;
  
  if (pref_kind == '*' || pref_kind == '&' || pref_kind == '|' || pref_kind == '+') {
    
    pair_res1 = DoCreatePreference(as<List>(pref_lst["p1"]), scores, next_id);
    pair_res2 = DoCreatePreference(as<List>(pref_lst["p2"]), scores, pair_res1.second);
    
    // Binary complex preference
    ppref res = NULL;
    
    switch(pref_kind) {
      case '*': res = pareto::make(          pair_res1.first, pair_res2.first); break;
      case '&': res = prior::make(           pair_res1.first, pair_res2.first); break;
      case '|': res = intersectionpref::make(pair_res1.first, pair_res2.first); break;
      case '+': res = unionpref::make(       pair_res1.first, pair_res2.first); break;
    }
    
    return(pprefnum(res, pair_res2.second));
    
  } else if (pref_kind == '-') {
    
    pair_res1 = DoCreatePreference(as<List>(pref_lst["p"]), scores, next_id);
    ppref res = reversepref::make(pair_res1.first);
    return(pprefnum(res, pair_res1.second));
    
  } else if (pref_kind == 's') {
    
    // Score (base) preference, scorevector is const!
    ppref res = scorepref::make(as<NumericVector>(scores[next_id]));
    next_id++;
    return(pprefnum(res, next_id));
    
  } 
  
  stop("Error during de-serialization of preference: Unexpected preference!");
  return (pprefnum(0,0));
}

// Interface to be called in the ..._impl function (psel-par(-top))
ppref CreatePreference(const List& pref_lst, const DataFrame& scores) {
  return(move(DoCreatePreference(pref_lst, scores, 0).first));
}
