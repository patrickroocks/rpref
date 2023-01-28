#include "pref-classes.h"

using namespace Rcpp;

// Special score preference
// ------------------------

// Scorepref and maker
// -------------------

scorepref::scorepref(const NumericVector& data_) : data(as<std::vector<double>>(data_)) {}

ppref scorepref::make(const NumericVector& data_)
{
  return std::make_shared<scorepref>(data_);
}

// Reversepref and maker
// ---------------------

reversepref::reversepref(ppref p_) : p(p_) {}

ppref reversepref::make(ppref p_)
{
  return std::make_shared<reversepref>(p_);
}


// Complexpref, subclasses, and makers
// -----------------------------------

// General constructor for 2-ary complex preferences
complexpref::complexpref(ppref p1_, ppref p2_) : p1(p1_), p2(p2_) {}

// Sub-Constructors (instead of inheriting via "using")
unionpref::unionpref(              ppref p1_, ppref p2_) : complexpref(p1_, p2_) {}
prior::prior(                      ppref p1_, ppref p2_) : complexpref(p1_, p2_) {}
productpref::productpref(          ppref p1_, ppref p2_) : complexpref(p1_, p2_) {}
pareto::pareto(                    ppref p1_, ppref p2_) : productpref(p1_, p2_) {}
intersectionpref::intersectionpref(ppref p1_, ppref p2_) : productpref(p1_, p2_) {}


ppref pareto::make(ppref p1_, ppref p2_)
{
  return std::make_shared<pareto>(p1_, p2_);
}

ppref prior::make(ppref p1_, ppref p2_)
{
  return std::make_shared<prior>(p1_, p2_);
}

ppref intersectionpref::make(ppref p1_, ppref p2_)
{
  return std::make_shared<intersectionpref>(p1_, p2_);
}

ppref unionpref::make(ppref p1_, ppref p2_)
  {
  return std::make_shared<unionpref>(p1_, p2_);
}


// Compare/Equality functions
// --------------------------

bool scorepref::cmp(int i, int j) const
{
  return data[i] < data[j];
}

bool scorepref::eq(int i, int j) const
{
  return data[i] == data[j];
}

bool reversepref::cmp(int i, int j) const
{
  return p->cmp(j, i);
}

bool reversepref::eq(int i, int j) const
{
  return p->eq(i, j);
}

bool complexpref::eq(int i, int j) const
{
  return p1->eq(i, j) && p2->eq(i, j);
}

bool prior::cmp(int i, int j) const
{
  return p1->cmp(i, j) || (p1->eq(i, j) && p2->cmp(i, j));
}

bool pareto::cmp(int i, int j) const
{
  return (p1->cmp(i, j) && (p2->cmp(i, j) || p2->eq(i, j))) ||
         (p2->cmp(i, j) && (p1->cmp(i, j) || p1->eq(i, j)));
}

bool intersectionpref::cmp(int i, int j) const
{
  return p1->cmp(i, j) && p2->cmp(i, j);
}

bool unionpref::cmp(int i, int j) const
{
  return p1->cmp(i, j) || p2->cmp(i, j);
}


// Only internal: Recursively create preference
ppref_with_id DoCreatePreference(const List& pref_lst, const DataFrame& scores, int next_id)
{
  const char pref_kind = as<char>(pref_lst["kind"]);
  ppref_with_id pair_res1, pair_res2;
  
  if (pref_kind == '*' || pref_kind == '&' || pref_kind == '|' || pref_kind == '+') {
    
    pair_res1 = DoCreatePreference(as<List>(pref_lst["p1"]), scores, next_id);
    pair_res2 = DoCreatePreference(as<List>(pref_lst["p2"]), scores, pair_res1.second);
    
    // Binary complex preference
    const ppref res_pref = [&]() -> ppref {
      switch(pref_kind) {
        case '*': return pareto::make(          pair_res1.first, pair_res2.first);
        case '&': return prior::make(           pair_res1.first, pair_res2.first);
        case '|': return intersectionpref::make(pair_res1.first, pair_res2.first);
        case '+': return unionpref::make(       pair_res1.first, pair_res2.first);
      }
      return nullptr; // cannot happen
    }();
    
    return ppref_with_id(res_pref, pair_res2.second);
    
  } else if (pref_kind == '-') {
    
    pair_res1 = DoCreatePreference(as<List>(pref_lst["p"]), scores, next_id);
    ppref res_pref = reversepref::make(pair_res1.first);
    return ppref_with_id(res_pref, pair_res1.second);
    
  } else if (pref_kind == 's') {
    
    // Score (base) preference, scorevector is const!
    ppref res_pref = scorepref::make(as<NumericVector>(scores[next_id]));
    next_id++;
    return ppref_with_id(res_pref, next_id);
    
  } 
  
  stop("Error during deserialization of preference: Unexpected preference!");
  return ppref_with_id(0, 0);
}

// Interface to be called in the ..._impl function (psel-par(-top))
ppref CreatePreference(const List& pref_lst, const DataFrame& scores)
{
  return std::move(DoCreatePreference(pref_lst, scores, 0).first);
}
