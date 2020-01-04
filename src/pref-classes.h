#pragma once

#include <Rcpp.h>
#include <memory>

// Preference classes using shared pointers
// ----------------------------------------

class pref
{
public:
  pref() {};
  virtual ~pref() {};
  
  virtual bool cmp(int, int) const = 0;
  virtual bool eq(int, int) const = 0;
}; 

// shared pointer on preferences
typedef std::shared_ptr<pref> ppref;

class complexpref : public pref
{
public:
  ppref p1;
  ppref p2;
  
  complexpref(ppref, ppref);
  
  bool eq(int i, int j) const override;
};

// Common Superclass for Pareto and intersection 
// as they can both handled by Scalagon
class productpref : public complexpref
{
public:
  productpref(ppref, ppref);
};

// cmp is implemented differently for each preference
class pareto : public productpref
{
public:
  bool cmp(int i, int j) const override;
  
  pareto(ppref, ppref);
  
  static ppref make(ppref, ppref);
};

class unionpref : public complexpref
{
public:
  static ppref make(ppref p1_, ppref p2_);
  bool cmp(int i, int j) const override;
  
  unionpref(ppref, ppref);
};

class prior : public complexpref
{
public:
  static ppref make(ppref p1_, ppref p2_);
  bool cmp(int i, int j) const override;
  
  prior(ppref, ppref);
};

class intersectionpref : public productpref
{
public:
  static ppref make(ppref p1_, ppref p2_);
  bool cmp(int i, int j) const override;
  
  intersectionpref(ppref, ppref);
};

class reversepref : public pref
{
public:
  ppref p;
  
  reversepref(ppref p);
  
  static ppref make(ppref p_);
  
  bool cmp(int i, int j) const override;
  bool eq(int i, int j) const override;
};


// Special score preference
// ------------------------

class scorepref : public pref
{
public:
  // this must not be a reference! (std::vector is faster than numeric vector)
  const std::vector<double> data;
  
  scorepref(const Rcpp::NumericVector& data);
  
  static ppref make(const Rcpp::NumericVector& data_);
  
  bool cmp(int i, int j) const override;
  bool eq(int i, int j) const override;
};


typedef std::pair<ppref, int> pprefnum;


// Deserialize preference 
ppref CreatePreference(const Rcpp::List& pref_lst, const Rcpp::DataFrame& scores);
