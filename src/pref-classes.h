
#include <Rcpp.h>

#include <memory> 

using namespace std;
using namespace Rcpp;



// --------------------------------------------------------------------------------------------------------------------------------
// BEGIN VS block
// --------------------------------------------------------------------------------------------------------------------------------


// Preference classes using shared pointers
// ----------------------------------------

class pref {
public:
  
  pref() {};
  ~pref() {};
  
  virtual const bool cmp(int, int) = 0;
  virtual const bool eq(int, int) = 0;
}; 

// shared pointer on preferences
typedef shared_ptr<pref> ppref;


class complexpref : public pref {
public:
  ppref p1;
  ppref p2;
  
  complexpref(ppref, ppref);
  ~complexpref();
  
  const bool eq(int i, int j);
};

// Common Superclass for Pareto and intersection 
// as they can both handled by Scalagon
class productpref : public complexpref {
public:
  
  //using complexpref::complexpref;
  productpref(ppref, ppref);
};

// cmp is implemented differently for each preference
class pareto : public productpref {
public:
  const bool cmp(int i, int j);
  
  //using productpref::productpref;
  pareto(ppref, ppref);
  
  static ppref make(ppref, ppref);
};

class unionpref : public complexpref {
public:
  static ppref make(ppref p1_, ppref p2_);
  const bool cmp(int i, int j);
  
  // using complexpref::complexpref;
  unionpref(ppref, ppref);
};

class prior : public complexpref {
public:
  static ppref make(ppref p1_, ppref p2_);
  const bool cmp(int i, int j);
  
  // using complexpref::complexpref;
  prior(ppref, ppref);
};

class intersectionpref : public productpref {
public:
  static ppref make(ppref p1_, ppref p2_);
  const bool cmp(int i, int j);
  
  //using productpref::productpref;
  intersectionpref(ppref, ppref);
};

class reversepref : public pref {
public:
  ppref p;
  
  reversepref(ppref p);
  ~reversepref();
  
  static ppref make(ppref p_);
  
  const bool cmp(int i, int j);
  const bool eq(int i, int j);
};



// --------------------------------------------------------------------------------------------------------------------------------
// END VS block
// --------------------------------------------------------------------------------------------------------------------------------

// Special score preference
// ------------------------

class scorepref : public pref {
public:
  
  // this must not be a reference!
  const NumericVector data;
  
  scorepref(const NumericVector& data);
  ~scorepref();	
  
  static ppref make(const NumericVector& data_);
  
  const bool cmp(int i, int j);
  const bool eq(int i, int j);
};


typedef pair<ppref, int> pprefnum;


// Derserialize preference 
ppref CreatePreference(const List& pref_lst, const DataFrame& scores);
