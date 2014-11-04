

class pref {
public:
	virtual bool cmp(int, int) = 0;
	virtual bool eq(int, int) = 0;
  
  // Virtual desctructor, so that sub-destructors are called!
  virtual ~pref() {};
}; 

class scorepref : public pref {
public:
  const NumericVector data;
  
  scorepref(const NumericVector& data) : data(data) {};

	bool cmp(int i, int j);
	bool eq(int i, int j);
};

class complexpref : public pref {
public:
  pref *p1;
	pref *p2;
  
  complexpref() : p1(0), p2(0) {};
  ~complexpref();

	bool eq(int i, int j);
};

// Common superclass for Pareto and intersection 
class productpref : public complexpref {};

class pareto : public productpref {
public:
	bool cmp(int i, int j);
};

class unionpref : public complexpref {
public:
	bool cmp(int i, int j);
};

class prior : public complexpref {
public:
	bool cmp(int i, int j);
};

class intersectionpref : public productpref {
public:
	bool cmp(int i, int j);
};

class reversepref : public pref {
public:
	pref *p;
  
  reversepref() : p(0) {};
  ~reversepref();

	bool cmp(int i, int j);
	bool eq(int i, int j);
};

// Derserialize preference 
pref* CreatePreference(const List& pref_lst, const DataFrame& scores);
