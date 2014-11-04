#include <Rcpp.h>
using namespace Rcpp;

#include "pref-classes.h"


// Return hasse diagramm for given dataframe and preference
// [[Rcpp::export]]
NumericVector get_hasse_impl(DataFrame scores, List serial_pref) {
  
  NumericVector col1 = scores[0];  
  int ntuples = col1.size();
  
  // De-Serialize preference
  pref* p = CreatePreference(serial_pref, scores);

  // The edgelist (concatenated, to be transformed to a matrix afterwards)
  std::list<int> edges;
  
  // Helper variables
  std::list<int> window;
  std::list<int> remainder_el;
  std::list<int> new_remainder_el;
  std::list<int>::iterator i, j, j_del;
  std::list<int> last_window;
  bool dominated;
    
  // Fill remainder initially
  for (int k=0; k<ntuples; k++) remainder_el.push_back(k);
  
  // Loop as long as remainder is not 0
  while(remainder_el.size() != 0) {
    
    // Save last window and prepare new window
    last_window = window;
    window.clear();
    
    
    // Run BNL to get the next window
    for (i=remainder_el.begin(); i != remainder_el.end(); ++i) {
      dominated = false;
      for (j=window.begin(); j != window.end(); ++j) {
        if (p->cmp(*j, *i)) { // *j (window element) is better
          dominated = true;
          break; 
        } else if (p->cmp(*i, *j)) { // *i (picked element) is better
          // delete j and add j to new_v
          j_del = j;
          new_remainder_el.push_back(*j);
          ++j;
          window.erase(j_del);
  		    --j;
        }
      }
      if (!dominated) {
        window.push_back(*i);
      } else {
  	    new_remainder_el.push_back(*i);
  	  }
    }
    
    
    // Swap remainder elements
    remainder_el = new_remainder_el;
    new_remainder_el.clear();
    
    // Get all the better-than-relationships between current and last windows (not done in first step)
    for (i = last_window.begin(); i != last_window.end(); ++i) {
      for (j = window.begin(); j != window.end(); ++j) {
        if (p->cmp(*i, *j)) { // *i is better
          edges.push_back(*i);
          edges.push_back(*j);
        }
      }
    }
  }
    
  NumericMatrix res(2, edges.size()/2);
  std::copy(edges.begin(), edges.end(), res.begin());
  
  return(res);
}

