#include "hasse.h"

using namespace Rcpp;

// Return hasse diagramm for given dataframe and preference
// [[Rcpp::export]]
NumericVector get_hasse_impl(const DataFrame& scores, List serial_pref)
{
  
  NumericVector col1 = scores[0];  
  const int ntuples = col1.size();
  
  // De-Serialize preference
  const ppref p = CreatePreference(serial_pref, scores);

  // Get edgelist (concatenated, to be transformed to a matrix afterwards)
  std::list<int> edges = get_transitive_reduction(p, ntuples);
    
  NumericMatrix res(2, edges.size()/2);
  std::copy(edges.begin(), edges.end(), res.begin());
  
  return res;
}


// Return transitive reduction as 1-dim list (x1,x2,x3,x4) means
// x1 < x2 and x3 < x4 in the sense of the transitive reduction
std::list<int> get_transitive_reduction(const ppref& p, int ntuples)
{
  // The edgelist
  std::list<int> edges;

  // Naive approach for transitive reduction: Check all pairs and check if there exists some element between them
  for (int i = 0; i < ntuples; i++) {
    for (int j = 0; j < ntuples; j++) {
      if (p->cmp(i, j)) {
        bool found = false;
        for (int k = 0; k < ntuples; k++) {
          if (p->cmp(i, k) && p->cmp(k, j)) {
            found = true;
            break;
          }
        }
        if (!found) {
          edges.push_back(i);
          edges.push_back(j);
        }
      }
    }
  }

  return edges;
}
  
