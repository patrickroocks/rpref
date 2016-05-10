#include <Rcpp.h>

using namespace std;
using namespace Rcpp;


// --------------------------------------------------------------------------------------------------------------------------------
// From here from VS
// --------------------------------------------------------------------------------------------------------------------------------


class topk_setting
{
public:
  
  int topk;
  int at_least;
  int toplevel;
  bool and_connected;
  bool is_simple; // top-level == 1 and no other things set (i.e. non-top-k evaluation)
  
  topk_setting(int topk = -1, int at_least = -1, int toplevel = -1, bool and_connected = true) : 
    topk(topk), at_least(at_least), toplevel(toplevel), and_connected(and_connected),
    is_simple(topk == -1 && at_least == -1 && toplevel == 1) {}
  
  ~topk_setting() {};
  
  // for top-k algorithms
  bool do_break(int level, int ntuples);
  
  // Template for top-k cut function (must be completely in header file)
  template<typename T> void cut(list<T> &lst, int ntuples) {
    // cut if topk is set and {we have and AND - connection OR if topk is the only value}
    if (topk != -1 && (and_connected || (toplevel == -1 && at_least == -1)))
      lst.resize(min(topk, ntuples));
  }
};

