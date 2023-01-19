#pragma once

#include <Rcpp.h>

class topk_setting
{
public:
  const int topk;
  const int at_least;
  const int toplevel;
  const bool and_connected;
  const bool is_simple; // top-level == 1 and no other things set (i.e. non-top-k evaluation)
  
  topk_setting(int topk = -1, int at_least = -1, int toplevel = -1, bool and_connected = true) : 
    topk(topk), 
    at_least(at_least), 
    toplevel(toplevel), 
    and_connected(and_connected),
    is_simple(topk == -1 && at_least == -1 && toplevel == 1) {}
  
  bool do_break(int level, int ntuples) const;
  
  template<typename T> void cut(std::vector<T>& vec) const
  {
    // cut if topk is set and {we have and AND-connection OR if topk is the only value}
    if (topk != -1 && static_cast<std::size_t>(topk) < vec.size() && (and_connected || (toplevel == -1 && at_least == -1))) {
      vec.resize(topk);
    }
  }
};
