
#include "topk_setting.h"


// --------------------------------------------------------------------------------------------------------------------------------
// From here from VS
// --------------------------------------------------------------------------------------------------------------------------------



bool topk_setting::do_break(int level, int ntuples) { // gets level of this iteration!
    if (and_connected) {
      // Take intersection, break if one limit is reached
      if ((topk     != -1 && ntuples >= topk)     || 
          (at_least != -1 && ntuples >= at_least) || 
          level   == toplevel) return(true);
    } else {
      // Take union, break if all limits are reached (or limits are not set)
      if ((topk     == -1 || ntuples >= topk)      && 
          (at_least == -1 || ntuples >= at_least)  && 
          (toplevel == -1 || level   >= toplevel)) return(true);
    }
  return(false);
}
