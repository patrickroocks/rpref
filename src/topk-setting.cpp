#include "topk-setting.h"

bool topk_setting::do_break(int level, int ntuples) const
{ 
  // gets level of this iteration!
  if (and_connected) {
    // Take intersection, break if one limit is reached
    return (topk     != -1 && ntuples >= topk)
        || (at_least != -1 && ntuples >= at_least)
        || level   == toplevel;  // (... && toplevel != -1) is always implicitly checked (level is never -1)
  } else {
    // Take union, break if all limits are reached (or limits are not set)
    return (topk     == -1 || ntuples >= topk)
        && (at_least == -1 || ntuples >= at_least)
        && (toplevel == -1 || level   >= toplevel);
  }
}
