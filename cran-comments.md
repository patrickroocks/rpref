## Test environments

* rhub: Debian Linux, R-devel, GCC ASAN/UBSAN
* win-builder: R Under development (unstable) (2025-08-06 r88516 ucrt)
* local: Ubuntu 24.04, R 4.5.1

## R CMD check results

I get the NOTE "Compilation used the following non-portable flag(s): ‘-mno-omit-leaf-frame-pointer’"
This NOTE comes from RcppParallel and is required for correct operation. This is consistent with other accepted CRAN packages using RcppParallel.
