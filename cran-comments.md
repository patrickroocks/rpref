## Test environments

* rhub: Debian Linux, R-devel, GCC ASAN/UBSAN
* win-builder: R Under development (unstable) (2023-01-27 r83711 ucrt)
* local: Ubuntu 22.04, R 4.2.1

## R CMD check results

There were NOTES that I should use C++17.
But RcppParallel currently raises compiler warnings with C++17, so it would be very kind if I could keep C++11 for this package, cf. https://github.com/RcppCore/RcppParallel/issues/191.
All rhub/local results were "OK".
