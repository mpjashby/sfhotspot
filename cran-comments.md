## R CMD check results

0 errors | 0 warnings | 0 notes

Apologies for the submission one week after the previous one, but v0.9.0 
introduced a bug (#46) that is now fixed in v0.9.1.

CRAN currently shows an error "there is no package called 'terra'" for sfhotspot 
on r-devel-macos-arm64. terra is a second-order dependency via SpatialKDE, but
the current CRAN checks page for SpatialKDE and terra show no errors on 
r-devel-macos-arm64 and I cannot replicate the error on macOS locally, so I
suspect the error shown on CRAN checks is a configuration issue on the test
server. There are no errors on any other systems.
