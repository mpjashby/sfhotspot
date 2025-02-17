## R CMD check results

0 errors | 0 warnings | 0 notes

Apologies for the release one week after the previous one, but v0.9.0 introduced
a bug that is now fixed in v0.9.1.

Checks at https://cran.r-project.org/web/checks/check_results_sfhotspot.html
currently show an error on r-devel-macos-arm64 due to the package terra not
being found. I cannot replicate this locally and looking at other packages it 
appears to be a transient issue with packages on that platform.
