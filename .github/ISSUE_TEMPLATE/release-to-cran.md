---
name: Release to CRAN
about: Checklist of actions before submitting a release to CRAN
title: 'RELEASE: '
labels: release
assignees: mpjashby

---

These checks should be run **in this order** before submitting to CRAN. If any of these tests finds further work is needed on the package, start the checklist again from scratch. Changes only to files excluded by `.Rbuildignore`, such as `cran-comments.md`, do not require all checks to be repeated.

## Prepare the release

- [ ] Review the package's current [CRAN check results](https://cran.rstudio.org/web/checks/check_results_sfhotspot.html) and resolve any problems
- [ ] Update and proofread `NEWS.md`
- [ ] Update `DESCRIPTION`, including incrementing the version number
- [ ] Confirm that the maintainer name and email address in `DESCRIPTION` are current
- [ ] `urlchecker::url_check()`
- [ ] `devtools::spell_check()`
- [ ] `devtools::document()`
- [ ] `devtools::build_readme()`
- [ ] `pkgdown::build_site()`

## Local checks

- [ ] Run `devtools::check(remote = TRUE, manual = TRUE)`
- [ ] Confirm 0 errors, 0 warnings and no unexplained notes
- [ ] Update `cran-comments.md` with the local check results and explanations for any notes
- [ ] Confirm that all generated and modified package files are committed
- [ ] Commit and Sync

## Remote checks

- [ ] Confirm GitHub `R CMD check` workflow passes
- [ ] Run appropriate R-hub v2 checks with `rhub::rhub_check()` and inspect every result
- [ ] `devtools::check_win_devel()`
- [ ] `devtools::check_mac_release()`
- [ ] Confirm 0 errors, 0 warnings and no unexplained notes across the remote checks

## Finalize submission

- [ ] Add remote check results and any necessary explanations to `cran-comments.md`
- [ ] Commit and sync updated `cran-comments.md`
- [ ] `devtools::build()`

## Submit

- [ ] `devtools::submit_cran()`
- [ ] Accept the confirmation email from CRAN
