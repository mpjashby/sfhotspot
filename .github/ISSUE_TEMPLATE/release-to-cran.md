---
name: Release to CRAN
about: Checklist of actions before submitting a release to CRAN
title: 'RELEASE: '
labels: release
assignees: mpjashby

---

These checks should be run **in this order** before submitting to CRAN. If any of these tests finds further work is needed on the package, start the checklist again from scratch.

- [ ] `urlchecker::url_check()`
- [ ] `devtools::spell_check()`
- [ ] `devtools::document()`
- [ ] `devtools::build_readme()`
- [ ] `devtools::build_vignettes()`
- [ ] `pkgdown::build_site()`
- [ ] `devtools::check()`
- [ ] `devtools::check_win_devel()`
- [ ] `devtools::check_rhub()`
- [ ] update `NEWS.md`
- [ ] update `DESCRIPTION`, including incrementing version number
- [ ] update `cran-comments.md`
- [ ] push any uncommitted files

Finally, to submit to CRAN run `devtools::release()`.
