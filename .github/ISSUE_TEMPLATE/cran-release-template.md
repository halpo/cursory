---
name: CRAN Release Template
about: Checklist for CRAN releases.
title: "[Release] Version X.Y.Z"
labels: ''
assignees: halpo

---

Complete all the following prior to releasing to CRAN:

- [ ] Documentation
  + [ ] DESCRIPTION is up to date
    * [ ] Title case for package title.
    * [ ] Description includes complete sentences with proper grammar.
  + [ ] Man pages up to date.
    * [ ] All functions include examples
    * [ ] All functions include `\value`/`@return`
  + [ ] `NEWS.md` is up to date.
- [ ] `devtools` pre-release  checks
  + [ ] `spell_check()`
  + [ ] `check(cran=TRUE)`
  + [ ] `check_rhub()`
  + [ ] `check_win_release()`
  + [ ] `check_win_devel()`
- [ ] create/update `cran-comments.md` (create with `usethis::use_cran_comments()`)
- [ ] `release()` to CRAN. 
- [ ] tag release in git.
