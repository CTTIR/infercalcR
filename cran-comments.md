## Submission

This is a new submission: infercalcR 0.1.0.

## Test environments

* Local: R 4.6.2 on Ubuntu 26.04 (x86_64, Docker container),
  `R CMD check --as-cran --no-manual` on the built tarball with `NOT_CRAN`
  unset, including the three vignettes. The PDF manual was built separately
  with `R CMD Rd2pdf` and pdflatex.
* GitHub Actions (`--as-cran --no-manual`): ubuntu-latest (R devel,
  release, oldrel-1), windows-latest (R release), macos-latest
  (R release), all with Suggests installed, plus ubuntu-latest R 4.1
  with hard dependencies only (vignettes not built there).

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

## Tests and check time

* Tests are plain R scripts in `tests/`. With `NOT_CRAN` unset they check
  the base engine's numerical results against stored values. Slower parts
  run only with `NOT_CRAN=true` or `INFER_SUITE_FULL=true`: a retained
  'testthat' regression lane, live re-runs of the original packages in
  Suggests, and two slow fixtures (a Monte Carlo test and an 'lme4' model).
  CI runs all of them.
* With `NOT_CRAN` unset, the tests took 2.5 min CPU (3 min elapsed) and
  the whole check 13 min elapsed on a heavily loaded 8-core machine; most
  of the rest is "checking R code for possible problems" (about 4 min CPU)
  and installation (1.5 min).

## Notes

* The optional backends 'cluster', 'nlme', 'MASS', 'nnet', 'lme4' and
  'geepack' (and 'digest'/'jsonlite' for canonical identities) are in
  Suggests and are used conditionally through `requireNamespace()`.
  Procedures that need a missing backend fail with an explicit error; the
  tests skip them.
* The vignettes use synthetic data only. Chunks that need 'digest' or
  'jsonlite' are evaluated only when those packages are installed.
