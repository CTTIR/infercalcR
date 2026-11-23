## Submission

This is a new submission: infercalcR 0.1.0.

## Test environments

* win-builder, Windows Server 2022 x64: R-devel (2026-11-17 r90926 ucrt)
  and R-release (4.6.2), each with `R CMD check --as-cran`. These runs
  checked the previous revision; the NOTE they reported and the changes
  made since are described below.
* Local: R 4.6.1 on Ubuntu 26.04 (x86_64), `R CMD check --as-cran` on the
  built tarball, including the three vignettes and the PDF manual, with
  `NOT_CRAN` unset and `_R_CHECK_CRAN_INCOMING_REMOTE_=true`.
* GitHub Actions (`--as-cran --no-manual`): ubuntu-latest (R devel,
  release, oldrel-1), windows-latest (R release), macos-latest
  (R release), all with Suggests installed, plus ubuntu-latest R 4.1
  with hard dependencies only (vignettes not built there).

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE

  New submission.

  Possibly misspelled words in DESCRIPTION: Boschloo (13:26)

  "Boschloo" is the surname of R. D. Boschloo, the author of the cited
  test (Boschloo 1970, <doi:10.1111/j.1467-9574.1970.tb00104.x>).

  The win-builder NOTE also listed "backends" as a possibly misspelled
  word and two README links to files that are not part of the source
  package (CONTRACT.md and LICENSE.md). The description now reads
  "back ends", and the README links to both files by absolute URL.

## Tests and check time

* Tests are plain R scripts in `tests/`. With `NOT_CRAN` unset they check
  the base engine's numerical results against stored values. Slower parts
  run only with `NOT_CRAN=true` or `INFER_SUITE_FULL=true`: a retained
  'testthat' regression lane, live re-runs of the original packages in
  Suggests, and two slow fixtures (a Monte Carlo test and an 'lme4' model).
  CI runs all of them.
* On win-builder the whole check took 107 s and installation 13 s; the
  tests took 21 s (R-release) and 22 s (R-devel), and "checking R code for
  possible problems" 33 s and 35 s.

## Notes

* The optional back ends 'cluster', 'nlme', 'MASS', 'nnet', 'lme4' and
  'geepack' (and 'digest'/'jsonlite' for canonical identities) are in
  Suggests and are used conditionally through `requireNamespace()`.
  Procedures that need a missing back end fail with an explicit error; the
  tests skip them.
* The vignettes use synthetic data only. Chunks that need 'digest' or
  'jsonlite' are evaluated only when those packages are installed.
