## Submission

This is a new submission: infercalcR 0.1.0.

## Test environments

* Local: macOS 27 on Apple silicon, R 4.6.1 (aarch64-apple-darwin),
  `R CMD check --as-cran --no-manual` on the built tarball with `NOT_CRAN`
  unset. LaTeX is not installed on this machine, so the PDF manual was not
  built locally.
* GitHub Actions (r-lib/actions v2, `--as-cran`): macos-latest (R release),
  windows-latest (R release), ubuntu-latest (R devel, R release,
  R oldrel-1), plus ubuntu-latest R 4.1 with hard dependencies only.

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

## Notes

* The optional backends 'cluster', 'nlme', 'MASS', 'nnet', 'lme4' and
  'geepack' (and 'digest'/'jsonlite' for canonical identities) are in
  Suggests and are used conditionally through `requireNamespace()`.
  Procedures that need a missing backend fail with an explicit error; the
  tests skip them.
* Tests are ordinary R scripts plus a retained 'testthat' regression lane;
  together they run in about one minute.
