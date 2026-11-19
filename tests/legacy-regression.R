# Retained framework coverage moved intact with its numerical owner.
# This lane takes several minutes, so it runs only in development and CI checks
# (NOT_CRAN=true, as set by devtools/rcmdcheck) or the full lane (INFER_SUITE_FULL=true).
full <- identical(Sys.getenv("INFER_SUITE_FULL"), "true")
if (!full && !identical(Sys.getenv("NOT_CRAN"), "true")) {
  cat("SKIP retained testthat regressions: set NOT_CRAN=true or INFER_SUITE_FULL=true to run them\n")
} else {
  available <- vapply(c("testthat", "withr"), requireNamespace, logical(1), quietly=TRUE)
  if (!all(available)) {
    if (full)
      stop("The full lane requires testthat and withr for retained legacy regressions")
    cat("SKIP retained testthat regressions: optional development dependencies absent in minimal lane\n")
  } else {
    library(testthat)
    library(infercalcR)
    test_check("infercalcR")
  }
}
