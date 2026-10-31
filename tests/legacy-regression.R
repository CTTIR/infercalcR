# Retained framework coverage moved intact with its numerical owner.
available <- vapply(c("testthat", "withr"), requireNamespace, logical(1), quietly=TRUE)
if (!all(available)) {
  if (identical(Sys.getenv("INFER_SUITE_FULL"), "true"))
    stop("The full lane requires testthat and withr for retained legacy regressions")
  cat("SKIP retained testthat regressions: optional development dependencies absent in minimal lane\n")
} else {
  library(testthat)
  library(infercalcR)
  test_check("infercalcR")
}
