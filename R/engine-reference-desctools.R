# Reference engines that call DescTools, the package DescrTab2 uses for these
# tests. Each adapter has the formals of the base function, validates input with
# the base method's own input checks, and returns the base function's native
# fields computed by the original. Files named engine-reference-*.R are
# collated before engine-registry.R, which refers to these functions.

.ref_cochran_q <- function(data, alternative = "two.sided", conf.level = 0.95, missing = "error") {
  b <- .obs_cochran_q_check_blocks(data, missing, alternative, conf.level)
  raw <- as.matrix(data[, setdiff(names(data), "id"), drop = FALSE])
  stopifnot(all(is.na(raw) | raw %in% c(0, 1)))
  x <- b$x
  k <- ncol(x)
  common <- list(method = "cochran_q", n_input = b$n_input, n_complete = nrow(x), n_excluded = b$n_excluded)
  # Only discordant blocks carry information; without them the original returns NaN.
  if (all(rowSums(x) %in% c(0, k)))
    return(c(common, list(status = "unavailable", reason = "No discordant blocks: conditional variance is zero.",
      p_value = NULL)))
  test <- DescTools::CochranQTest(x)
  p <- test$p.value
  c(common, list(status = "ok", occasions = colnames(x), statistic = unname(test$statistic),
    df = unname(test$parameter), p_value = p,
    descriptive = list(event = 1, event_counts = colSums(x), event_proportions = colMeans(x)),
    confidence_interval = NULL, interval_status = "not_computed", requested_confidence_level = conf.level,
    alpha = 1 - conf.level, reject = p < 1 - conf.level, missing_strategy = missing,
    assumptions = "Independent participants; matched occasions; complete blocks. Complete-case inference requires justified MCAR. Chi-squared asymptotic omnibus test; no automatic pairwise testing or method substitution.",
    reference_output = unclass(test)))
}
