# Reference engines that call exact2x2, the package DescrTab2 uses for the
# exact McNemar test; its midp option is the established mid-p version.

# Paired table with x in rows and y in columns, events first: the original's
# conditional odds ratio is then n10 / n01, the base engine's orientation.
.ref_mcnemar_table <- function(counts)
  matrix(c(counts[["n11"]], counts[["n01"]], counts[["n10"]], counts[["n00"]]), 2L, 2L)

.ref_mcnemar <- function(counts, alternative, conf.level, midp, check_settings, check_counts, method) {
  check_settings(alternative, conf.level)
  counts <- check_counts(counts)
  b <- unname(counts["n10"])
  c <- unname(counts["n01"])
  n <- b + c
  if (n == 0)
    return(list(status = "not_applicable", reason = "No discordant pairs; no conditional test information.",
      n_total = sum(counts), n_discordant = 0, p_value = NULL, confidence_interval = NULL))
  test <- exact2x2::exact2x2(.ref_mcnemar_table(counts), paired = TRUE, alternative = alternative,
    tsmethod = "central", conf.level = conf.level, midp = midp)
  # The original's interval is for the conditional odds ratio b / c; b / (b + c) = OR / (1 + OR).
  odds <- as.numeric(test$conf.int)
  interval <- ifelse(is.infinite(odds), 1, odds / (1 + odds))
  list(status = "ok", method = method, n_total = sum(counts), n_discordant = n, n_x_only = b, n_y_only = c,
    conditional_positive_probability = b / n, marginal_difference = unname((b - c) / sum(counts)),
    p_value = test$p.value, alternative = alternative, confidence_interval = interval,
    confidence_level = conf.level,
    interval_method = if (midp) "exact2x2 mid-p interval for P(x-only | discordant), transformed from the conditional odds ratio" else
      "Clopper-Pearson for P(x-only | discordant), transformed from exact2x2's conditional odds ratio interval",
    limitations = if (midp) "Mid-p does not guarantee exact type-I error control. Interval concerns discordance probability, not marginal difference." else
      "Interval concerns discordance probability, not marginal difference; pairs must be independent across units.",
    reference_output = unclass(test))
}

.ref_mcnemar_exact_conditional <- function(counts, alternative = "two.sided", conf.level = 0.95)
  .ref_mcnemar(counts, alternative, conf.level, midp = FALSE, .obs_mcnemar_exact_conditional_check_settings,
    .obs_mcnemar_exact_conditional_check_counts, "Exact conditional McNemar test")

.ref_mcnemar_midp <- function(counts, alternative = "two.sided", conf.level = 0.95)
  .ref_mcnemar(counts, alternative, conf.level, midp = TRUE, .obs_mcnemar_midp_check_settings,
    .obs_mcnemar_midp_check_counts, "Conditional McNemar mid-p test")
