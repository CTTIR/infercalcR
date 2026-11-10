# Reference engines for methods DescrTab2 does not offer: the established
# brunnermunzel package, and R's own anova.mlm for the Greenhouse-Geisser test.

.ref_brunner_munzel <- function(x, y, alternative = "two.sided", conf.level = 0.95) {
  sizes <- .obs_brunner_munzel_check(x, y, alternative, conf.level)
  test <- brunnermunzel::brunnermunzel.test(x, y, alternative = alternative, alpha = 1 - conf.level)
  # The original estimates P(X < Y) + 0.5 P(X = Y); the base estimand is its
  # complement, so the estimate, interval and statistic change sign or side.
  theta <- 1 - unname(test$estimate)
  common <- list(method = "Brunner-Munzel t approximation", n_x = sizes[[1L]], n_y = sizes[[2L]], estimate = theta)
  # With zero placement variance the original reports an infinite statistic and p = 0.
  if (!is.finite(test$statistic))
    return(c(common, list(status = "unavailable",
      reason = "Zero estimated placement variance: no t inference or automatic replacement is available.",
      p_value = NULL, confidence_interval = NULL)))
  p <- test$p.value
  two_sided <- alternative == "two.sided"
  raw <- if (two_sided) sort(1 - as.numeric(test$conf.int)) else NULL
  c(common, list(status = "ok", null = 0.5,
    estimand = "P(X > Y) + 0.5 P(X = Y); greater means stochastic superiority of x", statistic = -unname(test$statistic),
    df = unname(test$parameter), p_value = p, alternative = alternative, confidence_level = conf.level,
    confidence_interval = if (two_sided) pmax(0, pmin(1, raw)), unconstrained_interval = raw,
    interval_status = if (two_sided) "computed" else "not_computed",
    interval_method = if (two_sided) "brunnermunzel placement-variance t interval for the complement, transformed and intersected with [0,1]" else
      "The original reports a two-sided interval only; no one-sided interval is reported.",
    alpha = 1 - conf.level, reject = p < 1 - conf.level,
    assumptions = "Independent observations and groups; ordinal or continuous outcome; unequal shapes/spreads allowed. Small-sample accuracy is not guaranteed. No mean/median shift interpretation, automatic method substitution or data-driven direction.",
    reference_output = unclass(test)))
}

.ref_anova_repeated_one_way_gg <- function(data, occasion_columns, independent_subjects = FALSE,
    alternative = "two.sided", conf.level = 0.95, complete_cases = FALSE) {
  prepared <- .obs_anova_repeated_one_way_gg_prepare(data, occasion_columns, independent_subjects,
    alternative, conf.level, complete_cases)
  y <- prepared$y
  common <- list(n_total = prepared$n_total, n_used = prepared$n, n_excluded = prepared$n_total - prepared$n,
    occasions = occasion_columns, alpha = 1 - conf.level, confidence_interval = NULL,
    interval_status = "not_computed", requested_confidence_level = conf.level)
  # The base engine's guards decide whether a test is defined; without them
  # anova.mlm turns rounding noise of degenerate data into a confident F test.
  parts <- .obs_anova_repeated_one_way_gg_decompose(y, prepared$n, prepared$k)
  if (!is.null(parts$reason))
    return(c(common, list(status = "not_applicable", reason = parts$reason, p_value = NULL)))
  table <- tryCatch(stats::anova(stats::lm(y ~ 1), X = ~1, test = "Spherical"), error = function(e) NULL)
  statistic <- if (is.null(table)) NA_real_ else table[["F"]][[1L]]
  p <- if (is.null(table)) NA_real_ else table[["G-G Pr"]][[1L]]
  if (!is.finite(statistic) || !is.finite(p))
    return(c(common, list(status = "not_applicable", reason = "The original returned no finite Greenhouse-Geisser test.",
      p_value = NULL)))
  c(common, list(status = "ok", method = "One-way repeated-measures ANOVA; routine Greenhouse-Geisser correction",
    statistic = statistic, p_value = p,
    uncorrected_df = c(numerator = table[["num Df"]][[1L]], denominator = table[["den Df"]][[1L]]),
    epsilon_note = "stats::anova.mlm reports the Greenhouse-Geisser epsilon only in its printed heading; corrected degrees of freedom are not returned.",
    assumptions = "Independent subjects with jointly normal repeated outcomes for the approximate GG F inference; one within-subject factor, no between-subject factors or covariates. Complete-case inference needs justified missingness assumptions.",
    reference_output = unclass(table)))
}
