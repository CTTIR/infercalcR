# Reference engines that call Exact, the Boschloo comparator chosen on
# 3 October 2026 (decision D12) because exact2x2::boschloo, which DescrTab2
# calls, drops tied tables. Exact also provides the unconditional pooled-Z
# test. Tables have the groups in rows (Exact's cond.row = TRUE) and events in
# the first column; Exact's "less" is H1: p1 < p2, as in the base engines.

.ref_exact_p <- function(table, method, alternative, tsmethod = NULL) {
  args <- list(data = unname(table), method = method, alternative = alternative, to.plot = FALSE,
    ref.pvalue = TRUE)
  if (!is.null(tsmethod)) args$tsmethod <- tsmethod
  test <- do.call(Exact::exact.test, args)
  list(p = unname(test$p.value), output = unclass(test))
}

.ref_boschloo <- function(table, alternative, conf.level, variant) {
  .obs_boschloo_check_table(table)
  if (variant == "central") {
    # The base definition, min(1, 2 * min(p_less, p_greater)) with each side
    # maximised separately, from two one-sided calls of the original. Exact's
    # own tsmethod = "central" doubles the side in the observed direction and
    # gives 1 when the sample proportions are equal.
    sides <- lapply(c(less = "less", greater = "greater"), function(side) .ref_exact_p(table, "boschloo", side))
    p <- min(1, 2 * min(sides$less$p, sides$greater$p))
    output <- lapply(sides, `[[`, "output")
  } else {
    fit <- if (variant == "minlike") .ref_exact_p(table, "boschloo", "two.sided", "square") else
      .ref_exact_p(table, "boschloo", variant)
    p <- fit$p
    output <- fit$output
  }
  list(status = "ok", method = .bl_variant_info[[variant]]$label, table = table,
    n_total = sum(table), event_probability_difference = table[1, 1] / sum(table[1, ]) - table[2, 1] / sum(table[2, ]),
    null_difference = 0, p_value = p, alternative = alternative, confidence_interval = NULL,
    confidence_level = conf.level, interval_status = "not_computed",
    interval_method = "No sampling confidence interval is implemented by this example.",
    limitations = "Exact maximises over the nuisance parameter on a grid with refinement; no Fisher or asymptotic substitution.",
    reference_output = output)
}

.ref_boschloo_one_sided <- function(table, alternative = "greater", conf.level = 0.95) {
  .obs_boschloo_one_sided_check_settings(alternative, conf.level)
  .ref_boschloo(table, alternative, conf.level, alternative)
}

.ref_boschloo_two_sided_central <- function(table, alternative = "two.sided", conf.level = 0.95) {
  .obs_boschloo_two_sided_central_check_settings(alternative, conf.level)
  .ref_boschloo(table, alternative, conf.level, "central")
}

.ref_boschloo_two_sided_minlike <- function(table, alternative = "two.sided", conf.level = 0.95) {
  .obs_boschloo_two_sided_minlike_check_settings(alternative, conf.level)
  .ref_boschloo(table, alternative, conf.level, "minlike")
}

.ref_unconditional_exact_z_pooled <- function(table, alternative = "two.sided", conf.level = 0.95,
    independent_binomial_groups = FALSE, tolerance = 1e-08, max_subdivisions = 4096L) {
  .obs_unconditional_exact_z_pooled_check_settings(alternative, conf.level)
  .obs_unconditional_exact_z_pooled_check_input(table, independent_binomial_groups, tolerance, max_subdivisions)
  # Two-sided ordering by the squared statistic, as in the base engine.
  fit <- .ref_exact_p(table, "z-pooled", alternative, if (alternative == "two.sided") "square")
  p <- fit$p
  list(status = "ok", method = "Unconditional exact pooled-Z test", table = table, n_total = sum(table),
    alternative = alternative, null_difference = 0, statistic = unname(fit$output$statistic),
    event_probability_difference = table[1, 1] / sum(table[1, ]) - table[2, 1] / sum(table[2, ]),
    p_value = p, alpha = 1 - conf.level, confidence_interval = NULL, confidence_level = conf.level,
    interval_status = "not_computed", interval_method = "No sampling interval is implemented.",
    limitations = "Exact maximises over the nuisance parameter on a grid with refinement. Total N at most 30, as in the base engine.",
    reference_output = fit$output)
}
