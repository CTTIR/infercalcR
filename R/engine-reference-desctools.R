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

# Run code with a fixed seed under the base engines' RNG kinds, restoring the
# caller's RNG state afterwards.
.ref_with_seed <- function(seed, code) {
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv) else
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(as.integer(seed))
  code
}

.ref_cochran_armitage_trend <- function(events, totals, scores, scores_prespecified = FALSE,
    alternative = "two.sided", conf.level = 0.95) {
  n <- .obs_cochran_armitage_trend_check(events, totals, scores, scores_prespecified, alternative, conf.level)
  method <- "Cochran-Armitage asymptotic trend test"
  probability <- sum(events) / n
  if (probability == 0 || probability == 1)
    return(list(method = method, status = "unavailable", reason = "No event variation: the binomial score variance is zero.",
      p_value = NULL, confidence_interval = NULL))
  # The original reads its scores from the column names of the table, so the
  # prespecified scores are passed there, never the group names. Events in the
  # first row give Z the base engine's sign: positive for increasing event probability.
  table <- matrix(c(events, totals - events), nrow = 2L, byrow = TRUE,
    dimnames = list(c("event", "non_event"), format(unname(scores), digits = 17L, trim = TRUE)))
  test <- DescTools::CochranArmitageTest(table)
  z <- unname(test$statistic)
  # DescTools' "one.sided" p is the tail in the observed direction. A prespecified
  # direction therefore uses the normal tail of the original's Z.
  p <- switch(alternative, two.sided = test$p.value, greater = stats::pnorm(z, lower.tail = FALSE),
    less = stats::pnorm(z))
  list(method = method, status = "ok", events = events, totals = totals, scores = scores, scores_prespecified = TRUE,
    event_proportions = events / totals, pooled_event_probability = probability, signed_statistic = z,
    statistic = z^2, df = 1L, p_value = p, alternative = alternative,
    direction = "greater means increasing event probability across increasing prespecified scores",
    confidence_interval = NULL, interval_status = "not_computed", requested_confidence_level = conf.level,
    expected_events = totals * probability, expected_non_events = totals * (1 - probability),
    alpha = 1 - conf.level, reject = p < 1 - conf.level,
    assumptions = "Independent binomial units with fixed group sizes and prespecified ordered scores. Asymptotic normal/chi-squared reference; sparse counts can make it inaccurate. No exact, continuity-corrected or automatic fallback. Scores are not fitted from outcomes. This is a trend score test, not an estimated common risk difference or logistic slope interval.",
    reference_output = unclass(test))
}

.ref_jonckheere_terpstra <- function(data, group_order, order_prespecified, independent_units, seed,
    alternative = "two.sided", B = 1999L, conf.level = 0.95, mc.level = 0.99, complete_cases = FALSE) {
  prepared <- .obs_jonckheere_terpstra_prepare(data, group_order, order_prespecified, independent_units,
    seed, alternative, B, conf.level, mc.level, complete_cases)
  y <- prepared$y
  common <- list(method = "Jonckheere-Terpstra ordered-group Monte Carlo permutation test", group_order = group_order,
    group_sizes = stats::setNames(prepared$counts, group_order), order_prespecified = TRUE,
    n_total = prepared$n_total, n_used = length(y), n_excluded = sum(!prepared$keep), independent_units = TRUE,
    alternative = alternative, seed = seed, requested_replicates = B, confidence_interval = NULL,
    interval_status = "not_computed", alpha = 1 - conf.level,
    assumptions = "Independent units and exchangeability under identical group distributions; group order and direction prespecified. Complete-case inference requires justified missingness. Concordance is descriptive and pair-weighted; this is not a mean, median, or causal-effect test.")
  if (length(unique(y)) == 1L) return(c(common, list(status = "no_rank_variation", p_value = 1, reject = FALSE)))
  groups <- factor(group_order[prepared$g], levels = group_order, ordered = TRUE)
  # Always the permutation route, as in DescrTab2 (nperm = 1000): the exact
  # route of DescTools 0.99.60 returns negative p-values.
  # DescTools 0.99.60 warns "p-value based on normal approximation" for tied or
  # large samples even when nperm is given and the p-value is a permutation
  # p-value; that one warning would be false here and is muffled.
  test <- withCallingHandlers(.ref_with_seed(seed, DescTools::JonckheereTerpstraTest(y, groups,
    alternative = switch(alternative, two.sided = "two.sided", greater = "increasing", less = "decreasing"),
    nperm = B)), warning = function(w) {
      if (grepl("Specify nperm for permutation p-value", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning")
    })
  # DescTools doubles the smaller tail before capping, so heavily tied outcomes
  # can give a two-sided p above 1; a p-value is capped at 1.
  p <- min(1, test$p.value)
  c(common, list(status = "ok", statistic = unname(test$statistic), p_value = p, reject = p < 1 - conf.level,
    inference_note = "Greater means increasing outcomes along group_order; less means decreasing. DescTools permutation p-value from B permutations; two-sided doubles the smaller tail.",
    reference_output = unclass(test)))
}

.ref_stuart_maxwell_marginal_homogeneity <- function(counts, alternative = "two.sided", conf.level = 0.95,
    n_excluded = 0L) {
  .obs_stuart_maxwell_marginal_homogeneity_check_table(counts, alternative, conf.level)
  .obs_stuart_maxwell_marginal_homogeneity_check_excluded(n_excluded)
  n <- sum(counts)
  # The original silently removes categories without off-diagonal counts, which
  # changes the degrees of freedom; the base contract allows no automatic
  # category removal or pooling.
  if (any(diag(counts) == rowSums(counts) & diag(counts) == colSums(counts)))
    return(list(status = "unavailable", p_value = NULL,
      reason = "A category has no off-diagonal counts; no automatic category removal, pooling or generalized inverse."))
  test <- tryCatch(DescTools::StuartMaxwellTest(counts), error = function(e) NULL)
  if (is.null(test) || !is.finite(test$statistic))
    return(list(status = "unavailable", p_value = NULL,
      reason = "Singular or numerically unresolved marginal covariance in the original; no automatic category pooling or generalized inverse."))
  p <- test$p.value
  list(method = "stuart_maxwell_marginal_homogeneity", status = "ok", n_pairs = n, n_excluded = n_excluded,
    categories = rownames(counts), null = "Equal marginal category distributions at the two occasions",
    observed = counts, row_proportions = rowSums(counts) / n, column_proportions = colSums(counts) / n,
    statistic = unname(test$statistic), df = unname(test$parameter), p_value = p, confidence_interval = NULL,
    interval_status = "not_computed", requested_confidence_level = conf.level, alpha = 1 - conf.level,
    reject = p < 1 - conf.level,
    assumptions = "Independent matched participants; identical category meaning/order; chi-squared large-sample approximation. Symmetry and marginal homogeneity are different hypotheses; no automatic substitution.",
    reference_output = unclass(test))
}

.ref_variance_brown_forsythe <- function(x, group, alternative = "two.sided", conf.level = 0.95) {
  .obs_variance_brown_forsythe_check_settings(alternative, conf.level, TRUE)
  g <- .obs_variance_brown_forsythe_check_groups(x, group)
  test <- DescTools::LeveneTest(x, g, center = stats::median)
  statistic <- test[["F value"]][[1L]]
  p <- test[["Pr(>F)"]][[1L]]
  if (!is.finite(statistic) || !is.finite(p))
    return(list(status = "not_applicable", p_value = NULL,
      reason = "The absolute deviations have no resolved within-group variation.", confidence_interval = NULL,
      interval_status = "not_computed"))
  list(method = "Brown-Forsythe (median-centred Levene)", status = "ok", group_sizes = table(g),
    group_medians = tapply(x, g, stats::median), statistic = statistic,
    df = as.numeric(test[["Df"]][1:2]), p_value = p, confidence_interval = NULL, interval_status = "not_computed",
    requested_confidence_level = conf.level, alpha = 1 - conf.level, reject = p < 1 - conf.level,
    assumptions = "Independent units and groups; approximate F reference for median-centred absolute deviations. Different distribution shapes affect the interpretation as a pure spread comparison.",
    reference_output = unclass(test))
}
