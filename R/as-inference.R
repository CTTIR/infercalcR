# Explicit conversions: htest values are read as stored; model summaries are finalized once here.
#' @export
as_inference.htest <- function(x, method_id = NULL, n = NA_real_, population,
    groups = NULL, event = NULL, n_excluded = NA_real_, ...) {
  if (length(list(...))) .ic_abort("unknown htest conversion arguments")
  if (missing(population) || !.ic_string(population)) .ic_abort("Declare the analysis population when converting an htest object")
  if (!is.numeric(n) || !length(n) || any(is.nan(n)) || any(!is.na(n) & (!is.finite(n) | n < 0 | n != floor(n)))) .ic_abort("n must contain known sample counts or explicit NA")
  if (!is.numeric(n_excluded) || length(n_excluded) != 1L || is.nan(n_excluded) || (!is.na(n_excluded) && (!is.finite(n_excluded) || n_excluded < 0 || n_excluded != floor(n_excluded)))) .ic_abort("n_excluded must be one known count or explicit NA")
  label <- x$method
  if (!.ic_string(label)) .ic_abort("htest method label is missing")
  is_t <- grepl("t-test", label, fixed = TRUE) && !grepl("Wilcoxon", label)
  is_wilcox <- grepl("Wilcoxon", label)
  is_binom <- grepl("binomial test", label, ignore.case = TRUE)
  is_prop <- grepl("proportions", label, ignore.case = TRUE)
  is_fisher <- grepl("Fisher", label)
  is_cor <- grepl("correlation|Kendall", label)
  is_var <- grepl("F test to compare two variances", label, fixed = TRUE)
  effectless_fisher <- is_fisher && !length(x$estimate)
  omnibus_prop <- is_prop && length(x$estimate) > 2L
  is_omnibus <- effectless_fisher || omnibus_prop || grepl("Pearson's Chi-squared|McNemar|Kruskal-Wallis|Friedman|Fligner-Killeen|One-way analysis of means", label)
  if (!any(c(is_t, is_wilcox, is_binom, is_prop, is_fisher, is_cor, is_var, is_omnibus))) .ic_abort("No explicit htest mapping for this method", "unsupported")
  inferred <- if (is_t) if (grepl("Welch", label)) "t_means_two_independent_welch" else if (grepl("Paired", label)) "t_means_paired" else if (length(x$estimate) == 2L) "t_means_two_independent_student" else "t_mean_one_sample" else
    if (is_wilcox) if (grepl("rank sum", label)) "rank_sum_mann_whitney" else "signed_rank_wilcoxon_one_sample" else
    if (is_binom) "binomial_exact_one_sample" else if (is_prop) "imported_proportion_test" else if (is_fisher) if (length(x$estimate)) "fisher_exact_2x2" else "fisher_exact_rxc" else
    if (is_cor) if (grepl("Pearson", label)) "correlation_pearson" else if (grepl("Spearman", label)) "correlation_spearman" else "correlation_kendall" else
    if (is_var) "variance_f_test_two" else "imported_omnibus_test"
  if (!is.null(method_id) && !identical(method_id, inferred)) .ic_abort("method_id conflicts with the stored htest method; conversion cannot relabel a procedure")
  estimate <- NA_real_; estimand <- "omnibus hypothesis"; null <- .ic_scalar(x$null.value)
  scale <- "identity"; interval <- NA_character_; contrast <- NA_character_
  if (is_t) {
    if (length(x$estimate) == 2L) {
      if (!is.character(groups) || length(groups) != 2L || anyNA(groups) || any(!nzchar(groups)) || anyDuplicated(groups)) .ic_abort("Declare two ordered group labels for a two-sample mean difference")
      estimate <- unname(x$estimate[1L] - x$estimate[2L]); estimand <- "population mean difference"; contrast <- paste(groups, collapse = " minus ")
    } else { estimate <- .ic_scalar(x$estimate); estimand <- if (grepl("Paired", label)) "population mean paired difference" else "population mean" }
    interval <- if (grepl("Welch", label)) "Welch t" else "Student t"
  } else if (is_wilcox) {
    if (grepl("rank sum", label)) {
      if (!is.character(groups) || length(groups) != 2L || anyNA(groups) || any(!nzchar(groups)) || anyDuplicated(groups)) .ic_abort("Declare two ordered groups for the location contrast")
      contrast <- paste(groups, collapse = " minus ")
    }
    estimate <- .ic_scalar(x$estimate); estimand <- if (grepl("rank sum", label)) "location shift under common-shape shift model" else "location pseudomedian under symmetric shift model"
    interval <- "Wilcoxon inversion for location; not a general median difference"
  } else if (omnibus_prop) {
    if (!.ic_string(event)) .ic_abort("Declare the event definition for a proportions object")
    estimand <- "equality of event probabilities across groups"
  } else if (is_binom || is_prop || (is_fisher && !effectless_fisher)) {
    if (!.ic_string(event)) .ic_abort("Declare the event definition for a proportion or odds-ratio object")
    if (length(x$estimate) == 2L) {
      if (!is.character(groups) || length(groups) != 2L || anyNA(groups) || any(!nzchar(groups)) || anyDuplicated(groups)) .ic_abort("Declare two ordered group labels for the proportion contrast")
      estimate <- unname(x$estimate[1L] - x$estimate[2L]); estimand <- "event probability difference"; contrast <- paste(groups, collapse = " minus ")
    } else { estimate <- .ic_scalar(x$estimate); estimand <- if (is_fisher && length(x$estimate)) "conditional event odds ratio" else "event probability" }
    if (is_fisher) {
      if (!is.character(groups) || length(groups) != 2L || anyNA(groups) || any(!nzchar(groups)) || anyDuplicated(groups)) .ic_abort("Declare two ordered groups for the odds ratio")
      contrast <- paste(groups, collapse = " / ")
    }
    scale <- if (is_fisher && length(x$estimate)) "ratio" else "probability"
    interval <- if (is_binom) "Clopper-Pearson" else if (is_fisher) "exact conditional odds ratio" else "stats::prop.test interval; correction retained in stored method"
  } else if (is_cor) { estimate <- .ic_scalar(x$estimate); estimand <- names(x$estimate)[1L]; interval <- if (grepl("Pearson", label)) "Fisher z" else "not_provided_by_method" }
  else if (is_var) {
    if (!is.character(groups) || length(groups) != 2L || anyNA(groups) || any(!nzchar(groups)) || anyDuplicated(groups)) .ic_abort("Declare two ordered groups for the variance ratio")
    contrast <- paste(groups, collapse = " / ")
    estimate <- .ic_scalar(x$estimate); estimand <- "population variance ratio"; interval <- "exact F"; scale <- "ratio" }
  ci <- x$conf.int
  has_ci <- is.numeric(ci) && length(ci) == 2L
  status <- if (is.numeric(x$p.value) && length(x$p.value) == 1L && is.finite(x$p.value)) "ok" else "undefined"
  d <- data.frame(row_id = paste0(inferred, ":1"), component = if (is_omnibus) "omnibus" else "contrast",
    estimand = .ic_text(estimand), contrast = contrast, estimate = estimate,
    statistic = .ic_scalar(x$statistic), df = .ic_scalar(x$parameter),
    p.value = if (status == "ok") x$p.value else NA_real_, null.value = null,
    alternative = .ic_text(x$alternative), conf.low = if (has_ci) as.double(ci[1L]) else NA_real_,
    conf.high = if (has_ci) as.double(ci[2L]) else NA_real_, conf.level = if (has_ci) .ic_scalar(attr(ci, "conf.level")) else NA_real_,
    interval.method = interval, interval.scope = if (has_ci) "pointwise" else NA_character_, interval.scale = scale,
    n = if (anyNA(n)) NA_real_ else sum(n), n.excluded = .ic_scalar(n_excluded), status = status, stringsAsFactors = FALSE)
  new_inference_result(d, list(method_id = inferred, imported = TRUE, source_method = label,
    population = population, group_sizes = n, groups = groups, event = event,
    unknown_metadata = "Sample/exclusion counts and populations are never reconstructed from degrees of freedom.",
    validation = "conversion of supplied finalized htest"), payload = list(native = x))
}

.ic_model_conversion <- function(x, conf.level, population, glm = FALSE) {
  if (!is.numeric(conf.level) || length(conf.level) != 1L || !is.finite(conf.level) || conf.level <= 0 || conf.level >= 1) .ic_abort("conf.level must be in (0, 1)")
  if (!.ic_string(population)) .ic_abort("Declare the model analysis population")
  if (glm) {
    fam <- x$family$family; link <- x$family$link
    if (!fam %in% c("binomial", "poisson", "gaussian") ||
        !paste(fam, link) %in% c("binomial logit", "poisson log", "gaussian identity")) .ic_abort("Only binomial-logit, Poisson-log and Gaussian-identity glm conversion is implemented", "unsupported")
    if (!isTRUE(x$converged) || isTRUE(x$boundary)) .ic_abort("The supplied glm did not converge to an interior fit", "numerical_failure")
  }
  sm <- summary(x)
  table <- sm$coefficients
  df <- stats::df.residual(x)
  is_normal <- !glm || identical(x$family$family, "gaussian")
  critical <- if (is_normal) stats::qt((1-conf.level)/2, df, lower.tail=FALSE) else stats::qnorm((1-conf.level)/2, lower.tail=FALSE)
  all_terms <- names(stats::coef(x)); rows <- list()
  for (term in all_terms) {
    available <- term %in% rownames(table)
    z <- if (available) table[term, ] else rep(NA_real_, 4)
    state <- if (available && all(is.finite(z))) "ok" else "undefined"
    ci <- z[1L] + c(-1,1)*critical*z[2L]
    rows[[length(rows)+1L]] <- data.frame(row_id = paste0("coefficient:", length(rows)+1L), component = "coefficient",
      estimand = if (glm) paste(x$family$link, "scale coefficient") else "conditional mean coefficient", contrast = term,
      estimate = z[1L], statistic = z[3L], df = if (is_normal) df else NA_real_, p.value = if (is.finite(z[4L])) z[4L] else NA_real_,
      conf.low = ci[1L], conf.high = ci[2L], conf.level = conf.level,
      interval.method = if (is_normal) "model-based marginal t" else "model-based marginal Wald normal",
      interval.scope = "pointwise", interval.scale = if (glm) "link" else "outcome", null.value = 0,
      alternative = "two.sided", n = as.double(stats::nobs(x)), status = state,
      reason = if (state == "ok") NA_character_ else "aliased or numerically unresolved coefficient", stringsAsFactors = FALSE)
    if (glm && fam %in% c("binomial", "poisson") && term != "(Intercept)" && state == "ok") {
      ratio <- exp(c(z[1L], ci)); resolved <- all(is.finite(ratio)) && all(ratio > 0)
      rr <- rows[[length(rows)]]
      rr$row_id <- paste0("ratio:", length(rows)+1L); rr$component <- "ratio"
      rr$estimand <- if (fam == "binomial") "event odds ratio" else "event rate ratio"
      rr$estimate <- if (resolved) ratio[1L] else NA_real_; rr$conf.low <- if (resolved) ratio[2L] else NA_real_; rr$conf.high <- if (resolved) ratio[3L] else NA_real_
      rr$null.value <- 1; rr$interval.scale <- "ratio"
      if (!resolved) { rr$status <- "numerical_failure"; rr$reason <- "Finite link quantities overflow or underflow on exponentiation" }
      rows[[length(rows)+1L]] <- rr
    }
  }
  new_inference_result(do.call(rbind,rows), list(method_id = if (glm) paste0("imported_glm_",fam,"_",link) else "imported_lm",
    population = population, covariance = "model-based; no robust substitution", interval_policy = if (is_normal) "marginal t" else "explicit marginal Wald; not profile likelihood",
    event = if (glm && fam == "binomial") "success coding retained from the supplied fitted model" else NULL,
    validation = "conversion summarizes existing fitted model; no refit", source_call = paste(deparse(x$call),collapse=" ")),
    payload = list(coefficients = stats::coef(x), covariance = stats::vcov(x), residual_df = df,
      family = if (glm) list(family = fam, link = link) else "Gaussian linear model"))
}
#' @export
as_inference.lm <- function(x, conf.level = .95, population, ...) {
  if (length(list(...))) .ic_abort("unknown lm conversion arguments")
  if (missing(population)) .ic_abort("Declare the model analysis population")
  .ic_model_conversion(x, conf.level, population, FALSE)
}
#' @export
as_inference.glm <- function(x, conf.level = .95, population, interval = "wald", ...) {
  if (length(list(...))) .ic_abort("unknown glm conversion arguments")
  if (missing(population)) .ic_abort("Declare the model analysis population")
  if (!identical(interval, "wald")) .ic_abort("This fitted-glm conversion supports explicitly selected Wald intervals; use the logistic procedure for profile intervals", "unsupported")
  .ic_model_conversion(x, conf.level, population, TRUE)
}
