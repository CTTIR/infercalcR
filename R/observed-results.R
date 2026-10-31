# Deliberate views of finalized native outputs. No computation is delegated to reporting.
.ic_native_status <- function(status) {
  if (is.null(status)) return("ok")
  if (is.list(status)) status <- status$code
  if (!is.character(status) || length(status) != 1L || is.na(status)) return("unresolved")
  if (status %in% .ic_statuses) return(status)
  switch(status, unavailable = "not_applicable", numerical_limit = "numerical_failure",
    numerically_indeterminate = "unresolved", validation_incomplete = "unresolved",
    simulation_incomplete = "unresolved", stability_incomplete = "unresolved",
    insufficient_tail_resolution = "unresolved", no_rank_variation = "not_applicable", "unresolved")
}
.ic_scalar <- function(x) if (is.numeric(x) && length(x) == 1L && !is.nan(x)) as.double(x) else NA_real_
.ic_text <- function(x, default = NA_character_) if (is.character(x) && length(x) == 1L && !is.na(x)) x else default
.ic_first_number <- function(...) {
  for (x in list(...)) if (is.numeric(x) && length(x) == 1L && !is.na(x)) return(as.double(x))
  NA_real_
}
.ic_bind_rows <- function(rows) {
  columns <- unique(unlist(lapply(rows, names)))
  prototypes <- lapply(columns, function(nm) {
    i <- which(vapply(rows, function(x) nm %in% names(x), logical(1)))[1L]
    rows[[i]][[nm]]
  }); names(prototypes) <- columns
  rows <- lapply(rows, function(row) {
    for (nm in setdiff(columns, names(row))) row[[nm]] <- rep(if (is.character(prototypes[[nm]])) NA_character_ else NA_real_, nrow(row))
    row[columns]
  })
  do.call(rbind, rows)
}
.ic_native_result <- function(native, spec, args, warnings) {
  id <- spec$method_id
  state <- .ic_native_status(native$status)
  if (identical(native$decision, "unresolved") || identical(native$decision, "numerically_indeterminate")) state <- "unresolved"
  reason <- .ic_text(native$reason)
  n_input <- if (is.data.frame(args$data)) nrow(args$data) else if (is.data.frame(args$evaluation)) nrow(args$evaluation) else
    if (is.numeric(args$x)) length(args$x) + if (is.numeric(args$y)) length(args$y) else 0 else
    if (is.numeric(args$counts)) sum(args$counts) else if (is.numeric(args$table)) sum(args$table) else
    if (is.numeric(args$observed)) sum(args$observed) else .ic_scalar(args$n)
  n <- .ic_first_number(native$n_nonzero, native$n_discordant, native$n_used, native$n, native$n_complete, native$n_pairs,
    native$n_total, if (length(native$group_sizes)) sum(native$group_sizes), n_input)
  if (id %in% c("signed_rank_wilcoxon", "signed_rank_wilcoxon_one_sample") && is.numeric(native$n_zero)) n <- n_input - native$n_zero
  excluded <- .ic_first_number(native$n_excluded, if (is.finite(n_input) && is.finite(n)) n_input - n else 0)
  level <- .ic_first_number(native$attained_confidence_level, native$confidence_level,
    native$requested_confidence_level, spec$conf.level)
  alternative <- if (!is.null(.ic_method_info[[id]]) && !.ic_method_info[[id]]$hypothesis) NA_character_ else spec$alternative
  metadata <- list(method_id = id, settings = spec, source_status = native$status,
    status_reason = native$reason, warnings = unique(c(warnings, native$warnings, native$diagnostics[is.character(native$diagnostics)])),
    assumptions = native$assumptions, limitations = native$limitations,
    population = list(n_input = n_input, n_used = n, n_excluded = excluded),
    orientation = list(group_reference = native$group_reference, group_comparison = native$group_comparison,
      event = native$event, sign_convention = native$sign_convention, group_order = native$group_order),
    validation = "adapter preserves inventoried source procedure; see versioned validation evidence")
  if (grepl("^(classifier_|diagnostic_)", id) && !is.null(native$result)) {
    out <- as_inference(native$result)
    out$metadata$template_method_id <- id
    out$metadata$method_id <- id
    out$metadata$variant_id <- id
    out$metadata$settings <- spec
    out$metadata$limitations <- native$limitations
    # The converted classifier payload owns its established privacy policy.
    out$metadata$template_context <- native[intersect(names(native), c("sampling", "cutoff_origin", "evaluation_split", "prevalence", "count_kind"))]
    out$payload$classifier <- out$payload
    out$payload$native <- native[setdiff(names(native), c("specification", "result"))]
    out$metadata$payload_privacy <- "Template specification and attached source result omitted; aggregate finalized template fields retained; classifier conversion controls its raw-data-free payload."
    validate_inference(out)
    return(out)
  }
  rows <- list()
  add <- function(component, estimand, estimate = NA_real_, p = NA_real_, statistic = NA_real_, df = NA_real_,
      ci = NULL, interval_method = NA_character_, scale = "identity", scope = "pointwise",
      contrast = NA_character_, null = NA_real_, row_alternative = alternative, status = state, extra = list()) {
    has_ci <- is.numeric(ci) && length(ci) == 2L
    row <- data.frame(row_id = paste0(id, ":", length(rows) + 1L), component = component,
      estimand = estimand, contrast = contrast, estimate = .ic_scalar(estimate),
      statistic = .ic_scalar(statistic), df = .ic_scalar(df),
      conf.low = if (has_ci) as.double(ci[1L]) else NA_real_, conf.high = if (has_ci) as.double(ci[2L]) else NA_real_,
      conf.level = if (has_ci) level else NA_real_, interval.method = .ic_text(interval_method),
      interval.scale = scale, interval.scope = if (has_ci) scope else NA_character_,
      p.value = .ic_scalar(p), null.value = .ic_scalar(null), alternative = .ic_text(row_alternative),
      n = n, n.excluded = excluded, status = status, reason = reason, stringsAsFactors = FALSE)
    for (nm in names(extra)) row[[nm]] <- extra[[nm]]
    rows[[length(rows) + 1L]] <<- row
  }
  add_omnibus <- function(test, label = "omnibus hypothesis") {
    if (is.null(test)) return(invisible(NULL))
    add("omnibus", label, p = test$p_value, statistic = test$statistic,
      df = if (!is.null(test$df)) test$df else test$df_numerator,
      row_alternative = "two.sided", extra = list(df.numerator = .ic_scalar(test$df_numerator), df.denominator = .ic_scalar(test$df_denominator)))
  }
  add_coefficients <- function(tab, method, ratio = FALSE) {
    if (!is.data.frame(tab)) return(invisible(NULL))
    for (i in seq_len(nrow(tab))) {
      z <- tab[i, , drop = FALSE]
      term <- if ("term" %in% names(z)) as.character(z$term) else as.character(i)
      contrast <- if ("category" %in% names(z)) paste(z$category, "versus", z$reference, ":", term) else term
      add("coefficient", "model coefficient", z$estimate, z$p_value,
        if ("statistic" %in% names(z)) z$statistic else z$z, native$df,
        if (all(c("lower", "upper") %in% names(z))) c(z$lower, z$upper) else NULL,
        method, contrast = contrast, null = 0, row_alternative = "two.sided")
      if (ratio) {
        value <- .ic_first_number(z$ratio, z$odds_ratio, z$relative_odds)
        if (!is.na(value)) add("ratio", if (grepl("poisson|negative_binomial", id)) "event rate ratio" else "event odds ratio",
          value, z$p_value, ci = c(z$ratio_lower, z$ratio_upper), interval_method = method,
          scale = "ratio", contrast = contrast, null = 1, row_alternative = "two.sided")
      }
    }
  }
  scalar_fields <- c(t_mean_one_sample = "mean_difference", t_means_paired = "mean_difference",
    t_means_two_independent_student = "mean_difference", t_means_two_independent_welch = "mean_difference",
    binomial_exact_one_sample = "proportion", prop_one_sample_score = "proportion",
    prop_two_independent_pearson_chisq = "proportion_difference", fisher_exact_2x2 = "conditional_odds_ratio",
    sign_test_one_sample = "positive_probability", sign_test_paired = "positive_probability",
    signed_rank_wilcoxon = "pseudomedian", signed_rank_wilcoxon_one_sample = "pseudomedian",
    brunner_munzel = "estimate", mcnemar_exact_conditional = "conditional_positive_probability",
    mcnemar_midp = "conditional_positive_probability", variance_f_test_two = "variance_ratio",
    correlation_pearson = "estimate", correlation_spearman = "estimate", correlation_kendall = "estimate",
    regression_linear_simple = "estimate", glmm_logistic_random_intercept = "odds_ratio", gee_logistic_exchangeable = "odds_ratio")
  labels <- c(mean_difference = "mean difference (x minus reference or y)", proportion = "event probability",
    proportion_difference = "event probability difference (row 1 minus row 2)", conditional_odds_ratio = "conditional event odds ratio (row 1 / row 2)",
    positive_probability = "probability of a positive difference conditional on a nonzero difference",
    pseudomedian = "location pseudomedian under the symmetric shift model",
    conditional_positive_probability = "probability of x-only conditional on discordance",
    variance_ratio = "population variance ratio (x / y)", odds_ratio = "event odds ratio (comparison / reference)")
  if (id %in% names(scalar_fields)) {
    field <- scalar_fields[[id]]
    label <- if (field %in% names(labels)) labels[[field]] else switch(id,
      brunner_munzel = "P(X > Y) + 0.5 P(X = Y)", regression_linear_simple = "conditional mean slope",
      correlation_pearson = "Pearson correlation", correlation_spearman = "Spearman rank correlation", correlation_kendall = "Kendall tau")
    interval <- .ic_text(native$interval_method, switch(id,
      t_mean_one_sample = "Student t mean difference", t_means_paired = "paired Student t",
      t_means_two_independent_student = "pooled Student t", t_means_two_independent_welch = "Welch t",
      binomial_exact_one_sample = "Clopper-Pearson", prop_one_sample_score = "Wilson score",
      correlation_pearson = "Fisher z", regression_linear_simple = "normal-model t", variance_f_test_two = "exact F", NA_character_))
    null <- if (field %in% c("conditional_odds_ratio", "variance_ratio", "odds_ratio")) .ic_first_number(native$null_ratio, 1) else
      if (field %in% c("positive_probability", "conditional_positive_probability") || id == "brunner_munzel") .5 else
      if (field == "proportion") native$reference else if (field == "pseudomedian") native$null_location else 0
    add("contrast", label, native[[field]], native$p_value, native$statistic, native$df,
      native$confidence_interval, interval, scale = if (field %in% c("conditional_odds_ratio", "variance_ratio", "odds_ratio")) "ratio" else "identity",
      null = null, extra = list(df.denominator = if (length(native$df) == 2L) native$df[2L] else NA_real_))
  } else if (id == "rank_sum_mann_whitney") {
    add("rank_test", "identical group distributions; descriptive probability of superiority", native$probability_superiority,
      native$p_value, native$statistic, null = NA_real_)
    if (!is.null(native$shift_estimate)) add("location_shift", "x-minus-y location shift under the common-shape model",
      native$shift_estimate, ci = native$confidence_interval, interval_method = native$interval_method)
  } else if (id == "descriptive_summary") {
    if (native$measurement == "continuous") {
      add("description", "population mean", native$mean, ci = native$confidence_interval, interval_method = "normal-model t", row_alternative = NA_character_)
      add("description", "sample standard deviation", native$sd, row_alternative = NA_character_)
      for (i in 1:3) add("description", c("first quartile", "median", "third quartile")[i], native$quantiles[i], row_alternative = NA_character_)
    } else if (native$measurement == "binary") {
      add("description", "event probability", native$proportion, ci = native$confidence_interval,
        interval_method = "Clopper-Pearson", contrast = native$event, row_alternative = NA_character_)
    } else for (i in seq_len(nrow(native$categories))) {
      z <- native$categories[i, ]
      add("description", "category probability", z$proportion, ci = c(z$lower, z$upper),
        interval_method = "Clopper-Pearson", scope = "marginal, not simultaneous", contrast = z$category,
        row_alternative = NA_character_, extra = list(count = z$count))
    }
  } else if (id == "anova_factorial" && is.data.frame(native$terms)) {
    for (i in seq_len(nrow(native$terms))) {
      z <- native$terms[i, ]
      add("omnibus_term", "type-III sum-contrast factorial term", p = z$p_value, statistic = z$F, df = z$df,
        contrast = z$term, row_alternative = "two.sided", extra = list(p.adjusted = z$p_holm,
          p.reported = z$p_holm, p.method = "holm", family_id = "all_factorial_terms", family_size = nrow(native$terms),
          df.denominator = native$residual_df, partial.eta.squared = z$partial_eta_squared))
    }
    metadata$adjustment <- list(list(family_id = "all_factorial_terms", rows = vapply(rows, function(x) x$row_id, ""),
      size = nrow(native$terms), method = "holm", interval_adjustment = "not_computed"))
  } else if (id == "ancova_baseline_adjusted" && !is.null(native$group_test)) {
    add_omnibus(native$group_test, "adjusted equality of group conditional means")
    for (i in seq_len(nrow(native$adjusted_means))) {
      z <- native$adjusted_means[i, ]; add("adjusted_mean", "conditional mean at pooled covariate means", z$estimate,
        ci = c(z$lower, z$upper), interval_method = "normal-model t; marginal", contrast = z$group)
    }
    for (i in seq_len(nrow(native$differences))) {
      z <- native$differences[i, ]; add("contrast", "adjusted group mean difference", z$estimate, z$p_value, z$statistic,
        native$group_test$df_denominator, c(z$lower, z$upper), "normal-model t; unadjusted marginal", contrast = paste(z$group, "minus", z$reference), null = 0)
    }
  } else if (id %in% c("regression_linear_multiple", "regression_logistic_binary", "regression_poisson", "regression_negative_binomial", "regression_multinomial", "regression_ordinal_proportional_odds")) {
    if (!is.null(native$omnibus)) {
      omnibus <- native$omnibus; if (is.null(omnibus$p_value)) omnibus$p_value <- native$p_value
      add_omnibus(omnibus, "all prespecified slopes are zero")
    }
    add_omnibus(native$partial_test, "prespecified subset of slopes is jointly zero")
    tab <- if (is.data.frame(native$coefficient_table)) native$coefficient_table else native$coefficient_table_two_sided
    add_coefficients(tab, .ic_text(native$interval_method, "normal-model marginal t"), ratio = id != "regression_linear_multiple")
  } else if (grepl("^lmm_", id)) {
    add("omnibus", "fixed-factor mean equality", p = native$p_value, statistic = native$statistic,
      df = native$df_numerator, extra = list(df.denominator = .ic_scalar(native$df_denominator)))
    if (is.data.frame(native$factor_means)) for (i in seq_len(nrow(native$factor_means))) {
      z <- native$factor_means[i, ]; add("factor_mean", "marginal factor-level mean", z$estimate,
        df = native$mean_interval_df, ci = c(z$lower, z$upper), interval_method = native$mean_interval_method,
        contrast = z$level, row_alternative = NA_character_)
    }
  } else if (grepl("^prediction_model_", id)) {
    for (component in c("apparent", "corrected")) if (is.numeric(native[[component]])) for (metric in names(native[[component]])) {
      add(component, paste(component, metric), native[[component]][[metric]], row_alternative = NA_character_,
        extra = list(monte.carlo.se = if (component == "corrected") .ic_scalar(native$monte_carlo_se[[metric]]) else NA_real_))
    }
  } else if (id == "pca_components" && is.numeric(native$eigenvalues)) {
    for (i in seq_along(native$eigenvalues)) add("component_variance", "principal component variance", native$eigenvalues[i],
      contrast = paste0("PC", i), row_alternative = NA_character_, extra = list(variance.proportion = native$variance_proportion[i]))
  } else if (id == "efa_common_factor_ml" && is.matrix(native$loadings)) {
    for (j in seq_len(ncol(native$loadings))) for (i in seq_len(nrow(native$loadings))) add("loading", "oblique factor pattern loading",
      native$loadings[i, j], contrast = paste(rownames(native$loadings)[i], colnames(native$loadings)[j], sep = ":"), row_alternative = NA_character_)
    add_omnibus(native$goodness_of_fit, "normal-theory goodness of fit for the prespecified factor count")
  } else if (id == "efa_common_factor_ml.parallel_analysis" && is.numeric(native$observed_eigenvalues)) {
    for (i in seq_along(native$observed_eigenvalues)) add("parallel_analysis", "SMC common-factor eigenvalue", native$observed_eigenvalues[i],
      contrast = paste0("dimension", i), row_alternative = NA_character_, extra = list(null.threshold = native$null_thresholds[i]))
  } else if (grepl("^cluster_", id)) {
    add("description", "mean silhouette", native$mean_silhouette, row_alternative = NA_character_)
    add("stability", "mean adjusted Rand index across all subsamples", native$stability$mean, row_alternative = NA_character_)
  } else {
    # These listed methods have a genuine omnibus/decision test, not a pairwise effect CI.
    omnibus_ids <- c("anova_one_way", "anova_one_way_welch", "anova_repeated_one_way_gg", "kruskal_wallis", "friedman", "cochran_q",
      "cochran_armitage_trend", "jonckheere_terpstra", "chisq_goodness_of_fit", "chisq_goodness_of_fit_mc", "chisq_independence_rxc",
      "fisher_exact_rxc", "mcnemar_asymptotic_cc", "mcnemar_asymptotic_uncorrected", "bowker_symmetry",
      "stuart_maxwell_marginal_homogeneity", "variance_brown_forsythe", "variance_fligner_killeen", "unconditional_exact_z_pooled")
    if (id %in% omnibus_ids || grepl("^boschloo_", id)) {
      add("omnibus", .ic_text(native$null, .ic_text(native$method, id)), p = native$p_value, statistic = native$statistic,
        df = if (is.numeric(native$df) && length(native$df)) native$df[1L] else native$df_numerator,
        extra = list(df.denominator = .ic_first_number(native$df_denominator, if (length(native$df) == 2L) native$df[2L]),
          p.lower = .ic_scalar(native$p_lower), p.upper = .ic_scalar(native$p_upper), decision = .ic_text(native$decision)))
      if (!is.null(native$event_probability_difference)) add("description", "observed event probability difference (row 1 minus row 2)", native$event_probability_difference)
    }
  }
  if (!length(rows)) {
    if (state == "ok") .ic_abort(paste("No common view implemented for successful", id), "unsupported")
    add("unavailable", id, status = state)
  }
  kind <- if (id == "descriptive_summary") "descriptive" else "inference"
  new_inference_result(.ic_bind_rows(rows), metadata, kind = kind, payload = list(native = native))
}
