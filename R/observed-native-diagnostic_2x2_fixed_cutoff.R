# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.diagnostic_2x2_fixed_cutoff.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_diagnostic_2x2_fixed_cutoff_analyse <- function(tp, fn, fp, tn, sampling, cutoff_origin, evaluation_split, 
    conf.level = 0.95, prevalence = NULL, prevalence_source = NULL) {
    stopifnot(is.character(sampling), length(sampling) == 1L, sampling %in% c("cross_sectional", "case_control", 
        "unknown"), is.character(cutoff_origin), length(cutoff_origin) == 1L, cutoff_origin %in% c("predeclared_rule", 
        "held_out_tuning"), is.character(evaluation_split), length(evaluation_split) == 1L, evaluation_split %in% 
        c("same_data", "held_out_test", "external"))
    if (is.null(prevalence)) {
        stopifnot(is.null(prevalence_source))
    }
    else {
        stopifnot(is.character(prevalence_source), length(prevalence_source) == 1L, prevalence_source %in% 
            c("assumed", "external"))
    }
    cells <- list(tp = tp, fn = fn, fp = fp, tn = tn)
    stopifnot(all(vapply(cells, function(v) is.numeric(v) && length(v) == 1L && is.finite(v) && v >= 
        0 && v == floor(v), logical(1))))
    if (cutoff_origin == "held_out_tuning" && evaluation_split == "same_data") 
        stop("A tuned cutoff needs untouched held-out or external evaluation data.")
    spec <- classification_spec(counts = unlist(cells), rule = cutoff_rule(cutoff = 1, selection_data = cutoff_origin), 
        models = classifier_model("test", score_meaning = "binary test decision: 1 = alert, 0 = no alert"), 
        direction = "higher_is_positive", comparator = ">=", evaluation_split = evaluation_split, sampling = sampling, 
        prevalence = prevalence, prevalence_source = prevalence_source, uncertainty = classification_uncertainty(level = conf.level, 
            proportion_methods = "clopper_pearson", auc_method = "none"))
    result <- evaluate_cutoff(spec)
    m <- result$models[[1]]
    list(method = "Fixed-cutoff diagnostic 2 x 2 counts", status = result$status$code, counts = unlist(cells), 
        sampling = sampling, cutoff_origin = cutoff_origin, evaluation_split = evaluation_split, metrics = m$metrics, 
        intervals = m$intervals, scenario = m$scenario, summaries = m$summaries, specification = spec, 
        result = result, limitations = "Independent units and a rule fixed before these evaluation outcomes were inspected must be justified. No ROC/PR curve, AUC or cutoff sweep can be inferred from four cells. External-prevalence values are scenarios under transportable class-conditional performance, not new observed counts. Marginal Clopper-Pearson intervals are conditional on the fixed rule, not adjusted for selection or clustering.")
}

