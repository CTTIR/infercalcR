# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.classifier_binormal_theory.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_classifier_binormal_theory_analyse <- function(auc, sigma, cutoff, prevalence, n_total) {
    stopifnot(is.numeric(cutoff), length(cutoff) == 1L, is.finite(cutoff))
    spec <- classification_spec(theory = list(auc = auc, sigma = sigma, n_total = n_total), cutoff = cutoff, 
        prevalence = prevalence, prevalence_source = "assumed", models = classifier_model("binormal", 
            name = "Hypothetical binormal model", score_meaning = "score standardised to the non-event distribution"), 
        direction = "higher_is_positive", comparator = ">=", uncertainty = classification_uncertainty(proportion_methods = character(), 
            auc_method = "none"))
    result <- evaluate_cutoff(spec, include_curves = FALSE)
    m <- result$models[[1]]
    list(method = "Hypothetical binormal cutoff scenario", status = result$status$code, model = binormal_model(auc, 
        sigma), cutoff = cutoff, analytic_rates = binormal_rates(cutoff, auc, sigma), prevalence = prevalence, 
        n_total = n_total, count_kind = m$count_kind, operating_point = m$operating_point, metrics = m$metrics, 
        summaries = m$summaries, confidence_interval = NULL, interval_status = "not_applicable", specification = spec, 
        result = result, limitations = "Assumed score laws and prevalence, not observed performance or external validation. No sampling interval is inferred from expected counts. Changing N scales expected counts without changing analytic rates. Class-conditional rates remain defined at prevalence endpoints even when count-based conditional metrics have zero denominators. No classifier training, cutoff optimisation or sample-size planning is performed.")
}

