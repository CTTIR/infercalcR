# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.fisher_exact_rxc.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_fisher_exact_rxc_analyse <- function(counts, alternative = "two.sided", conf.level = 0.95) {
    stopifnot(is.numeric(counts), is.matrix(counts), all(dim(counts) >= 2L), all(dim(counts) <= 5L), 
        all(is.finite(counts)), all(counts >= 0), all(counts == floor(counts)), sum(counts) <= 100, all(rowSums(counts) > 
            0), all(colSums(counts) > 0), identical(alternative, "two.sided"), is.numeric(conf.level), 
        length(conf.level) == 1L, is.finite(conf.level), conf.level > 0, conf.level < 1)
    warnings <- character()
    fit <- tryCatch(withCallingHandlers(stats::fisher.test(counts, alternative = "two.sided", conf.int = FALSE, 
        simulate.p.value = FALSE, hybrid = FALSE, workspace = 200000L), warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
    }), error = function(e) e)
    if (inherits(fit, "error")) 
        return(list(method = "Fisher fixed-margin exact test", status = "unavailable", reason = conditionMessage(fit), 
            observed = counts, p_value = NULL, note = "Exact calculation failed within the fixed workspace; no simulation or asymptotic substitution was used."))
    list(method = fit$method, status = "ok", observed = counts, n = sum(counts), row_totals = rowSums(counts), 
        column_totals = colSums(counts), null = "Row and column independence conditional on both margins", 
        ordering = "Sum of probabilities of tables no more probable than the observed table", p_value = fit$p.value, 
        confidence_interval = NULL, interval_status = "not_computed", requested_confidence_level = conf.level, 
        alpha = 1 - conf.level, reject = fit$p.value < 1 - conf.level, warnings = warnings, assumptions = "Independent units; prespecified mutually exclusive categories; fixed-margin conditional inference.")
}

