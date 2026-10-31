# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.chisq_independence_rxc.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_chisq_independence_rxc_check_settings <- function(alternative, conf.level) {
    stopifnot(identical(alternative, "two.sided"), is.numeric(conf.level), length(conf.level) == 1L, 
        is.finite(conf.level), conf.level > 0, conf.level < 1)
}

.obs_chisq_independence_rxc_check_counts <- function(x) {
    stopifnot(is.numeric(x), length(x) > 1L, all(is.finite(x)), all(x >= 0), all(x == floor(x)), sum(x) > 
        0, sum(x) <= 2^31 - 1)
}

.obs_chisq_independence_rxc_analyse <- function(counts, alternative = "two.sided", conf.level = 0.95) {
    .obs_chisq_independence_rxc_check_settings(alternative, conf.level)
    .obs_chisq_independence_rxc_check_counts(counts)
    stopifnot(is.matrix(counts), nrow(counts) >= 2L, ncol(counts) >= 2L, all(rowSums(counts) > 0), all(colSums(counts) > 
        0))
    messages <- character()
    fit <- withCallingHandlers(stats::chisq.test(counts, correct = FALSE, simulate.p.value = FALSE), 
        warning = function(w) {
            messages <<- c(messages, conditionMessage(w))
            invokeRestart("muffleWarning")
        })
    list(method = fit$method, status = "ok", n = sum(counts), observed = counts, expected = fit$expected, 
        row_proportions = counts/rowSums(counts), statistic = unname(fit$statistic), df = unname(fit$parameter), 
        p_value = fit$p.value, cramer_v = sqrt(unname(fit$statistic)/(sum(counts) * min(dim(counts) - 
            1L))), pearson_residuals = fit$residuals, continuity_correction = FALSE, confidence_interval = NULL, 
        interval_status = "not_computed", requested_confidence_level = conf.level, alpha = 1 - conf.level, 
        reject = fit$p.value < 1 - conf.level, warnings = messages, assumptions = "Independent multinomial units; positive margins; chi-squared approximation. No automatic exact-test substitution for sparse cells.")
}

