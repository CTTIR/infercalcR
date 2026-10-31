# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.mcnemar_asymptotic_cc.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_mcnemar_asymptotic_cc_check_settings <- function(alternative, conf.level) {
    stopifnot(identical(alternative, "two.sided"), is.numeric(conf.level), length(conf.level) == 1L, 
        is.finite(conf.level), conf.level > 0, conf.level < 1)
}

.obs_mcnemar_asymptotic_cc_check_counts <- function(x) {
    stopifnot(is.numeric(x), length(x) > 1L, all(is.finite(x)), all(x >= 0), all(x == floor(x)), sum(x) > 
        0, sum(x) <= 2^31 - 1)
}

.obs_mcnemar_asymptotic_cc_analyse <- function(counts, alternative = "two.sided", conf.level = 0.95) {
    .obs_mcnemar_asymptotic_cc_check_settings(alternative, conf.level)
    .obs_mcnemar_asymptotic_cc_check_counts(counts)
    stopifnot(is.matrix(counts), identical(dim(counts), c(2L, 2L)))
    b <- counts[1, 2]
    c <- counts[2, 1]
    discordant <- b + c
    if (discordant == 0) 
        return(list(status = "not_applicable", reason = "No discordant pairs; the asymptotic statistic is undefined.", 
            n = sum(counts), discordant_pairs = 0, marginal_difference = 0, p_value = NULL, confidence_interval = NULL, 
            interval_status = "not_computed"))
    fit <- stats::mcnemar.test(counts, correct = TRUE)
    list(method = fit$method, status = "ok", n = sum(counts), observed = counts, discordant_pairs = discordant, 
        marginal_difference = (b - c)/sum(counts), statistic = unname(fit$statistic), df = 1, p_value = fit$p.value, 
        continuity_correction = TRUE, confidence_interval = NULL, interval_status = "not_computed", requested_confidence_level = conf.level, 
        alpha = 1 - conf.level, reject = fit$p.value < 1 - conf.level, warnings = if (discordant < 25) "Few discordant pairs; chi-squared approximation may be inaccurate." else character(), 
        assumptions = "Independent pairs; paired binary outcomes with fixed coding. No automatic conditional or mid-p substitution.")
}

