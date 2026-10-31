# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.signed_rank_wilcoxon_one_sample.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_signed_rank_wilcoxon_one_sample_check_settings <- function(alternative, conf.level, exact) {
    stopifnot(is.character(alternative), length(alternative) == 1L, alternative %in% c("two.sided", "greater", 
        "less"), is.numeric(conf.level), length(conf.level) == 1L, is.finite(conf.level), conf.level > 
        0, conf.level < 1, is.logical(exact), length(exact) == 1L, !is.na(exact))
}

.obs_signed_rank_wilcoxon_one_sample_check_vector <- function(x) stopifnot(is.numeric(x), is.null(dim(x)), 
    length(x) > 0L, all(is.finite(x)))

.obs_signed_rank_wilcoxon_one_sample_signed_analysis <- function(x, mu, alternative, conf.level, exact) {
    .obs_signed_rank_wilcoxon_one_sample_check_settings(alternative, conf.level, exact)
    .obs_signed_rank_wilcoxon_one_sample_check_vector(x)
    stopifnot(is.numeric(mu), length(mu) == 1L, is.finite(mu))
    d <- x - mu
    stopifnot(all(is.finite(d)))
    zeros <- sum(d == 0)
    ties <- anyDuplicated(abs(d[d != 0])) > 0L
    if (exact) 
        stopifnot(zeros == 0L, !ties, length(x) <= 30L)
    if (all(d == 0)) 
        return(list(status = "not_applicable", reason = "All differences are zero; no informative ranks.", 
            p_value = NULL, confidence_interval = NULL, n_total = length(d), n_zero = zeros))
    messages <- character()
    fit <- withCallingHandlers(stats::wilcox.test(x[d != 0], mu = mu, alternative = alternative, exact = exact, 
        correct = TRUE, conf.int = TRUE, conf.level = conf.level, digits.rank = Inf), warning = function(w) {
        messages <<- c(messages, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
    list(status = if (is.finite(fit$p.value)) "ok" else "undefined", method = fit$method, n_total = length(x), 
        n_zero = zeros, tied_absolute_ranks = ties, statistic = unname(fit$statistic), p_value = fit$p.value, 
        alternative = alternative, null_location = mu, pseudomedian = unname(fit$estimate), confidence_interval = as.numeric(fit$conf.int), 
        requested_confidence_level = conf.level, attained_confidence_level = attr(fit$conf.int, "conf.level"), 
        interval_method = "Inversion for the location/pseudomedian under the symmetric shift model", 
        exact = exact, diagnostics = messages, limitations = "Symmetry and interval scale must be justified externally; zeros are omitted from signed ranks in approximation mode. A location interval is not a mean interval.")
}

.obs_signed_rank_wilcoxon_one_sample_analyse <- function(x, mu, alternative = "two.sided", conf.level = 0.95, 
    exact = TRUE) {
    .obs_signed_rank_wilcoxon_one_sample_signed_analysis(x, mu, alternative, conf.level, exact)
}

