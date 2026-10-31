# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.t_mean_one_sample.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_t_mean_one_sample_check_settings <- function(alternative, conf.level) {
    stopifnot(length(alternative) == 1L, alternative %in% c("two.sided", "greater", "less"), is.numeric(conf.level), 
        length(conf.level) == 1L, is.finite(conf.level), conf.level > 0, conf.level < 1)
}

.obs_t_mean_one_sample_analyse <- function(x, mu0, alternative = "two.sided", conf.level = 0.95) {
    .obs_t_mean_one_sample_check_settings(alternative, conf.level)
    stopifnot(is.numeric(x), length(x) >= 2L, all(is.finite(x)), is.numeric(mu0), length(mu0) == 1L, 
        is.finite(mu0))
    fit <- stats::t.test(x, mu = mu0, alternative = alternative, conf.level = conf.level)
    list(method = fit$method, n = length(x), mean = mean(x), reference = mu0, mean_difference = mean(x) - 
        mu0, confidence_interval = fit$conf.int - mu0, statistic = unname(fit$statistic), df = unname(fit$parameter), 
        p_value = fit$p.value)
}

