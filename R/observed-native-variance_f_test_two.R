# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.variance_f_test_two.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_variance_f_test_two_check_settings <- function(alternative, conf.level, omnibus = FALSE) {
    stopifnot(is.character(alternative), length(alternative) == 1L, !is.na(alternative), alternative %in% 
        c("two.sided", "greater", "less"), is.numeric(conf.level), length(conf.level) == 1L, is.finite(conf.level), 
        conf.level > 0, conf.level < 1)
    if (omnibus) 
        stopifnot(identical(alternative, "two.sided"))
}

.obs_variance_f_test_two_check_sample <- function(x) stopifnot(is.numeric(x), is.null(dim(x)), length(x) >= 
    2L, all(is.finite(x)))

.obs_variance_f_test_two_analyse <- function(x, y, ratio0 = 1, alternative = "two.sided", conf.level = 0.95) {
    .obs_variance_f_test_two_check_settings(alternative, conf.level)
    .obs_variance_f_test_two_check_sample(x)
    .obs_variance_f_test_two_check_sample(y)
    stopifnot(is.numeric(ratio0), length(ratio0) == 1L, is.finite(ratio0), ratio0 > 0, is.finite(stats::var(x)), 
        is.finite(stats::var(y)), stats::var(x) > 0, stats::var(y) > 0)
    stopifnot(is.finite(stats::var(x)/stats::var(y)), is.finite((stats::var(x)/stats::var(y))/ratio0), 
        stats::var(x)/stats::var(y) > 0, (stats::var(x)/stats::var(y))/ratio0 > 0)
    fit <- stats::var.test(x, y, ratio = ratio0, alternative = alternative, conf.level = conf.level)
    list(method = fit$method, status = "ok", n_x = length(x), n_y = length(y), variance_x = stats::var(x), 
        variance_y = stats::var(y), variance_ratio = unname(fit$estimate), null_ratio = ratio0, alternative = alternative, 
        statistic = unname(fit$statistic), df = unname(fit$parameter), p_value = fit$p.value, confidence_interval = as.numeric(fit$conf.int), 
        confidence_level = conf.level, interval_estimand = "population variance x / population variance y", 
        alpha = 1 - conf.level, reject = fit$p.value < 1 - conf.level)
}

