# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.correlation_kendall.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_correlation_kendall_check_settings <- function(alternative, conf.level) {
    stopifnot(length(alternative) == 1L, alternative %in% c("two.sided", "greater", "less"), is.numeric(conf.level), 
        length(conf.level) == 1L, is.finite(conf.level), conf.level > 0, conf.level < 1)
}

.obs_correlation_kendall_analyse <- function(data, alternative = "two.sided", conf.level = 0.95) {
    .obs_correlation_kendall_check_settings(alternative, conf.level)
    stopifnot(is.data.frame(data), all(c("id", "x", "y") %in% names(data)), nrow(data) >= 4, !anyNA(data$id), 
        !anyDuplicated(data$id), is.numeric(data$x), is.numeric(data$y), all(is.finite(data$x)), all(is.finite(data$y)), 
        stats::sd(data$x) > 0, stats::sd(data$y) > 0)
    fit <- stats::cor.test(data$x, data$y, method = "kendall", alternative = alternative, conf.level = conf.level, 
        exact = FALSE)
    list(method = fit$method, n = nrow(data), estimate = unname(fit$estimate), statistic = unname(fit$statistic), 
        confidence_interval = if (is.null(fit$conf.int)) NULL else fit$conf.int, interval_status = if (is.null(fit$conf.int)) "not_provided_by_method" else "available", 
        p_value = fit$p.value, alternative = alternative)
}

