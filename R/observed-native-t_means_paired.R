# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.t_means_paired.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_t_means_paired_analyse <- function(data, alternative = "two.sided", conf.level = 0.95) {
    stopifnot(is.data.frame(data), all(c("id", "x", "y") %in% names(data)), nrow(data) >= 2L, !anyNA(data$id), 
        !anyDuplicated(data$id), is.numeric(data$x), is.numeric(data$y), all(is.finite(data$x)), all(is.finite(data$y)), 
        length(alternative) == 1L, alternative %in% c("two.sided", "greater", "less"), is.numeric(conf.level), 
        length(conf.level) == 1L, is.finite(conf.level), conf.level > 0, conf.level < 1)
    fit <- stats::t.test(data$x, data$y, paired = TRUE, alternative = alternative, mu = 0, conf.level = conf.level)
    list(method = fit$method, n_pairs = nrow(data), mean_difference = mean(data$x - data$y), confidence_interval = fit$conf.int, 
        statistic = unname(fit$statistic), df = unname(fit$parameter), p_value = fit$p.value, alternative = fit$alternative)
}

