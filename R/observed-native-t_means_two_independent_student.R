# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.t_means_two_independent_student.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_t_means_two_independent_student_analyse <- function(x, y, alternative = "two.sided", conf.level = 0.95) {
    stopifnot(is.numeric(x), is.numeric(y), length(x) >= 2L, length(y) >= 2L, all(is.finite(x)), all(is.finite(y)), 
        length(alternative) == 1L, alternative %in% c("two.sided", "greater", "less"), is.numeric(conf.level), 
        length(conf.level) == 1L, is.finite(conf.level), conf.level > 0, conf.level < 1)
    fit <- stats::t.test(x, y, paired = FALSE, var.equal = TRUE, alternative = alternative, mu = 0, conf.level = conf.level)
    list(method = fit$method, n = c(x = length(x), y = length(y)), mean_difference = mean(x) - mean(y), 
        confidence_interval = fit$conf.int, statistic = unname(fit$statistic), df = unname(fit$parameter), 
        p_value = fit$p.value, alternative = fit$alternative)
}

