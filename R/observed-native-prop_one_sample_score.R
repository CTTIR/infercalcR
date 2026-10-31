# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.prop_one_sample_score.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_prop_one_sample_score_check_settings <- function(alternative, conf.level) {
    stopifnot(length(alternative) == 1L, alternative %in% c("two.sided", "greater", "less"), is.numeric(conf.level), 
        length(conf.level) == 1L, is.finite(conf.level), conf.level > 0, conf.level < 1)
}

.obs_prop_one_sample_score_analyse <- function(events, n, p0, alternative = "two.sided", conf.level = 0.95) {
    .obs_prop_one_sample_score_check_settings(alternative, conf.level)
    stopifnot(is.numeric(events), length(events) == 1L, is.finite(events), events >= 0, events == floor(events), 
        is.numeric(n), length(n) == 1L, is.finite(n), n > 0, n == floor(n), events <= n, is.numeric(p0), 
        length(p0) == 1L, is.finite(p0), p0 > 0, p0 < 1)
    fit <- stats::prop.test(events, n, p = p0, alternative = alternative, conf.level = conf.level, correct = FALSE)
    list(method = fit$method, events = events, n = n, proportion = events/n, reference = p0, confidence_interval = fit$conf.int, 
        p_value = fit$p.value, alternative = alternative)
}

