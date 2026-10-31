# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.fisher_exact_2x2.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_fisher_exact_2x2_check_settings <- function(alternative, conf.level) {
    stopifnot(is.character(alternative), length(alternative) == 1L, alternative %in% c("two.sided", "greater", 
        "less"), is.numeric(conf.level), length(conf.level) == 1L, is.finite(conf.level), conf.level > 
        0, conf.level < 1)
}

.obs_fisher_exact_2x2_analyse <- function(table, alternative = "two.sided", conf.level = 0.95) {
    .obs_fisher_exact_2x2_check_settings(alternative, conf.level)
    stopifnot(is.matrix(table), is.numeric(table), identical(dim(table), c(2L, 2L)), all(is.finite(table)), 
        all(table >= 0), all(table == floor(table)), all(rowSums(table) > 0), all(colSums(table) > 0))
    fit <- stats::fisher.test(table, alternative = alternative, conf.level = conf.level, or = 1)
    list(status = "ok", method = fit$method, n_total = sum(table), table = table, conditional_odds_ratio = unname(fit$estimate), 
        null_odds_ratio = 1, p_value = fit$p.value, alternative = alternative, confidence_interval = as.numeric(fit$conf.int), 
        confidence_level = conf.level, interval_method = "Exact conditional odds-ratio interval", limitations = "Conditional MLE differs from the sample cross-product odds ratio; two-sided p-value uses probability ordering.")
}

