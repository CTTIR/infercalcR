# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.prop_two_independent_pearson_chisq.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_prop_two_independent_pearson_chisq_analyse <- function(counts, alternative = "two.sided", conf.level = 0.95) {
    stopifnot(length(alternative) == 1L, alternative %in% c("two.sided", "greater", "less"), is.numeric(conf.level), 
        length(conf.level) == 1L, is.finite(conf.level), conf.level > 0, conf.level < 1, is.matrix(counts), 
        is.numeric(counts), identical(dim(counts), c(2L, 2L)), all(is.finite(counts)), all(counts >= 
            0), all(counts == floor(counts)), sum(counts) <= 2^31 - 1, all(rowSums(counts) > 0), all(colSums(counts) > 
            0))
    messages <- character()
    n <- rowSums(counts)
    x <- counts[, 1]
    fit <- withCallingHandlers(stats::prop.test(x, n, alternative = alternative, conf.level = conf.level, 
        correct = FALSE), warning = function(w) {
        messages <<- c(messages, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
    list(method = fit$method, status = "ok", group_sizes = n, events = x, proportions = x/n, proportion_difference = unname(x[1]/n[1] - 
        x[2]/n[2]), confidence_interval = as.numeric(fit$conf.int), confidence_level = conf.level, interval_method = "Unpooled normal approximation for the difference; truncated to [-1,1].", 
        alternative = alternative, statistic = unname(fit$statistic), df = 1, p_value = fit$p.value, 
        expected = outer(n, c(sum(x), sum(n) - sum(x))/sum(n)), continuity_correction = FALSE, alpha = 1 - 
            conf.level, reject = fit$p.value < 1 - conf.level, warnings = messages, assumptions = "Independent binomial groups and adequate expected counts. Interval and test use different variance estimators; their decisions need not coincide. No automatic exact-test substitution.")
}

