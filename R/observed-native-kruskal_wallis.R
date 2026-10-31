# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.kruskal_wallis.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_kruskal_wallis_check_input <- function(x, group, alternative, conf.level) {
    stopifnot(is.numeric(x), is.null(dim(x)), length(x) >= 4L, all(is.finite(x)), is.factor(group) || 
        is.character(group), length(group) == length(x), !anyNA(group), all(nzchar(as.character(group))), 
        identical(alternative, "two.sided"), is.numeric(conf.level), length(conf.level) == 1L, is.finite(conf.level), 
        conf.level > 0, conf.level < 1)
    g <- droplevels(factor(group))
    stopifnot(nlevels(g) >= 2L, all(table(g) >= 2L))
    g
}

.obs_kruskal_wallis_analyse <- function(x, group, alternative = "two.sided", conf.level = 0.95) {
    g <- .obs_kruskal_wallis_check_input(x, group, alternative, conf.level)
    if (length(unique(x)) < 2L) 
        return(list(status = "not_applicable", p_value = NULL, reason = "All observations are tied; rank variation is zero.", 
            confidence_interval = NULL, interval_status = "not_computed"))
    fit <- stats::kruskal.test(x, g)
    stopifnot(is.finite(fit$statistic), is.finite(fit$p.value))
    list(method = fit$method, status = "ok", group_sizes = table(g), group_mean_ranks = tapply(rank(x, 
        ties.method = "average"), g, mean), statistic = unname(fit$statistic), df = unname(fit$parameter), 
        p_value = fit$p.value, confidence_interval = NULL, interval_status = "not_computed", requested_confidence_level = conf.level, 
        alpha = 1 - conf.level, reject = fit$p.value < 1 - conf.level, posthoc = "No pairwise comparisons computed. Specify contrasts and multiplicity control before analysis.", 
        assumptions = "Independent groups and units, at least ordinal outcomes; identical distributions under the null, tie correction and approximate chi-squared reference.")
}

