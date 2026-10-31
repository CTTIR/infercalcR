# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.anova_one_way.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_anova_one_way_check_input <- function(x, group, alternative, conf.level) {
    stopifnot(is.numeric(x), is.null(dim(x)), length(x) >= 4L, all(is.finite(x)), is.factor(group) || 
        is.character(group), length(group) == length(x), !anyNA(group), all(nzchar(as.character(group))), 
        identical(alternative, "two.sided"), is.numeric(conf.level), length(conf.level) == 1L, is.finite(conf.level), 
        conf.level > 0, conf.level < 1)
    g <- droplevels(factor(group))
    stopifnot(nlevels(g) >= 2L, all(table(g) >= 2L))
    g
}

.obs_anova_one_way_analyse <- function(x, group, alternative = "two.sided", conf.level = 0.95) {
    g <- .obs_anova_one_way_check_input(x, group, alternative, conf.level)
    v <- tapply(x, g, stats::var)
    means <- tapply(x, g, mean)
    stopifnot(all(is.finite(v)), all(is.finite(means)))
    if (all(v <= 0)) 
        return(list(status = "not_applicable", p_value = NULL, reason = "The required within-group variance is zero or unresolved.", 
            confidence_interval = NULL, interval_status = "not_computed"))
    fit <- stats::oneway.test(x ~ g, var.equal = TRUE)
    if (!is.finite(fit$statistic) || !is.finite(fit$p.value) || any(!is.finite(fit$parameter))) 
        return(list(status = "not_applicable", p_value = NULL, reason = "The F calculation is not numerically resolved.", 
            confidence_interval = NULL, interval_status = "not_computed"))
    list(method = fit$method, status = "ok", group_sizes = table(g), group_means = means, group_variances = v, 
        statistic = unname(fit$statistic), df = unname(fit$parameter), p_value = fit$p.value, confidence_interval = NULL, 
        interval_status = "not_computed", requested_confidence_level = conf.level, alpha = 1 - conf.level, 
        reject = fit$p.value < 1 - conf.level, posthoc = "No pairwise comparisons computed. Specify contrasts and multiplicity control before analysis.", 
        assumptions = "Independent groups and units; normal within-group errors for the F model. Equal population variances justified in advance.")
}

