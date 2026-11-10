# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.boschloo_two_sided_central.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_boschloo_two_sided_central_check_settings <- function(alternative, conf.level) {
    stopifnot(is.character(alternative), length(alternative) == 1L, alternative %in% "two.sided", is.numeric(conf.level), 
        length(conf.level) == 1L, is.finite(conf.level), conf.level > 0, conf.level < 1)
}

.obs_boschloo_two_sided_central_analyse <- function(table, alternative = "two.sided", conf.level = 0.95) {
    .obs_boschloo_two_sided_central_check_settings(alternative, conf.level)
    .obs_boschloo_check_table(table)
    variant <- "central"
    fit <- boschloo_inference(table[1, 1], sum(table[1, ]), table[2, 1], sum(table[2, ]), variant = variant, 
        alpha = 1 - conf.level, groups = c("group_x", "group_y"), event = "event")$payload$native
    list(status = fit$status$code, reason = fit$status$reason, method = fit$variant$label, table = table, 
        n_total = sum(table), event_probability_difference = table[1, 1]/sum(table[1, ]) - table[2, 1]/sum(table[2, 
            ]), null_difference = 0, p_value = fit$p_value, p_upper = fit$p_upper, alternative = alternative, 
        confidence_interval = NULL, confidence_level = conf.level, interval_status = "not_computed", 
        interval_method = "No sampling confidence interval is implemented by this example.", numerical_bound_note = "p_upper bounds numerical optimisation error, not sampling uncertainty.", 
        decision = fit$decision, observed_result = fit, limitations = "Research preview: full numerical release certification remains pending. No substitution of a Fisher or asymptotic test.")
}

