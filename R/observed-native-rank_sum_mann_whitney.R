# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.rank_sum_mann_whitney.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_rank_sum_mann_whitney_check_settings <- function(alternative, conf.level, exact, location_shift) {
    stopifnot(is.character(alternative), length(alternative) == 1L, alternative %in% c("two.sided", "greater", 
        "less"), is.numeric(conf.level), length(conf.level) == 1L, is.finite(conf.level), conf.level > 
        0, conf.level < 1, is.logical(exact), length(exact) == 1L, !is.na(exact), is.logical(location_shift), 
        length(location_shift) == 1L, !is.na(location_shift))
}

.obs_rank_sum_mann_whitney_check_vector <- function(x) stopifnot(is.numeric(x), is.null(dim(x)), length(x) > 
    0L, all(is.finite(x)))

.obs_rank_sum_mann_whitney_analyse <- function(x, y, alternative = "two.sided", conf.level = 0.95, exact = TRUE, 
    location_shift = FALSE) {
    .obs_rank_sum_mann_whitney_check_settings(alternative, conf.level, exact, location_shift)
    .obs_rank_sum_mann_whitney_check_vector(x)
    .obs_rank_sum_mann_whitney_check_vector(y)
    ties <- anyDuplicated(c(x, y)) > 0L
    if (exact) 
        stopifnot(!ties, length(x) + length(y) <= 30L)
    ranks <- rank(c(x, y))
    u <- sum(ranks[seq_along(x)]) - length(x) * (length(x) + 1)/2
    superiority <- u/(length(x) * length(y))
    if (length(unique(c(x, y))) == 1L) 
        return(list(status = "not_applicable", reason = "All pooled values are equal; rank variance is zero.", 
            p_value = NULL, probability_superiority = superiority, confidence_interval = NULL))
    messages <- character()
    fit <- withCallingHandlers(stats::wilcox.test(x, y, alternative = alternative, exact = exact, correct = TRUE, 
        conf.int = location_shift, conf.level = conf.level, digits.rank = Inf), warning = function(w) {
        messages <<- c(messages, conditionMessage(w))
        invokeRestart("muffleWarning")
    })
    list(status = if (is.finite(fit$p.value)) "ok" else "undefined", method = fit$method, n_x = length(x), 
        n_y = length(y), tied_ranks = ties, statistic = unname(fit$statistic), p_value = if (is.finite(fit$p.value)) fit$p.value else NULL, 
        alternative = alternative, probability_superiority = superiority, rank_biserial = 2 * superiority - 
            1, superiority_interval_status = "not_computed", shift_estimate = if (location_shift) unname(fit$estimate) else NULL, 
        confidence_interval = if (location_shift) as.numeric(fit$conf.int) else NULL, requested_confidence_level = conf.level, 
        attained_confidence_level = if (location_shift) attr(fit$conf.int, "conf.level") else NULL, interval_status = if (location_shift) "shift_model_only" else "not_requested", 
        interval_method = "Hodges-Lehmann x-minus-y shift interval only under the explicitly assumed location-shift model", 
        exact = exact, location_shift = location_shift, diagnostics = messages, limitations = "Greater means x tends to exceed y. Probability of superiority is descriptive; the rank test assumes identical distributions under its null. No mean/median interpretation for general ordinal or unequal-shape data.")
}

