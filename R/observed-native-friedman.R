# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.friedman.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_friedman_check_blocks <- function(data, missing, alternative, conf.level) {
    stopifnot(is.data.frame(data), "id" %in% names(data), nrow(data) >= 2L, !anyNA(data$id), !anyDuplicated(data$id), 
        !anyDuplicated(names(data)), identical(alternative, "two.sided"), is.numeric(conf.level), length(conf.level) == 
            1L, is.finite(conf.level), conf.level > 0, conf.level < 1, length(missing) == 1L, missing %in% 
            c("error", "complete_cases"))
    values <- data[, setdiff(names(data), "id"), drop = FALSE]
    stopifnot(ncol(values) >= 3L, all(vapply(values, is.numeric, logical(1))))
    x <- as.matrix(values)
    stopifnot(!any(is.infinite(x)))
    keep <- stats::complete.cases(x)
    if (any(!keep) && missing == "error") 
        stop("Missing occasions: choose and justify complete_cases explicitly.")
    stopifnot(sum(keep) >= 2L)
    list(x = x[keep, , drop = FALSE], n_input = nrow(x), n_excluded = sum(!keep))
}

.obs_friedman_analyse <- function(data, alternative = "two.sided", conf.level = 0.95, missing = "error") {
    b <- .obs_friedman_check_blocks(data, missing, alternative, conf.level)
    x <- b$x
    k <- ncol(x)
    if (all(apply(x, 1L, function(z) length(unique(z)) == 1L))) 
        return(list(status = "unavailable", reason = "All blocks are tied: rank variance is zero.", n_input = b$n_input, 
            n_complete = nrow(x), n_excluded = b$n_excluded, p_value = NULL))
    fit <- stats::friedman.test(x)
    statistic <- unname(fit$statistic)
    df <- unname(fit$parameter)
    p <- fit$p.value
    descriptive <- list(mean_within_block_ranks = colMeans(t(apply(x, 1L, rank))), kendall_w = statistic/(nrow(x) * 
        (k - 1)))
    list(method = "friedman", status = "ok", n_input = b$n_input, n_complete = nrow(x), n_excluded = b$n_excluded, 
        occasions = colnames(x), statistic = statistic, df = df, p_value = p, descriptive = descriptive, 
        confidence_interval = NULL, interval_status = "not_computed", requested_confidence_level = conf.level, 
        alpha = 1 - conf.level, reject = p < 1 - conf.level, missing_strategy = missing, assumptions = "Independent participants; matched occasions; complete blocks. Complete-case inference requires justified MCAR. Chi-squared asymptotic omnibus test; no automatic pairwise testing or method substitution.")
}

