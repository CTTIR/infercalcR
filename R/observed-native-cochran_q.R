# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.cochran_q.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_cochran_q_check_blocks <- function(data, missing, alternative, conf.level) {
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

.obs_cochran_q_analyse <- function(data, alternative = "two.sided", conf.level = 0.95, missing = "error") {
    b <- .obs_cochran_q_check_blocks(data, missing, alternative, conf.level)
    raw <- as.matrix(data[, setdiff(names(data), "id"), drop = FALSE])
    stopifnot(all(is.na(raw) | raw %in% c(0, 1)))
    x <- b$x
    k <- ncol(x)
    stopifnot(all(x %in% c(0, 1)))
    totals <- colSums(x)
    rows <- rowSums(x)
    denominator <- sum(rows * (k - rows))
    if (denominator == 0) 
        return(list(status = "unavailable", reason = "No discordant blocks: conditional variance is zero.", 
            n_input = b$n_input, n_complete = nrow(x), n_excluded = b$n_excluded, p_value = NULL))
    statistic <- (k - 1) * k * sum((totals - mean(totals))^2)/denominator
    df <- k - 1
    p <- stats::pchisq(statistic, df, lower.tail = FALSE)
    descriptive <- list(event = 1, event_counts = totals, event_proportions = colMeans(x))
    list(method = "cochran_q", status = "ok", n_input = b$n_input, n_complete = nrow(x), n_excluded = b$n_excluded, 
        occasions = colnames(x), statistic = statistic, df = df, p_value = p, descriptive = descriptive, 
        confidence_interval = NULL, interval_status = "not_computed", requested_confidence_level = conf.level, 
        alpha = 1 - conf.level, reject = p < 1 - conf.level, missing_strategy = missing, assumptions = "Independent participants; matched occasions; complete blocks. Complete-case inference requires justified MCAR. Chi-squared asymptotic omnibus test; no automatic pairwise testing or method substitution.")
}

