# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.stuart_maxwell_marginal_homogeneity.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_stuart_maxwell_marginal_homogeneity_check_table <- function(counts, alternative, conf.level) {
    stopifnot(is.matrix(counts), is.numeric(counts), nrow(counts) >= 3L, nrow(counts) == ncol(counts), 
        all(is.finite(counts)), all(counts >= 0), all(counts == floor(counts)), sum(counts) > 0, sum(counts) <= 
            2^31 - 1, !is.null(rownames(counts)), identical(rownames(counts), colnames(counts)), !anyNA(rownames(counts)), 
        all(nzchar(rownames(counts))), !anyDuplicated(rownames(counts)), identical(alternative, "two.sided"), 
        is.numeric(conf.level), length(conf.level) == 1L, is.finite(conf.level), conf.level > 0, conf.level < 
            1)
}

.obs_stuart_maxwell_marginal_homogeneity_check_excluded <- function(n_excluded) {
    stopifnot(is.numeric(n_excluded), length(n_excluded) == 1L, is.finite(n_excluded), n_excluded >= 
        0, n_excluded == floor(n_excluded))
}

.obs_stuart_maxwell_marginal_homogeneity_analyse <- function(counts, alternative = "two.sided", conf.level = 0.95, 
    n_excluded = 0L) {
    .obs_stuart_maxwell_marginal_homogeneity_check_table(counts, alternative, conf.level)
    k <- nrow(counts)
    n <- sum(counts)
    .obs_stuart_maxwell_marginal_homogeneity_check_excluded(n_excluded)
    difference <- rowSums(counts) - colSums(counts)
    covariance <- -(counts + t(counts))
    diag(covariance) <- rowSums(counts) + colSums(counts) - 2 * diag(counts)
    v <- covariance[-k, -k, drop = FALSE]
    d <- difference[-k]
    inverse_product <- tryCatch(solve(v, d), error = function(e) NULL)
    if (is.null(inverse_product)) 
        return(list(status = "unavailable", p_value = NULL, reason = "Singular or numerically unresolved marginal covariance; no automatic category pooling or generalized inverse."))
    statistic <- sum(d * inverse_product)
    df <- k - 1
    p <- stats::pchisq(statistic, df, lower.tail = FALSE)
    null <- "Equal marginal category distributions at the two occasions"
    list(method = "stuart_maxwell_marginal_homogeneity", status = "ok", n_pairs = n, n_excluded = n_excluded, 
        categories = rownames(counts), null = null, observed = counts, row_proportions = rowSums(counts)/n, 
        column_proportions = colSums(counts)/n, statistic = statistic, df = df, p_value = p, confidence_interval = NULL, 
        interval_status = "not_computed", requested_confidence_level = conf.level, alpha = 1 - conf.level, 
        reject = p < 1 - conf.level, assumptions = "Independent matched participants; identical category meaning/order; chi-squared large-sample approximation. Symmetry and marginal homogeneity are different hypotheses; no automatic substitution.")
}

