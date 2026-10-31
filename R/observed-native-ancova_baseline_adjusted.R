# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.ancova_baseline_adjusted.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_ancova_baseline_adjusted_fit_linear <- function(data, predictors, conf.level, missing) {
    stopifnot(is.data.frame(data), !anyDuplicated(names(data)), all(c("id", "y") %in% names(data)), !anyNA(data$id), 
        !anyDuplicated(data$id), is.character(predictors), length(predictors) >= 1L, !anyNA(predictors), 
        !anyDuplicated(predictors), all(predictors %in% names(data)), !any(predictors %in% c("id", "y")), 
        is.numeric(conf.level), length(conf.level) == 1L, is.finite(conf.level), conf.level > 0, conf.level < 
            1, length(missing) == 1L, missing %in% c("error", "complete_cases"))
    values <- data[, c("y", predictors), drop = FALSE]
    stopifnot(all(vapply(values, is.numeric, logical(1))), !any(is.infinite(as.matrix(values))))
    keep <- stats::complete.cases(values)
    if (any(!keep) && missing == "error") 
        stop("Choose and justify complete_cases explicitly before excluding incomplete rows.")
    values <- values[keep, , drop = FALSE]
    n <- nrow(values)
    p <- length(predictors)
    stopifnot(n > p + 1L)
    names(values) <- c("y", paste0("x", seq_len(p)))
    fit <- stats::lm(stats::reformulate(names(values)[-1L], response = "y"), data = values, na.action = stats::na.fail, 
        singular.ok = FALSE)
    residual <- stats::residuals(fit)
    rss <- sum(residual^2)
    tss <- sum((values$y - mean(values$y))^2)
    if (!is.finite(rss) || !is.finite(tss) || tss <= 0 || rss <= 64 * n^2 * .Machine$double.eps^2 * max(tss, 
        sum(values$y^2))) 
        stop("Constant response or numerically exact fit: residual-variance inference is unavailable.")
    sm <- summary(fit)
    tab <- sm$coefficients
    rownames(tab) <- c("(Intercept)", predictors)
    critical <- stats::qt((1 - conf.level)/2, fit$df.residual, lower.tail = FALSE)
    coefficients <- data.frame(term = rownames(tab), estimate = tab[, 1], std_error = tab[, 2], statistic = tab[, 
        3], p_value = tab[, 4], lower = tab[, 1] - critical * tab[, 2], upper = tab[, 1] + critical * 
        tab[, 2], row.names = NULL)
    f <- unname(sm$fstatistic)
    omnibus <- list(statistic = f[1], df_numerator = f[2], df_denominator = f[3], p_value = stats::pf(f[1], 
        f[2], f[3], lower.tail = FALSE), null = "All non-intercept coefficients are zero")
    list(fit = fit, values = values, n_input = nrow(data), n_excluded = sum(!keep), n = n, coefficients = coefficients, 
        rss = rss, tss = tss, omnibus = omnibus, df = fit$df.residual, diagnostics = list(fitted = stats::fitted(fit), 
            residuals = residual, leverage = stats::hatvalues(fit), cooks_distance = stats::cooks.distance(fit), 
            scaled_design_condition_number = kappa(scale(as.matrix(values[, -1, drop = FALSE])), exact = TRUE)), 
        assumptions = "Independent units; prespecified linear mean; classical homoscedastic normal-error t/F inference. Complete-case validity needs justified missingness assumptions. No automatic robust substitution or causal interpretation.")
}

.obs_ancova_baseline_adjusted_analyse <- function(data, covariates, group_levels, common_slopes, conf.level = 0.95, 
    missing = "error", alternative = "two.sided") {
    stopifnot(is.data.frame(data), !anyDuplicated(names(data)), all(c("id", "y", "group") %in% names(data)), 
        is.character(covariates), length(covariates) >= 1L, !anyNA(covariates), !anyDuplicated(covariates), 
        all(covariates %in% names(data)), !any(covariates %in% c("id", "y", "group")), is.character(group_levels), 
        length(group_levels) >= 2L, !anyNA(group_levels), !anyDuplicated(group_levels), all(nzchar(group_levels)), 
        isTRUE(common_slopes), identical(alternative, "two.sided"), missing %in% c("error", "complete_cases"), 
        length(missing) == 1L, !anyNA(data$id), !anyDuplicated(data$id))
    g <- as.character(data$group)
    stopifnot(all(is.na(g) | g %in% group_levels))
    values <- data[, c("y", covariates), drop = FALSE]
    stopifnot(all(vapply(values, is.numeric, logical(1))), !any(is.infinite(as.matrix(values))))
    keep <- stats::complete.cases(values) & !is.na(g)
    if (any(!keep) && missing == "error") 
        stop("Choose and justify complete_cases explicitly before excluding incomplete rows.")
    values <- values[keep, , drop = FALSE]
    g <- g[keep]
    stopifnot(all(group_levels %in% g))
    k <- length(group_levels)
    nc <- length(covariates)
    names(values) <- c("y", paste0("x", seq_len(nc)))
    d <- data.frame(id = data$id[keep], values, check.names = FALSE)
    for (j in 2:k) d[[paste0("g", j)]] <- as.numeric(g == group_levels[j])
    predictors <- c(paste0("g", 2:k), paste0("x", seq_len(nc)))
    b <- .obs_ancova_baseline_adjusted_fit_linear(d, predictors, conf.level, "error")
    group_beta <- stats::coef(b$fit)[2:k]
    group_covariance <- summary(b$fit)$cov.unscaled[2:k, 2:k, drop = FALSE]
    standardised <- forwardsolve(t(chol(group_covariance)), group_beta)
    delta <- sum(standardised^2)
    stopifnot(is.finite(delta), delta >= 0)
    statistic <- (delta/(k - 1))/(b$rss/b$df)
    group_test <- list(statistic = statistic, df_numerator = k - 1L, df_denominator = b$df, p_value = stats::pf(statistic, 
        k - 1, b$df, lower.tail = FALSE), partial_r_squared = delta/(delta + b$rss), null = "All group conditional means are equal at common covariate values under the prespecified additive model")
    reference <- colMeans(as.matrix(d[, paste0("x", seq_len(nc)), drop = FALSE]))
    L <- cbind(1, rbind(rep(0, k - 1), diag(k - 1)), matrix(rep(reference, each = k), nrow = k))
    beta <- stats::coef(b$fit)
    V <- stats::vcov(b$fit)
    q <- stats::qt((1 - conf.level)/2, b$df, lower.tail = FALSE)
    means <- as.vector(L %*% beta)
    se <- sqrt(diag(L %*% V %*% t(L)))
    adjusted <- data.frame(group = group_levels, estimate = means, std_error = se, lower = means - q * 
        se, upper = means + q * se)
    pairs <- utils::combn(seq_len(k), 2)
    contrasts <- t(vapply(seq_len(ncol(pairs)), function(i) {
        a <- pairs[1, i]
        z <- pairs[2, i]
        C <- L[z, ] - L[a, ]
        estimate <- sum(C * beta)
        se <- sqrt(as.numeric(C %*% V %*% C))
        t <- estimate/se
        c(estimate = estimate, std_error = se, statistic = t, p_value = 2 * stats::pt(abs(t), b$df, lower.tail = FALSE), 
            lower = estimate - q * se, upper = estimate + q * se)
    }, numeric(6)))
    differences <- data.frame(group = group_levels[pairs[2, ]], reference = group_levels[pairs[1, ]], 
        contrasts, row.names = NULL)
    list(method = "Prespecified additive ANCOVA", status = "ok", n = nrow(d), n_input = nrow(data), n_excluded = sum(!keep), 
        covariates = covariates, group_levels = group_levels, common_slopes = TRUE, reference_covariates = stats::setNames(reference, 
            covariates), reference_rule = "pooled means of retained covariates; common to all groups", 
        adjusted_means = adjusted, differences = differences, multiplicity = "unadjusted marginal intervals and p-values; no simultaneous coverage", 
        group_test = group_test, confidence_level = conf.level, missing_strategy = missing, diagnostics = b$diagnostics, 
        assumptions = "Prespecified covariates unaffected by group assignment; independent units; additive linear mean and common slopes; homoscedastic normal errors for classical t/F inference. Covariate overlap is needed for interpretation; no causal claim follows from adjustment alone. Complete-case validity needs justified missingness. Mean confidence intervals are not individual prediction intervals. No interaction pretest, automated variable selection or robust substitution is performed.")
}

