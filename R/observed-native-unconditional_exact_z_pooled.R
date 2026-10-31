# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.unconditional_exact_z_pooled.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_unconditional_exact_z_pooled_check_settings <- function(alternative, conf.level) {
    stopifnot(is.character(alternative), length(alternative) == 1L, alternative %in% c("two.sided", "less", 
        "greater"), is.numeric(conf.level), length(conf.level) == 1L, is.finite(conf.level), conf.level > 
        0, conf.level < 1)
}

.obs_unconditional_exact_z_pooled_analyse <- function(table, alternative = "two.sided", conf.level = 0.95, 
    independent_binomial_groups = FALSE, tolerance = 1e-08, max_subdivisions = 4096L) {
    .obs_unconditional_exact_z_pooled_check_settings(alternative, conf.level)
    stopifnot(isTRUE(independent_binomial_groups), is.matrix(table), is.numeric(table), identical(dim(table), 
        c(2L, 2L)), all(is.finite(table)), all(table >= 0), all(table == floor(table)), all(rowSums(table) > 
        0), sum(table) <= 30, is.numeric(tolerance), length(tolerance) == 1L, is.finite(tolerance), tolerance >= 
        1e-10, tolerance <= 1e-04, is.numeric(max_subdivisions), length(max_subdivisions) == 1L, is.finite(max_subdivisions), 
        max_subdivisions >= 1, max_subdivisions <= 4096, max_subdivisions == floor(max_subdivisions))
    n1 <- sum(table[1, ])
    n2 <- sum(table[2, ])
    N <- n1 + n2
    grid <- expand.grid(x1 = 0:n1, x2 = 0:n2)
    s <- grid$x1 + grid$x2
    delta <- grid$x1 * n2 - grid$x2 * n1
    den <- s * (N - s)
    den[den == 0] <- 1
    observed <- which(grid$x1 == table[1, 1] & grid$x2 == table[2, 1])
    lhs <- delta^2 * den[observed]
    rhs <- delta[observed]^2 * den
    sg <- sign(delta)
    so <- sg[observed]
    greater <- sg > so | (sg == so & if (so >= 0) 
        lhs >= rhs
    else lhs <= rhs)
    less <- sg < so | (sg == so & if (so >= 0) 
        lhs <= rhs
    else lhs >= rhs)
    extreme <- switch(alternative, two.sided = lhs >= rhs, greater = greater, less = less)
    weights <- round(choose(n1, grid$x1)) * round(choose(n2, grid$x2))
    coefficients <- vapply(0:N, function(k) sum(weights[extreme & s == k])/round(choose(N, k)), 0)
    eps <- 4 * .Machine$double.eps
    split <- function(v, side) {
        left <- right <- numeric(length(v))
        left[1] <- v[1]
        right[length(v)] <- v[length(v)]
        for (k in seq_len(length(v) - 1L)) {
            v <- (utils::head(v, -1L) + utils::tail(v, -1L))/2 + side * eps
            v <- pmax(0, pmin(1, v))
            left[k + 1L] <- v[1]
            right[length(right) - k] <- v[length(v)]
        }
        list(left = left, right = right)
    }
    lo <- pmax(0, coefficients - eps)
    hi <- pmin(1, coefficients + eps)
    nodes <- list(list(lo = lo, hi = hi, a = 0, b = 1))
    best <- max(lo[c(1, N + 1)])
    upper <- max(hi)
    iterations <- 0L
    while (upper - best > tolerance && iterations < max_subdivisions) {
        j <- which.max(vapply(nodes, function(v) max(v$hi), 0))
        node <- nodes[[j]]
        low <- split(node$lo, -1)
        high <- split(node$hi, 1)
        mid <- (node$a + node$b)/2
        best <- max(best, utils::tail(low$left, 1))
        nodes[[j]] <- NULL
        nodes <- c(nodes, list(list(lo = low$left, hi = high$left, a = node$a, b = mid), list(lo = low$right, 
            hi = high$right, a = mid, b = node$b)))
        keep <- vapply(nodes, function(v) max(v$hi) > best, TRUE)
        nodes <- nodes[keep]
        upper <- max(best, vapply(nodes, function(v) max(v$hi), 0))
        iterations <- iterations + 1L
    }
    converged <- upper - best <= tolerance
    alpha <- 1 - conf.level
    decision <- if (!converged) 
        "numerical_limit"
    else if (upper <= alpha) 
        "reject"
    else if (best > alpha) 
        "do_not_reject"
    else "numerically_indeterminate"
    list(status = if (converged) "ok" else "numerical_limit", method = "Unconditional exact pooled-Z test", 
        table = table, n_total = N, alternative = alternative, null_difference = 0, statistic = sign(delta[observed]) * 
            sqrt(delta[observed]^2 * N/(n1 * n2 * den[observed])), event_probability_difference = table[1, 
            1]/n1 - table[2, 1]/n2, p_value = if (converged) upper else NA_real_, p_lower = best, p_upper = upper, 
        numerical_tolerance = tolerance, nuisance_interval = c(0, 1), subdivisions = iterations, decision = decision, 
        alpha = alpha, confidence_interval = NULL, confidence_level = conf.level, interval_status = "not_computed", 
        interval_method = "No sampling interval is implemented.", numerical_bound_note = "Numerical enclosure of the nuisance maximum; not sampling uncertainty.", 
        limitations = "Total N at most 30. Independent fixed-size binomial groups and a prespecified event are required. No offset null, paired data, weights or method fallback.")
}

