# Extracted from inferguidR e91938f77eaa991b69989b121bfd32c124c95db0.
# Original source and MIT license: Raban Heller. See tools/extraction/source-map.csv.

.bl_plotdata <- function(plot_id, title, data, fixed, scope, reference = list(), notes = character(),
                         status = ig_status("ok")) {
  structure(list(object_type = "boschloo_plotdata", plot_id = plot_id, title = title,
                 status = .ig_status_to_list(status), scope = scope, fixed_inputs = fixed,
                 data = data, reference = reference, notes = notes),
            class = "boschloo_plotdata")
}

print.boschloo_plotdata <- function(x, ...) {
  cat(sprintf("<boschloo_plotdata> %s: %s\n", x$plot_id, x$title),
      sprintf("  scope:  %s\n", x$scope),
      sprintf("  status: %s\n", format(.ig_status_from_list(x$status))),
      sprintf("  rows:   %d\n", if (is.data.frame(x$data)) nrow(x$data) else 0L), sep = "")
  invisible(x)
}

.bl_pvalues_all <- function(n1, n2, info, control) {
  per_side <- lapply(info$sides, function(side) {
    ord <- .bl_ordering(n1, n2, side, control)
    val <- numeric(ord$n_levels)
    upp <- numeric(ord$n_levels)
    conv <- logical(ord$n_levels)
    for (j in seq_len(ord$n_levels)) {
      s <- .bl_sup(.bl_tail_c(ord, .bl_level_tables(ord, j)), control)
      val[j] <- s$value
      upp[j] <- s$upper
      conv[j] <- isTRUE(s$converged)
    }
    list(value = val[ord$level], upper = upp[ord$level], converged = all(conv),
         ordering = exp(ord$lt), level = ord$level, ambiguity = !is.null(ord$ambiguity))
  })
  if (length(per_side) == 1L) {
    s <- per_side[[1]]
    return(list(value = s$value, upper = s$upper, ordering = s$ordering, level = s$level,
                converged = s$converged, ambiguity = s$ambiguity))
  }
  lo <- per_side[[1]]
  up <- per_side[[2]]
  list(value = pmin(1, 2 * pmin(lo$value, up$value)), upper = pmin(1, 2 * pmin(lo$upper, up$upper)),
       ordering = pmin(1, 2 * pmin(lo$ordering, up$ordering)), level = NULL,
       level_less = lo$level, level_greater = up$level,
       converged = lo$converged && up$converged, ambiguity = lo$ambiguity || up$ambiguity)
}

boschloo_plotdata_lattice <- function(n1, n2, alpha = 0.05, variant, p1 = NULL, p2 = NULL,
                                      observed = NULL, pvalues = TRUE,
                                      control = boschloo_control()) {
  info <- .bl_variant(if (missing(variant)) NULL else variant)
  .bl_check_design(n1, n2)
  alpha <- .bl_check_alpha(alpha)
  if (is.null(p1) != is.null(p2)) .ig_abort_invalid("give both `p1` and `p2`, or neither")
  if (!is.null(p1)) {
    p1 <- .bl_check_prob(p1, "p1")
    p2 <- .bl_check_prob(p2, "p2")
  }
  if (!is.null(observed) && (!is.numeric(observed) || length(observed) != 2L ||
                             !.bl_is_count(observed[1]) || !.bl_is_count(observed[2]) ||
                             observed[1] > n1 || observed[2] > n2)) {
    .ig_abort_invalid("`observed` must be c(x1, x2) with 0 <= x1 <= n1 and 0 <= x2 <= n2")
  }
  control <- .bl_check_control(control)
  n1 <- as.integer(n1)
  n2 <- as.integer(n2)
  reg <- .bl_region(n1, n2, alpha, info, control)
  status <- .bl_region_status(reg)
  ok <- status$code == "ok"
  n_tab <- length(reg$member)
  notes <- character()
  pv <- NULL
  if (pvalues && n_tab <= 5000L && ok) {
    pv <- .bl_pvalues_all(n1, n2, info, control)
  } else if (pvalues) {
    notes <- c(notes, if (!ok) "p-values not computed: the region has no ok status" else
      sprintf("p-values not computed for %d tables (limit 5000); membership is still exact", n_tab))
  }
  pi_star <- if (ok && is.finite(reg$size$argmax)) reg$size$argmax else 0.5
  data <- data.frame(
    x1 = reg$x1, x2 = reg$x2, total = reg$t,
    proportion_1 = reg$x1 / n1, proportion_2 = reg$x2 / n2,
    fisher_ordering = .bl_fisher_p(n1, n2, info, control),
    boschloo_p = if (is.null(pv)) NA_real_ else pv$value,
    boschloo_p_upper = if (is.null(pv)) NA_real_ else pv$upper,
    in_region = if (ok) reg$member else NA,
    prob_null = stats::dbinom(reg$x1, n1, pi_star) * stats::dbinom(reg$x2, n2, pi_star),
    prob_alternative = if (is.null(p1)) NA_real_ else
      stats::dbinom(reg$x1, n1, p1) * stats::dbinom(reg$x2, n2, p2),
    observed = if (is.null(observed)) FALSE else reg$x1 == observed[1] & reg$x2 == observed[2]
  )
  if (!is.null(pv) && ok) {
    consistent <- identical(data$in_region, data$boschloo_p_upper <= alpha)
    if (!consistent) {
      notes <- c(notes, "per-table bounds and region membership differ at the boundary (tables within the floating-point allowance of alpha)")
    }
  }
  .bl_plotdata(
    "boschloo_lattice", sprintf("Possible tables and %s rejection region", info$label), data,
    fixed = list(n1 = n1, n2 = n2, alpha = alpha, variant = info$variant,
                 variant_id = info$variant_id, p1 = p1, p2 = p2,
                 null_probability_at = pi_star),
    scope = if (is.null(observed)) "hypothetical future study (all possible tables)" else
      "hypothetical future study; the marked table is the observed one (marking does not change planning inputs)",
    reference = list(attained_size = if (ok) reg$size$value else NA_real_,
                     attained_size_upper = if (ok) reg$size$upper else NA_real_,
                     power = if (!is.null(p1) && ok) .bl_power_idx(reg$idx, n1, n2, p1, p2) else NA_real_,
                     n_rejected = if (ok) length(reg$idx) else NA_integer_),
    notes = notes, status = status)
}

boschloo_plotdata_nuisance <- function(x, grid_points = 401L, control = NULL) {
  if (!.bl_is_count(grid_points, 3) || grid_points > 1e5) {
    .ig_abort_invalid("`grid_points` must be a whole number between 3 and 100000")
  }
  if (!inherits(x, "boschloo_test") && !inherits(x, "boschloo_region")) {
    .ig_abort("invalid_input", "`x` must be a boschloo_test or boschloo_region object")
  }
  recorded <- .bl_control_from_record(x$settings)
  if (is.null(control)) {
    control <- recorded
  } else {
    control <- .bl_check_control(control)
    if (!identical(.bl_control_key(control), .bl_control_key(recorded))) {
      .ig_abort("invalid_input", "`control` differs from the settings recorded in `x`; omit it to use those")
    }
  }
  if (inherits(x, "boschloo_test")) {
    d <- x$data
    info <- .bl_variant_info[[x$variant$variant]]
    n1 <- as.integer(d$n1); n2 <- as.integer(d$n2)
    fixed <- list(x1 = d$x1, n1 = n1, x2 = d$x2, n2 = n2, variant = info$variant,
                  variant_id = info$variant_id)
    title <- "Null probability of the tail set versus the common event probability"
    scope <- "observed data: tail set of the observed table"
    status <- .ig_status_from_list(unclass(x$status))
    if (status$code != "ok") {
      # No curve for a p-value that could not be certified.
      return(.bl_plotdata("boschloo_nuisance", title,
                          data.frame(side = character(), pi = numeric(), probability = numeric()),
                          fixed = fixed, scope = scope, notes = status$reason, status = status))
    }
    i <- as.integer(d$x1) + 1L + as.integer(d$x2) * (n1 + 1L)
    rows <- list()
    marks <- list()
    for (side in info$sides) {
      ord <- .bl_ordering(n1, n2, side, control)
      cvec <- .bl_tail_c(ord, .bl_level_tables(ord, ord$level[i]))
      s <- .bl_sup(cvec, control)
      pis <- sort(unique(c(seq(0, 1, length.out = grid_points), if (is.finite(s$argmax)) s$argmax)))
      rows[[side]] <- data.frame(side = side, pi = pis, probability = .bl_null_curve(cvec, pis))
      marks[[side]] <- list(argmax = s$argmax, supremum = s$value, upper = s$upper)
    }
    data <- do.call(rbind, rows)
    rownames(data) <- NULL
    return(.bl_plotdata("boschloo_nuisance", title, data, fixed = fixed, scope = scope,
                        reference = list(p_value = x$p_value, p_upper = x$p_upper, sides = marks,
                                         construction = info$construction),
                        status = status))
  }
  info <- .bl_variant_info[[x$variant$variant]]
  n1 <- as.integer(x$design$n1); n2 <- as.integer(x$design$n2)
  reg <- .bl_region(n1, n2, x$alpha, info, control)
  status <- .bl_region_status(reg)
  fixed <- list(n1 = n1, n2 = n2, alpha = x$alpha, variant = info$variant,
                variant_id = info$variant_id)
  title <- "Null rejection probability versus the common event probability"
  scope <- "hypothetical future study: rejection region of the design"
  if (status$code != "ok") {
    # No curve for a region that could not be certified.
    return(.bl_plotdata("boschloo_nuisance", title,
                        data.frame(side = character(), pi = numeric(), probability = numeric()),
                        fixed = fixed, scope = scope, reference = list(alpha = x$alpha),
                        notes = status$reason, status = status))
  }
  ord1 <- .bl_ordering(n1, n2, info$sides[1], control)
  cvec <- .bl_tail_c(ord1, reg$idx)
  pis <- sort(unique(c(seq(0, 1, length.out = grid_points),
                       if (is.finite(reg$size$argmax)) reg$size$argmax)))
  data <- data.frame(side = "region", pi = pis, probability = .bl_null_curve(cvec, pis))
  .bl_plotdata("boschloo_nuisance", title, data, fixed = fixed, scope = scope,
               reference = list(alpha = x$alpha, attained_size = reg$size$value,
                                attained_size_upper = reg$size$upper, argmax = reg$size$argmax),
               status = status)
}
