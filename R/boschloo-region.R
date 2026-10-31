# Extracted from inferguidR e91938f77eaa991b69989b121bfd32c124c95db0.
# Original source and MIT license: Raban Heller. See tools/extraction/source-map.csv.

.bl_region_side <- function(n1, n2, side, alpha, control) {
  key <- paste("region", n1, n2, side, .bl_cache_number(alpha), .bl_control_key(control), sep = "|")
  cached <- .bl_cache_get(key, control)
  if (!is.null(cached)) return(cached)
  ord <- .bl_ordering(n1, n2, side, control)
  L <- ord$n_levels
  sups <- list()
  evaluate <- function(j) {
    s <- .bl_sup(.bl_tail_c(ord, .bl_level_tables(ord, j)), control, alpha)
    sups[[as.character(j)]] <<- s
    s
  }
  # Invariant: level lo is admissible (lo = 0 is the empty region), level hi
  # is not (the full sample space has null probability 1 > alpha).
  lo <- 0L
  hi <- L
  while (hi - lo > 1L) {
    mid <- (lo + hi) %/% 2L
    s <- evaluate(mid)
    if (s$upper <= alpha) lo <- mid else hi <- mid
  }
  cut_sup <- if (lo == 0L) NULL else sups[[as.character(lo)]]
  next_sup <- if (hi >= L) NULL else sups[[as.character(hi)]] %||% evaluate(hi)
  idx <- .bl_level_tables(ord, lo)
  trace <- data.frame(level = as.integer(names(sups)),
                      value = vapply(sups, `[[`, 0, "value"),
                      upper = vapply(sups, `[[`, 0, "upper"),
                      row.names = NULL)
  out <- list(side = side, alpha = alpha, level = lo, n_levels = L, idx = idx,
              cut_sup = cut_sup, next_sup = next_sup,
              next_resolved = is.null(next_sup) || isTRUE(next_sup$resolved),
              converged = all(vapply(sups, function(s) isTRUE(s$converged), logical(1))),
              ambiguity_touches = .bl_ambiguity_touches(ord, lo, "cut"),
              arithmetic = ord$arithmetic, ambiguous_gaps = ord$ambiguous_gaps,
              threshold = if (lo > 0L) exp(max(ord$lt[idx])) else NA_real_,
              next_threshold = if (hi <= L && hi > lo) {
                exp(min(ord$lt[ord$perm[ord$level_start[hi]:ord$level_end[hi]]]))
              } else NA_real_,
              trace = trace[order(trace$level), , drop = FALSE])
  .bl_cache_set(key, out, control)
}

.bl_region <- function(n1, n2, alpha, info, control) {
  n1 <- as.integer(n1)
  n2 <- as.integer(n2)
  key <- paste("fullregion", n1, n2, info$variant, .bl_cache_number(alpha), .bl_control_key(control),
               sep = "|")
  cached <- .bl_cache_get(key, control)
  if (!is.null(cached)) return(cached)
  alpha_side <- if (info$variant == "central") alpha / 2 else alpha
  sides <- lapply(info$sides, function(s) .bl_region_side(n1, n2, s, alpha_side, control))
  names(sides) <- info$sides
  n_tab <- (n1 + 1L) * (n2 + 1L)
  member <- logical(n_tab)
  for (s in sides) member[s$idx] <- TRUE
  idx <- which(member)
  ord1 <- .bl_ordering(n1, n2, info$sides[1], control)
  size <- if (length(sides) == 1L) {
    sides[[1]]$cut_sup %||% .bl_sup(numeric(n1 + n2 + 1L), control)
  } else {
    .bl_sup(.bl_tail_c(ord1, idx), control)
  }
  problems <- character()
  for (s in sides) {
    if (isTRUE(s$ambiguity_touches)) {
      problems <- c(problems, sprintf(
        "the %s ordering has an ambiguous tie at the region boundary and n1 + n2 = %d exceeds %d, the limit of exact integer arithmetic",
        s$side, n1 + n2, .bl_exact_max_n))
    }
    if (!isTRUE(s$converged)) {
      problems <- c(problems, sprintf("a nuisance supremum of the %s ordering did not converge", s$side))
    }
  }
  if (!isTRUE(size$converged)) problems <- c(problems, "the attained-size supremum did not converge")
  warnings <- character()
  for (s in sides) {
    if (!isTRUE(s$next_resolved)) {
      warnings <- c(warnings, sprintf(
        "the first excluded %s level has a supremum within the floating-point allowance of alpha; it is excluded (conservative)",
        s$side))
    }
  }
  out <- list(n1 = n1, n2 = n2, alpha = alpha, info = info, sides = sides, member = member,
              idx = idx, size = size, problems = problems, warnings = warnings,
              x1 = ord1$x1, x2 = ord1$x2, t = ord1$t)
  .bl_cache_set(key, out, control)
}

.bl_power_idx <- function(idx, n1, n2, p1, p2) {
  if (!length(idx)) return(0)
  P <- outer(stats::dbinom(0:n1, n1, p1), stats::dbinom(0:n2, n2, p2))
  min(1, sum(P[idx]))
}

.bl_region_status <- function(reg) {
  if (length(reg$problems)) ig_status("numerical_failure", paste(reg$problems, collapse = "; ")) else ig_status("ok")
}

.bl_side_region_record <- function(s, ok) {
  num <- function(v) if (ok) v else NA_real_
  list(side = s$side,
       direction = if (s$side == "lower") "p1 < p2" else if (s$side == "upper") "p1 > p2" else "p1 != p2",
       alpha_side = s$alpha, last_level_in_region = s$level, n_levels = s$n_levels,
       tables_in_region = length(s$idx),
       ordering_threshold = num(s$threshold),
       next_ordering_value = num(s$next_threshold),
       p_at_boundary = if (is.null(s$cut_sup)) NULL else
         list(value = num(s$cut_sup$value), upper = num(s$cut_sup$upper)),
       p_first_excluded = if (is.null(s$next_sup)) NULL else
         list(value = num(s$next_sup$value), upper = num(s$next_sup$upper),
              resolved = isTRUE(s$next_sup$resolved)),
       ordering_arithmetic = s$arithmetic, ambiguous_gaps = s$ambiguous_gaps,
       binary_search = list(levels = s$trace$level, value = if (ok) s$trace$value else NA_real_,
                            upper = if (ok) s$trace$upper else NA_real_))
}

boschloo_region <- function(n1, n2, alpha = 0.05, variant, control = boschloo_control()) {
  info <- .bl_variant(if (missing(variant)) NULL else variant)
  .bl_check_design(n1, n2)
  alpha <- .bl_check_alpha(alpha)
  control <- .bl_check_control(control)
  reg <- .bl_region(n1, n2, alpha, info, control)
  .bl_region_object(reg, control)
}

.bl_region_object <- function(reg, control) {
  status <- .bl_region_status(reg)
  ok <- status$code == "ok"
  num <- function(v) if (ok) v else NA_real_
  rejected <- data.frame(x1 = reg$x1[reg$idx], x2 = reg$x2[reg$idx])
  if (!ok) rejected <- rejected[0, , drop = FALSE]
  body <- list(
    object_type = "boschloo_region",
    status = .ig_status_to_list(status),
    variant = .bl_variant_record(reg$info),
    design = list(n1 = reg$n1, n2 = reg$n2, n_total = reg$n1 + reg$n2,
                  n_tables = length(reg$member)),
    alpha = reg$alpha,
    rejected = rejected,
    n_rejected = if (ok) length(reg$idx) else NA_integer_,
    size = list(value = num(reg$size$value), upper = num(reg$size$upper),
                argmax = num(reg$size$argmax), converged = isTRUE(reg$size$converged),
                note = "attained size = sup over the common event probability of the null rejection probability"),
    sides = lapply(reg$sides, .bl_side_region_record, ok = ok),
    warnings = reg$warnings,
    undefined = if (ok) .ig_empty_object() else list(region = status$reason),
    settings = .bl_control_record(control),
    scope = "hypothetical future study with fixed group sizes n1 and n2",
    provenance = .bl_provenance()
  )
  body$region_hash <- .ig_hash_value(list(design = body$design, alpha = body$alpha,
                                          variant_id = reg$info$variant_id,
                                          settings = body$settings,
                                          rejected = list(x1 = I(rejected$x1), x2 = I(rejected$x2))))
  structure(body, class = c("boschloo_region", "ig_frozen"))
}

format.boschloo_region <- function(x, ...) {
  lines <- c(sprintf("<boschloo_region> %s", x$variant$label),
             sprintf("  design:      n1 = %d, n2 = %d (%d tables); alpha = %s",
                     as.integer(x$design$n1), as.integer(x$design$n2),
                     as.integer(x$design$n_tables), format(x$alpha)),
             sprintf("  status:      %s", format(.ig_status_from_list(x$status))))
  if (identical(x$status$code, "ok")) {
    lines <- c(lines,
               sprintf("  rejected:    %d tables", as.integer(x$n_rejected)),
               sprintf("  attained size: %s (upper bound %s) at pi = %s", .bl_fmt(x$size$value, 7),
                       .bl_fmt(x$size$upper, 7), .bl_fmt(x$size$argmax, 5)))
  }
  c(lines, if (length(x$warnings)) paste0("  warning: ", x$warnings),
    sprintf("  region hash: %s", x$region_hash))
}

print.boschloo_region <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

.bl_fisher_p <- function(n1, n2, info, control, convention = "same") {
  if (convention == "minlike" || info$variant == "minlike") {
    return(exp(.bl_ordering(n1, n2, "minlike", control)$lt))
  }
  if (info$variant == "central") {
    tl <- exp(.bl_ordering(n1, n2, "lower", control)$lt)
    tu <- exp(.bl_ordering(n1, n2, "upper", control)$lt)
    return(pmin(1, 2 * pmin(tl, tu)))
  }
  exp(.bl_ordering(n1, n2, info$sides, control)$lt)
}

boschloo_compare_fisher <- function(n1, n2, alpha = 0.05, variant, p1 = NULL, p2 = NULL,
                                    fisher_convention = c("same", "minlike"),
                                    control = boschloo_control()) {
  info <- .bl_variant(if (missing(variant)) NULL else variant)
  .bl_check_design(n1, n2)
  alpha <- .bl_check_alpha(alpha)
  fisher_convention <- match.arg(fisher_convention)
  if (is.null(p1) != is.null(p2)) .ig_abort_invalid("give both `p1` and `p2`, or neither")
  if (!is.null(p1)) {
    p1 <- .bl_check_prob(p1, "p1")
    p2 <- .bl_check_prob(p2, "p2")
  }
  control <- .bl_check_control(control)
  n1 <- as.integer(n1)
  n2 <- as.integer(n2)
  reg <- .bl_region(n1, n2, alpha, info, control)
  status <- .bl_region_status(reg)
  fp <- .bl_fisher_p(n1, n2, info, control, fisher_convention)
  fisher_member <- fp <= alpha * (1 + control$tie_tolerance)
  ord1 <- .bl_ordering(n1, n2, info$sides[1], control)
  fisher_size <- .bl_sup(.bl_tail_c(ord1, which(fisher_member)), control)
  cat_of <- ifelse(reg$member & fisher_member, "both",
                   ifelse(reg$member, "boschloo_only", ifelse(fisher_member, "fisher_only", "neither")))
  same <- fisher_convention == "same" || info$variant == "minlike"
  # Boschloo p-values of every table (one supremum per ordering level). The
  # dominance property p_Boschloo <= p_Fisher is checked on the p-values:
  # region decisions alone could show a "Fisher only" table when a p-value
  # lies within the numerical tolerance of alpha.
  n_tab <- length(reg$member)
  pv <- if (n_tab <= 5000L && status$code == "ok") .bl_pvalues_all(n1, n2, info, control) else NULL
  dominance <- NA
  if (same && !is.null(pv)) {
    dominance <- all(pv$value <= fp * (1 + control$tie_tolerance) + .bl_fp_allowance(n1 + n2))
  }
  ok <- status$code == "ok"
  if (!ok) cat_of <- rep(NA_character_, n_tab)    # no Boschloo decision without a certified region
  tables <- data.frame(x1 = reg$x1, x2 = reg$x2, fisher_p = fp,
                       boschloo_p = if (is.null(pv)) NA_real_ else pv$value,
                       boschloo_reject = if (ok) reg$member else NA,
                       fisher_reject = fisher_member, category = cat_of, stringsAsFactors = FALSE)
  counts <- vapply(c("both", "boschloo_only", "fisher_only", "neither"),
                   function(k) if (ok) sum(cat_of == k) else NA_integer_, integer(1))
  num <- function(v) if (ok) v else NA_real_
  out <- list(
    object_type = "boschloo_fisher_comparison",
    status = .ig_status_to_list(status),
    variant = .bl_variant_record(info),
    fisher_test = list(
      convention = if (same) "same sidedness convention as the Boschloo variant" else
        "two-sided Fisher p-value of stats::fisher.test (minlike); a different convention from the central Boschloo variant",
      conditional_on = "both margins (total number of events fixed)"),
    design = list(n1 = n1, n2 = n2), alpha = alpha,
    tables = tables, counts = counts,
    dominance_holds = dominance,
    dominance_note = if (!same) "not applicable across different two-sided conventions" else
      if (is.null(pv)) "not checked (more than 5000 tables, or the region has no ok status)" else
        paste("checked on the p-values of every table: Boschloo p <= Fisher p for the same ordering",
              "statistic (a 'fisher_only' table can only arise within numerical tolerance of alpha)"),
    size = list(boschloo = list(value = num(reg$size$value), upper = num(reg$size$upper)),
                fisher = list(value = fisher_size$value, upper = fisher_size$upper)),
    power = if (is.null(p1)) NULL else list(
      p1 = p1, p2 = p2,
      boschloo = num(.bl_power_idx(reg$idx, n1, n2, p1, p2)),
      fisher = .bl_power_idx(which(fisher_member), n1, n2, p1, p2)),
    undefined = if (ok) .ig_empty_object() else list(boschloo = status$reason),
    settings = .bl_control_record(control),
    scope = "hypothetical future study; comparison of two different tests on the same design"
  )
  structure(out, class = "boschloo_fisher_comparison")
}

print.boschloo_fisher_comparison <- function(x, ...) {
  cat(sprintf("<boschloo_fisher_comparison> %s vs Fisher (%s)\n", x$variant$label, x$fisher_test$convention),
      sprintf("  design n1 = %d, n2 = %d, alpha = %s\n", x$design$n1, x$design$n2, format(x$alpha)),
      sprintf("  rejected by both: %d; Boschloo only: %d; Fisher only: %d; neither: %d\n",
              x$counts[["both"]], x$counts[["boschloo_only"]], x$counts[["fisher_only"]],
              x$counts[["neither"]]),
      sprintf("  attained size: Boschloo %s, Fisher %s\n", .bl_fmt(x$size$boschloo$value),
              .bl_fmt(x$size$fisher$value)), sep = "")
  if (!is.null(x$power)) {
    cat(sprintf("  power at p1 = %s, p2 = %s: Boschloo %s, Fisher %s\n", format(x$power$p1),
                format(x$power$p2), .bl_fmt(x$power$boschloo), .bl_fmt(x$power$fisher)))
  }
  invisible(x)
}
