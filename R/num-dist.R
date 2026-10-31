# Extracted from inferguidR e91938f77eaa991b69989b121bfd32c124c95db0.
# Original source and MIT license: Raban Heller. See tools/extraction/source-map.csv.

.ig_pnt_ncp_limit <- sqrt(2 * log(2) * 1021)

.ig_pnt_df_limit <- 2e4

.ig_t_tail_switch <- 1e-5

.ig_log1mexp <- function(x) {
  out <- rep(NaN, length(x))
  ok <- !is.na(x) & x <= 0
  small <- ok & x > -log(2)
  out[small] <- log(-expm1(x[small]))
  big <- ok & !small
  out[big] <- log1p(-exp(x[big]))
  out[!is.na(x) & x == 0] <- -Inf
  out[is.na(x)] <- NA_real_
  out
}

.ig_logspace_add <- function(a, b) {
  hi <- pmax(a, b)
  lo <- pmin(a, b)
  out <- hi + log1p(exp(lo - hi))
  out[is.infinite(hi) & hi < 0] <- -Inf
  out[is.infinite(hi) & hi > 0] <- Inf
  out
}

.ig_logspace_sum <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(-Inf)
  mx <- max(x)
  if (!is.finite(mx)) return(mx)
  mx + log(sum(exp(x - mx)))
}

.ig_from_log <- function(lp, log_p) if (log_p) lp else exp(lp)

.ig_map_scalar <- function(fun, ...) {
  args <- list(...)
  n <- max(lengths(args))
  if (any(lengths(args) == 0L)) return(numeric(0))
  args <- lapply(args, rep_len, length.out = n)
  vapply(seq_len(n), function(i) do.call(fun, lapply(args, `[[`, i)), numeric(1))
}

.ig_p_norm <- function(q, mean = 0, sd = 1, lower_tail = TRUE, log_p = FALSE) {
  stats::pnorm(q, mean = mean, sd = sd, lower.tail = lower_tail, log.p = log_p)
}

.ig_q_norm <- function(p, mean = 0, sd = 1, lower_tail = TRUE, log_p = FALSE) {
  stats::qnorm(p, mean = mean, sd = sd, lower.tail = lower_tail, log.p = log_p)
}

.ig_p_t <- function(q, df, ncp = 0, lower_tail = TRUE, log_p = FALSE) {
  .ig_map_scalar(function(q, df, ncp) .ig_p_t1(q, df, ncp, lower_tail, log_p), q, df, ncp)
}

.ig_p_t1 <- function(q, df, ncp, lower_tail, log_p) {
  if (is.na(q) || is.na(df) || is.na(ncp)) return(NA_real_)
  if (df <= 0 || !is.finite(ncp)) return(NaN)
  if (is.infinite(q)) {
    one <- (q > 0) == lower_tail
    return(if (log_p) (if (one) 0 else -Inf) else as.double(one))
  }
  if (ncp == 0) return(stats::pt(q, df, lower.tail = lower_tail, log.p = log_p))
  if (is.infinite(df)) {
    return(stats::pnorm(q, mean = ncp, lower.tail = lower_tail, log.p = log_p))
  }
  if (q == 0) return(stats::pnorm(0, mean = ncp, lower.tail = lower_tail, log.p = log_p))
  # On the log scale a probability above one half is taken from the other
  # (small, accurately computed) tail.
  if (log_p) {
    p <- .ig_p_t1(q, df, ncp, lower_tail, FALSE)
    if (!is.na(p) && p > 0.5) return(log1p(-.ig_p_t1(q, df, ncp, !lower_tail, FALSE)))
  }
  # Use the series only within its checked precision range. A large tail
  # comes from a checked small complement, never a value above one.
  if (abs(ncp) <= .ig_pnt_ncp_limit && df <= .ig_pnt_df_limit) {
    p <- suppressWarnings(stats::pt(q, df, ncp = ncp, lower.tail = lower_tail))
    if (is.finite(p) && p >= .ig_t_tail_switch && p <= 0.5) {
      return(if (log_p) log(p) else p)
    }
    if (is.finite(p) && p > 0.5) {
      other <- suppressWarnings(stats::pt(q, df, ncp = ncp, lower.tail = !lower_tail))
      if (is.finite(other) && other >= .ig_t_tail_switch && other <= 0.5) {
        return(if (log_p) log1p(-other) else 1 - other)
      }
    }
  }
  # On the probability scale a tail that is provably below the smallest
  # double is 0, and its complement is 1, without any integration. Scans over
  # very large effects (ncp of 1e5 and more) would otherwise spend seconds
  # integrating probabilities such as exp(-1e16) that cannot be represented.
  if (!log_p) {
    if (.ig_t_tail_log_bound(q, df, ncp, lower_tail) < .ig_log_underflow) return(0)
    if (.ig_t_tail_log_bound(q, df, ncp, !lower_tail) < .ig_log_eps8) return(1)
  }
  # Integration resolves the smaller tail to a small relative error. The
  # larger tail is taken as the complement of the smaller one, so that it
  # never exceeds one through quadrature error.
  lp <- .ig_p_t_integral(q, df, ncp, lower_tail)
  if (lp > log(0.5)) {
    lp_other <- .ig_p_t_integral(q, df, ncp, !lower_tail)
    if (lp_other <= log(0.5)) lp <- .ig_log1mexp(lp_other)
  }
  .ig_from_log(lp, log_p)
}

.ig_log_underflow <- -1075 * log(2) - 1

.ig_log_eps8 <- log(.Machine$double.eps / 8)

.ig_t_tail_log_bound <- function(q, df, ncp, lower_tail) {
  if (!lower_tail) {
    q <- -q
    ncp <- -ncp
  }
  if (!(ncp > 0)) return(0)
  if (q <= 0) return(stats::pnorm(-ncp, log.p = TRUE))
  .ig_logspace_add(stats::pnorm(-ncp / 2, log.p = TRUE),
                   stats::pchisq(df * (ncp / (2 * q))^2, df, lower.tail = FALSE, log.p = TRUE))
}

.ig_p_t_integral <- function(q, df, ncp, lower_tail) {
  if (q < 0) return(.ig_p_t_integral(-q, df, -ncp, !lower_tail))
  inner_lower <- !lower_tail
  logf <- function(w) {
    stats::dnorm(w - ncp, log = TRUE) +
      stats::pchisq(df * (w / q)^2, df, lower.tail = inner_lower, log.p = TRUE)
  }
  # Mode of the (log-concave for df >= 1) integrand; the search interval
  # covers the normal factor's mass and the region where the chi-square
  # factor turns over.
  hi <- max(0, ncp) + 40 + 2 * q
  opt <- stats::optimize(function(w) {
    v <- logf(w)
    if (is.finite(v)) -v else 1e300
  }, c(0, hi), tol = 1e-10)
  m <- opt$minimum
  g <- seq(0, hi, length.out = 201L)
  v <- logf(g)
  if (any(is.finite(v)) && (!is.finite(logf(m)) || max(v[is.finite(v)]) > logf(m))) {
    m <- g[which.max(v)]
  }
  # The chi-square factor switches from 0 to 1 around w = q over a width of
  # about q / sqrt(2 df); for large df this is a near-step that the
  # quadrature must see, so it gets its own cut points.
  s <- q / sqrt(2 * df)
  step_cuts <- q + c(-1, 1) %o% (s * 4^(-1:4))
  tail_int <- .ig_integrate_log_peak(logf, 0, Inf, m, extra_cuts = c(q, step_cuts))
  if (lower_tail) {
    .ig_logspace_add(stats::pnorm(-ncp, log.p = TRUE), tail_int)
  } else {
    tail_int
  }
}

.ig_integrate_log_peak <- function(logf, lower, upper, mode, extra_cuts = numeric(0)) {
  lm <- logf(mode)
  if (!is.finite(lm)) return(-Inf)
  h <- 1e-5 * max(1, abs(mode))
  curv <- (logf(mode + h) - 2 * lm + logf(max(lower, mode - h))) / h^2
  width <- if (is.finite(curv) && curv < 0) min(1 / sqrt(-curv), 1e3) else 1
  offsets <- width * 4^(-2:6)
  cuts <- sort(unique(c(lower, mode - offsets, mode, mode + offsets, extra_cuts, upper)))
  cuts <- cuts[cuts >= lower & cuts <= upper]
  g <- function(w) exp(logf(w) - lm)
  pieces <- lapply(seq_len(length(cuts) - 1L), function(i) {
    stats::integrate(g, cuts[i], cuts[i + 1L], rel.tol = 1e-12, abs.tol = 0,
                     subdivisions = 2000L, stop.on.error = FALSE)
  })
  values <- vapply(pieces, `[[`, numeric(1), "value")
  errors <- vapply(pieces, `[[`, numeric(1), "abs.error")
  total <- sum(values)
  if (!(total > 0)) return(-Inf)
  # The integrand exp(logf(w) - lm) inherits the rounding error of logf,
  # about |lm| * eps in absolute terms, i.e. a relative error of |lm| * eps
  # in the result. A probability such as exp(-1e9) therefore cannot be
  # computed to better than about 1e-7 relative accuracy in double
  # precision, however good the quadrature. Quadrature error estimates
  # below that attainable level (and below 1e-10 in any case) are not
  # reported.
  attainable <- max(1e-10, 100 * .Machine$double.eps * abs(lm))
  # A quadrature message matters only if the piece can move the total: a
  # flagged piece whose error estimate is negligible relative to the total
  # (typically an infinite tail where the integrand underflows, or roundoff
  # far out on a narrow peak) is ignored. A piece that stopped for another
  # reason (subdivision limit, divergence, bad integrand behaviour) and holds
  # a non-negligible share of the total is also reported when its own
  # relative error is large, because its error estimate is then not
  # trustworthy. Roundoff alone is judged by its share of the total only, so
  # that the reported relative error and the decision to warn agree.
  messages <- vapply(pieces, function(x) as.character(x$message), character(1))
  flagged <- messages != "OK"
  roundoff <- grepl("roundoff", messages, fixed = TRUE)
  material <- flagged & (errors > attainable * total |
                           !roundoff & values > attainable * total & errors > attainable * values)
  if (any(material)) {
    i <- which(material)[1]
    warning(sprintf("tail-probability quadrature: %s (estimated relative error %.1e)",
                    pieces[[i]]$message, sum(errors[material]) / total), call. = FALSE)
  }
  log(total) + lm
}

.ig_q_t <- function(p, df, ncp = 0, lower_tail = TRUE, log_p = FALSE) {
  .ig_map_scalar(function(p, df, ncp) {
    if (is.na(p) || is.na(df) || is.na(ncp)) return(NA_real_)
    if (ncp == 0) return(stats::qt(p, df, lower.tail = lower_tail, log.p = log_p))
    if (is.infinite(df)) {
      return(stats::qnorm(p, mean = ncp, lower.tail = lower_tail, log.p = log_p))
    }
    start <- stats::qnorm(p, mean = ncp, lower.tail = lower_tail, log.p = log_p)
    .ig_invert_cdf(function(x, lt) .ig_p_t1(x, df, ncp, lt, TRUE), p, lower_tail, log_p,
                   support = c(-Inf, Inf), start = start)
  }, p, df, ncp)
}

.ig_p_chisq <- function(q, df, ncp = 0, lower_tail = TRUE, log_p = FALSE) {
  .ig_map_scalar(function(q, df, ncp) {
    if (is.na(q) || is.na(df) || is.na(ncp)) return(NA_real_)
    if (df <= 0 || ncp < 0 || !is.finite(ncp) || is.infinite(df)) return(NaN)
    if (ncp == 0) return(stats::pchisq(q, df, lower.tail = lower_tail, log.p = log_p))
    if (q <= 0 || is.infinite(q)) {
      one <- (q > 0) == lower_tail
      return(if (log_p) (if (one) 0 else -Inf) else as.double(one))
    }
    lp <- .ig_poisson_mixture_log(ncp / 2, function(j) {
      stats::pchisq(q, df + 2 * j, lower.tail = lower_tail, log.p = TRUE)
    })
    .ig_from_log(lp, log_p)
  }, q, df, ncp)
}

.ig_q_chisq <- function(p, df, ncp = 0, lower_tail = TRUE, log_p = FALSE) {
  .ig_map_scalar(function(p, df, ncp) {
    if (is.na(p) || is.na(df) || is.na(ncp)) return(NA_real_)
    if (ncp == 0) return(stats::qchisq(p, df, lower.tail = lower_tail, log.p = log_p))
    start <- df + ncp
    .ig_invert_cdf(function(x, lt) .ig_p_chisq(x, df, ncp, lt, TRUE), p, lower_tail, log_p,
                   support = c(0, Inf), start = start)
  }, p, df, ncp)
}

.ig_p_f <- function(q, df1, df2, ncp = 0, lower_tail = TRUE, log_p = FALSE) {
  .ig_map_scalar(function(q, df1, df2, ncp) {
    if (is.na(q) || is.na(df1) || is.na(df2) || is.na(ncp)) return(NA_real_)
    if (df1 <= 0 || df2 <= 0 || ncp < 0 || !is.finite(ncp) || is.infinite(df1)) return(NaN)
    if (is.infinite(df2)) {
      # F * df1 is chi-square(df1, ncp) in the limit.
      return(.ig_p_chisq(q * df1, df1, ncp, lower_tail, log_p))
    }
    if (ncp == 0) return(stats::pf(q, df1, df2, lower.tail = lower_tail, log.p = log_p))
    if (q <= 0 || is.infinite(q)) {
      one <- (q > 0) == lower_tail
      return(if (log_p) (if (one) 0 else -Inf) else as.double(one))
    }
    # A rigorous bound can establish probability-scale underflow without a
    # huge Poisson window. It is not the value of a requested log lower tail.
    if (ncp > 1000 && .ig_f_lower_log_bound(q, df1, df2, ncp) < -745.2 &&
        (!log_p || !lower_tail)) {
      return(if (lower_tail) 0 else if (log_p) 0 else 1)
    }
    # Use the smaller beta argument for both tails, avoiding cancellation
    # when q * df1 / df2 is extreme (and overflow in that product).
    log_ratio <- log(q) + log(df1) - log(df2)
    swapped <- log_ratio > 0
    z <- stats::plogis(-abs(log_ratio))
    tail_log <- function(lower) .ig_poisson_mixture_log(ncp / 2, function(j) {
      if (swapped) stats::pbeta(z, df2 / 2, df1 / 2 + j, lower.tail = !lower, log.p = TRUE)
      else stats::pbeta(z, df1 / 2 + j, df2 / 2, lower.tail = lower, log.p = TRUE)
    })
    small_lower <- log(q) + log(df1) < log(df1 + ncp)
    lp <- tail_log(small_lower)
    if (is.na(lp)) return(NaN)
    if (lp > -log(2)) {
      small_lower <- !small_lower
      lp <- tail_log(small_lower)
    }
    if (is.na(lp) || lp > 0) return(NaN)
    .ig_from_log(if (lower_tail == small_lower) lp else .ig_log1mexp(lp), log_p)
  }, q, df1, df2, ncp)
}

.ig_f_lower_log_bound <- function(crit, df1, df2, ncp) {
  if (!is.finite(crit) || crit <= 0 || !is.finite(ncp) || ncp <= 0 ||
      !is.finite(df1) || df1 <= 0 || !is.finite(df2) || df2 <= 0) return(0)
  # d - log(1+d) >= d^2 / (2*(1+d)). With k=3000/df2 and
  # d=k+sqrt(k), d^2 >= k*(1+d), hence the denominator bound
  # is at most exp(-750). No root tolerance is needed at huge df2.
  k <- 3000 / df2
  d <- k + sqrt(k)
  if (!is.finite(d)) return(0)
  a <- crit * df1 * (1 + d + 8 * .Machine$double.eps)
  u <- (df1 + sqrt(df1^2 + 4 * a * ncp)) / (2 * a)
  if (!is.finite(u) || u <= 1) return(0)
  th <- (u - 1) / 2
  lb1 <- th * a - ncp * th / u - (df1 / 2) * log(u)
  .ig_logspace_add(min(lb1, 0), -750)
}

.ig_q_f <- function(p, df1, df2, ncp = 0, lower_tail = TRUE, log_p = FALSE) {
  .ig_map_scalar(function(p, df1, df2, ncp) {
    if (is.na(p) || is.na(df1) || is.na(df2) || is.na(ncp)) return(NA_real_)
    if (ncp == 0) {
      lp <- if (log_p) p else suppressWarnings(log(p))
      # Preserve R's boundary, invalid-input and infinite-df limits.
      if (!is.finite(lp) || lp >= 0 || df1 <= 0 || df2 <= 0 ||
          !is.finite(df1) || !is.finite(df2)) {
        return(stats::qf(p, df1, df2, lower.tail = lower_tail, log.p = log_p))
      }
      # F = (df2/df1) B/(1-B). Compute the smaller beta component
      # directly, avoiding both qf's large-df approximation and 1-B
      # cancellation. Keep log probabilities intact, including p < 1e-308.
      b <- suppressWarnings(stats::qbeta(p, df1 / 2, df2 / 2,
                                         lower.tail = lower_tail, log.p = log_p))
      crit <- if (is.finite(b) && b <= 0.5) {
        df2 / df1 * b / (1 - b)
      } else {
        w <- suppressWarnings(stats::qbeta(p, df2 / 2, df1 / 2,
                                           lower.tail = !lower_tail, log.p = log_p))
        df2 / df1 * (1 - w) / w
      }
      logtail <- function(x, lt) stats::pf(x, df1, df2, lower.tail = lt, log.p = TRUE)
      residual <- logtail(crit, lower_tail) - lp
      if (is.finite(crit) && crit > 0 && is.finite(residual) &&
          abs(residual) <= 1e-10 * max(1, abs(lp))) return(crit)
      # Refine on log(x), so quantiles near zero retain relative accuracy.
      u <- .ig_invert_cdf(function(u, lt) logtail(exp(u), lt), p, lower_tail, log_p,
                          support = c(-Inf, Inf),
                          start = if (is.finite(crit) && crit > 0) log(crit) else 0)
      crit <- exp(u)
      residual <- logtail(crit, lower_tail) - lp
      if (is.finite(residual) && abs(residual) <= 1e-8 * max(1, abs(lp))) crit else NaN
    } else {
      start <- (df1 + ncp) / df1
      .ig_invert_cdf(function(x, lt) .ig_p_f(x, df1, df2, ncp, lt, TRUE), p, lower_tail, log_p,
                     support = c(0, Inf), start = start)
    }
  }, p, df1, df2, ncp)
}

.ig_poisson_mixture_log <- function(m, log_term, max_terms = 2e7) {
  if (!is.finite(m) || m < 0 || !is.finite(max_terms) || max_terms < 1) return(NaN)
  half <- ceiling(10 * sqrt(m + 1)) + 20
  lo <- max(0, floor(m) - half)
  hi <- floor(m) + half
  if (hi - lo + 1 > max_terms) return(NaN)
  term <- function(j) stats::dpois(j, m, log = TRUE) + log_term(j)
  j <- seq.int(lo, hi)
  v <- term(j)
  cut <- log(1e-20)
  repeat {
    mx <- max(v[is.finite(v)], -Inf)
    if (!is.finite(mx)) break
    grow_left <- lo > 0 && is.finite(v[1L]) && v[1L] - mx > cut
    grow_right <- is.finite(v[length(v)]) && v[length(v)] - mx > cut
    if (!grow_left && !grow_right) break
    step <- max(half, length(v))
    additional <- (if (grow_left) min(step, lo) else 0) + (if (grow_right) step else 0)
    if (length(v) + additional > max_terms) return(NaN)
    if (grow_left) {
      new_lo <- max(0, lo - step)
      jl <- seq.int(new_lo, lo - 1)
      v <- c(term(jl), v)
      lo <- new_lo
    }
    if (grow_right) {
      jr <- seq.int(hi + 1, hi + step)
      v <- c(v, term(jr))
      hi <- hi + step
    }
  }
  .ig_logspace_sum(v)
}

.ig_p_binom <- function(q, size, prob, lower_tail = TRUE, log_p = FALSE) {
  stats::pbinom(q, size, prob, lower.tail = lower_tail, log.p = log_p)
}

.ig_d_binom <- function(x, size, prob, log = FALSE) {
  stats::dbinom(x, size, prob, log = log)
}

.ig_invert_cdf <- function(logcdf, p, lower_tail, log_p, support, start) {
  lp <- if (log_p) p else log(p)
  if (is.na(lp) || lp > 0) return(NaN)
  if (lp == 0) return(if (lower_tail) support[2] else support[1])
  if (lp == -Inf) return(if (lower_tail) support[1] else support[2])
  h <- function(x) {
    v <- logcdf(x, lower_tail) - lp
    if (lower_tail) v else -v # increasing in x either way
  }
  if (!is.finite(start)) start <- if (is.finite(support[1])) support[1] + 1 else 0
  step <- max(1, abs(start))
  a <- start
  b <- start
  fa <- h(a)
  fb <- fa
  for (i in seq_len(200L)) {
    if (fa <= 0 && fb >= 0) break
    if (fa > 0) {
      a <- if (is.finite(support[1])) support[1] + (a - support[1]) / 2 else a - step
      fa <- h(a)
    }
    if (fb < 0) {
      b <- b + step
      fb <- h(b)
    }
    step <- step * 2
  }
  if (is.na(fa) || is.na(fb) || !(fa <= 0 && fb >= 0)) return(NaN)
  if (fa == 0) return(a)
  if (fb == 0) return(b)
  stats::uniroot(h, c(a, b), f.lower = fa, f.upper = fb, tol = 1e-13 * max(1, abs(a), abs(b)),
                 maxiter = 1000L)$root
}
