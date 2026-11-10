# Benchmark cases for the reference engines (development only).
#
# Every input comes from the original function's help-page example or from the
# method's entry in inst/methods/synthetic-baseline.rds. A case records where its
# input comes from. No input is made up for a benchmark.
#
# A case is list(case_id, source, spec = list(alternative, conf.level, options),
# args = <run_inference() arguments>, fields = <native fields to compare>).

baseline_case <- function(method_id, fields, case_id = "synthetic-baseline") {
  fixtures <- readRDS(file.path("inst", "methods", "synthetic-baseline.rds"))
  a <- fixtures[[method_id]]$arguments
  list(case_id = case_id, source = paste0("inst/methods/synthetic-baseline.rds: ", method_id),
    spec = list(alternative = if (is.null(a$alternative)) "two.sided" else a$alternative,
      conf.level = if (is.null(a$conf.level)) 0.95 else a$conf.level, options = list()),
    args = a[setdiff(names(a), c("alternative", "conf.level"))], fields = fields)
}

# DescTools CochranQTest help example: drug response data from the SAS/STAT
# FREQ documentation (F = favourable coded 1, U = unfavourable coded 0), built
# with expand.grid in the same cell order as the example's xtabs call.
cochran_q_help_data <- function() {
  cells <- expand.grid(A = c(1, 0), B = c(1, 0), C = c(1, 0))
  x <- cells[rep(seq_len(8L), c(6, 2, 2, 6, 16, 4, 4, 6)), ]
  data.frame(id = sprintf("subject_%02d", seq_len(nrow(x))), x, row.names = NULL)
}

bench_case <- function(case_id, source, args, fields, alternative = "two.sided", conf.level = 0.95)
  list(case_id = case_id, source = source, spec = list(alternative = alternative, conf.level = conf.level,
    options = list()), args = args, fields = fields)

# DescTools CochranArmitageTest help examples. "dose": PharmaSUG 2007 SP05 p. 4,
# responders per dose 0..3 out of 10. "lungtumor": tumours per dose 0..2.
trend_args <- function(events, totals, labels) {
  names(events) <- names(totals) <- labels
  list(events = events, totals = totals, scores = stats::setNames(seq_along(labels) - 1, labels),
    scores_prespecified = TRUE)
}

# DescTools JonckheereTerpstraTest help examples, as data frames with an
# explicit group order. Example 1 draws its outcome with set.seed(1234).
jt_args <- function(y, group, group_order, seed = 1234L, B = 1999L)
  list(data = data.frame(id = sprintf("unit_%02d", seq_along(y)), group = as.character(group), y = y),
    group_order = group_order, order_prespecified = TRUE, independent_units = TRUE, seed = seed, B = B)
jt_help_example <- function(tied = FALSE) {
  set.seed(1234)
  g <- ordered(rep(1:5, rep(10, 5)))
  x <- stats::rnorm(50) + 0.3 * as.numeric(g)
  if (tied) x[1:2] <- mean(x[1:2])
  jt_args(x, g, as.character(1:5))
}

# exact2x2 mcnemarExactDP help example: airway hyperresponsiveness before (x)
# and after (y) stem cell transplantation, Bentur et al. (2009), also Table II
# of Fagerland, Lydersen and Laake. AHR is coded 1.
ahr_counts <- function() c(n00 = 12, n01 = 7, n10 = 1, n11 = 1)
mcnemar_cases <- function(method_id, fields) {
  src <- "exact2x2 1.7.0 ?mcnemarExactDP example (Bentur et al. 2009 AHR pairs)"
  no_discordant <- ahr_counts()
  no_discordant[c("n01", "n10")] <- 0
  c(lapply(c("two.sided", "greater", "less"), function(alt)
      bench_case(paste0("help-ahr-", alt), src, list(counts = ahr_counts()), fields, alternative = alt)),
    list(bench_case("help-ahr-no-discordant", paste(src, "with discordant pairs removed (degenerate case)"),
        list(counts = no_discordant), "status"),
      baseline_case(method_id, fields)))
}

benchmark_cases <- list(
  mcnemar_exact_conditional = function()
    mcnemar_cases("mcnemar_exact_conditional", c("status", "p_value", "confidence_interval")),
  mcnemar_midp = function() mcnemar_cases("mcnemar_midp", c("status", "p_value")),
  cochran_armitage_trend = function() {
    ca_fields <- c("status", "signed_statistic", "statistic", "p_value")
    dose <- "DescTools 0.99.60 ?CochranArmitageTest example 1 (PharmaSUG 2007 SP05 dose data)"
    dose_args <- trend_args(c(0, 1, 0, 3), rep(10, 4), paste0("dose_", 0:3))
    no_events <- dose_args
    no_events$events[] <- 0
    c(lapply(c("two.sided", "greater", "less"), function(alt)
        bench_case(paste0("help-dose-", alt), dose, dose_args, ca_fields, alternative = alt)),
      list(bench_case("help-lungtumor", "DescTools 0.99.60 ?CochranArmitageTest example 2 (lungtumor data)",
          trend_args(c(2, 7, 15), c(40, 50, 48), paste0("dose_", 0:2)), ca_fields),
        bench_case("help-dose-no-responders", paste(dose, "with responders removed (degenerate case)"),
          no_events, "status"),
        baseline_case("cochran_armitage_trend", ca_fields)))
  },
  jonckheere_terpstra = function() {
    jt_fields <- c("status", "statistic", "p_value")
    src <- "DescTools 0.99.60 ?JonckheereTerpstraTest example"
    coffee <- list(c_4 = c(447, 396, 383, 410), c_2 = c(438, 521, 468, 391, 504, 472),
      c_0 = c(513, 543, 506, 489, 407))
    breaking <- data.frame(speed = c(20, 25, 25, 25, 25, 30, 30, 30, 35, 35),
      distance = c(48, 33, 59, 48, 56, 60, 101, 67, 85, 107))
    list(bench_case("help-normal", paste(src, "1 (set.seed(1234))"), jt_help_example(), jt_fields),
      bench_case("help-normal-greater", paste(src, "1 (set.seed(1234))"), jt_help_example(), jt_fields,
        alternative = "greater"),
      bench_case("help-normal-less", paste(src, "1 (set.seed(1234))"), jt_help_example(), jt_fields,
        alternative = "less"),
      bench_case("help-tied", paste(src, "1 with tied data"), jt_help_example(tied = TRUE), jt_fields),
      bench_case("help-coffee", paste(src, "coffee (Duller p. 222)"),
        jt_args(unlist(coffee, use.names = FALSE), rep(names(coffee), lengths(coffee)), names(coffee)), jt_fields),
      bench_case("help-breaking-greater", paste(src, "breaking distance, alternative increasing"),
        jt_args(breaking$distance, breaking$speed, as.character(c(20, 25, 30, 35))), jt_fields,
        alternative = "greater"),
      baseline_case("jonckheere_terpstra", jt_fields))
  },
  cochran_q = function() {
    q_fields <- c("status", "statistic", "df", "p_value")
    help <- cochran_q_help_data()
    concordant <- help[rowSums(help[c("A", "B", "C")]) %in% c(0, 3), ]
    list(
      list(case_id = "help-example", source = "DescTools 0.99.60 ?CochranQTest example (SAS/STAT FREQ documentation drug response data)",
        spec = list(alternative = "two.sided", conf.level = 0.95, options = list()),
        args = list(data = help), fields = q_fields),
      list(case_id = "help-example-concordant-blocks", source = "DescTools 0.99.60 ?CochranQTest example, concordant blocks only (degenerate case)",
        spec = list(alternative = "two.sided", conf.level = 0.95, options = list()),
        args = list(data = concordant), fields = "status"),
      baseline_case("cochran_q", q_fields))
  }
)
