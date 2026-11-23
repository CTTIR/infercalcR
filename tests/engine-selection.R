library(infercalcR)
# Engine selection: base is the default, the choice is recorded, and no engine
# is ever substituted. Inputs are the stored synthetic baseline examples.
fixtures <- readRDS(system.file("methods", "synthetic-baseline.rds", package = "infercalcR"))
inputs <- function(id) {
  a <- fixtures[[id]]$arguments
  a[setdiff(names(a), c("alternative", "conf.level"))]
}
run <- function(spec, id = spec$method_id) do.call(run_inference, c(list(spec), inputs(id)))
has_class <- function(expr, class) tryCatch({ expr; FALSE }, error = function(e) inherits(e, class))

spec <- method_spec("cochran_q")
stopifnot(identical(spec$engine, "base"))
engine <- result_metadata(run(spec))$engine
stopifnot(identical(engine$engine, "base"), identical(engine$package, "infercalcR"),
  identical(engine$version, as.character(utils::packageVersion("infercalcR"))))
tools::assertError(method_spec("cochran_q", engine = "fast"), classes = "infercalcR_invalid_input")
tools::assertError(method_spec("cochran_q", engine = c("base", "reference")), classes = "infercalcR_invalid_input")

# A specification saved by infercalcR 0.1.0 has no engine element.
legacy <- spec
legacy$engine <- NULL
stopifnot(identical(result_metadata(run(legacy, "cochran_q"))$engine$engine, "base"))
# Structural tampering with a specification is still rejected.
altered <- spec
altered$extra <- TRUE
tools::assertError(run(altered, "cochran_q"), classes = "infercalcR_invalid_input")

# Methods computed by stats itself have one engine, whichever is requested.
for (requested in c("base", "reference")) {
  single <- result_metadata(run(method_spec("t_means_paired", engine = requested)))$engine
  stopifnot(identical(single$engine, "single"), identical(single$package, "stats"),
    identical(single$version, as.character(utils::packageVersion("stats"))))
}
# Methods without a registered original cannot honour a reference request.
stopifnot(has_class(run(method_spec("classifier_predeclared_rule", engine = "reference")), "infercalcR_unsupported"))

# An original that is missing or too old stops with the required version.
fake <- list(cochran_q = list(package = "stats", functions = "t.test", min_version = "999.0",
  source = "test", adapter = identity))
message <- tryCatch(infercalcR:::.ic_engine_check("cochran_q", "reference", registry = fake),
  infercalcR_missing_backend = function(e) conditionMessage(e))
stopifnot(is.character(message), grepl("999.0", message, fixed = TRUE))
absent <- list(cochran_q = list(package = "infercalcRnotapackage", functions = "f", min_version = "1.0",
  source = "test", adapter = identity))
stopifnot(has_class(infercalcR:::.ic_engine_check("cochran_q", "reference", registry = absent),
  "infercalcR_missing_backend"))

# The DescTools trend test uses the prespecified scores, whatever the group names.
if (requireNamespace("DescTools", quietly = TRUE)) {
  trend_p <- function(engine, args, alternative)
    result_data(do.call(run_inference, c(list(method_spec("cochran_armitage_trend", alternative = alternative,
      engine = engine)), args)))$p.value[[1L]]
  trend <- inputs("cochran_armitage_trend")
  for (labels in list(c("3", "2", "1"), c("0", "10", "50"), names(trend$events))) {
    for (scores in list(c(0, 1, 2), c(0, 1, 3))) {
      args <- trend
      args$scores[] <- scores
      names(args$events) <- names(args$totals) <- names(args$scores) <- labels
      for (alternative in c("two.sided", "greater")) {
        base_p <- trend_p("base", args, alternative)
        reference_p <- trend_p("reference", args, alternative)
        stopifnot(abs(reference_p - base_p) <= 1e-8 * base_p)
      }
    }
  }
}

# DescTools doubles the smaller permutation tail without capping it, so heavily
# tied outcomes give a two-sided p above 1; the reference engine reports 1.
if (requireNamespace("DescTools", quietly = TRUE)) {
  tied <- list(data = data.frame(id = 1:12, group = rep(c("a", "b", "c"), each = 4), y = rep(c(1, 2), 6)),
    group_order = c("a", "b", "c"), order_prespecified = TRUE, independent_units = TRUE, seed = 1L, B = 999L)
  jt_p <- function(engine) result_data(do.call(run_inference,
    c(list(method_spec("jonckheere_terpstra", engine = engine)), tied)))$p.value[[1L]]
  stopifnot(identical(jt_p("base"), 1), identical(jt_p("reference"), 1))
}

# Reference engines enforce the base engines' scope limits, and stop quickly.
bounded <- function(expr) {
  setTimeLimit(elapsed = 30, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf))
  tryCatch(expr, error = function(e) e)
}
if (requireNamespace("DescTools", quietly = TRUE)) {
  large_b <- inputs("jonckheere_terpstra")
  large_b$B <- 1000000L
  jt_error <- function(engine) bounded(do.call(run_inference,
    c(list(method_spec("jonckheere_terpstra", engine = engine)), large_b)))
  base_error <- jt_error("base")
  reference_error <- jt_error("reference")
  stopifnot(inherits(base_error, "error"), identical(conditionMessage(reference_error), conditionMessage(base_error)))
}
if (requireNamespace("Exact", quietly = TRUE)) {
  large_design <- matrix(1000, 2, 2, dimnames = list(c("group_x", "group_y"), c("event", "non_event")))
  boschloo_error <- function(engine) bounded(run_inference(method_spec("boschloo_two_sided_minlike", engine = engine),
    table = large_design))
  base_error <- boschloo_error("base")
  reference_error <- boschloo_error("reference")
  stopifnot(inherits(base_error, "error"), identical(class(reference_error), class(base_error)))
}

# A residual below the numerical resolution of the inputs is no residual: an
# additive occasion shift (reacttime-derived) is not testable on any platform.
reacttime <- c(420, 420, 480, 420, 540, 360, 480, 480, 540, 480)
for (step in c(60, 0.1, 1 / 3)) {
  shifted <- data.frame(id = 1:10, sapply(0:5, function(j) reacttime + step * j))
  for (engine in c("base", "reference")) {
    status <- result_data(run_inference(method_spec("anova_repeated_one_way_gg", engine = engine), data = shifted,
      occasion_columns = names(shifted)[-1], independent_subjects = TRUE))$status[[1L]]
    stopifnot(identical(status, "not_applicable"))
  }
}

catalogue <- method_catalogue()
stopifnot(all(c("engines", "reference") %in% names(catalogue)),
  all(catalogue$engines %in% c("base", "single", "base+reference", "none")),
  identical(catalogue$engines[catalogue$method_id == "variance_chisq_one_sample"], "none"),
  identical(catalogue$engines[catalogue$method_id == "t_means_paired"], "single"))
cat("PASS engine selection, recording, legacy specifications and missing originals\n")

# Errors raised inside a method name the method. Omitted required arguments are
# invalid input naming them; other errors keep their class and message.
caught <- function(expr) tryCatch(expr, error = identity)
absent <- caught(run_inference(method_spec("t_mean_one_sample"), x = c(1, 2, 3)))
stopifnot(inherits(absent, "infercalcR_invalid_input"),
  grepl("t_mean_one_sample requires the argument mu0", conditionMessage(absent), fixed = TRUE))
none <- caught(run_inference(method_spec("t_mean_one_sample")))
stopifnot(inherits(none, "infercalcR_invalid_input"),
  grepl("requires the arguments x, mu0", conditionMessage(none), fixed = TRUE))
short <- caught(run_inference(method_spec("t_mean_one_sample", options = list(mu0 = 0)), x = 1))
stopifnot(inherits(short, "infercalcR_method_error"), inherits(short, "simpleError"),
  inherits(short, "infercalcR_error"), inherits(absent, "infercalcR_error"),
  identical(short$method_id, "t_mean_one_sample"), inherits(short$parent, "error"),
  startsWith(conditionMessage(short), "Method t_mean_one_sample: "),
  endsWith(conditionMessage(short), conditionMessage(short$parent)))
unknown <- caught(run_inference(method_spec("anova_one_way"), data = 1, outcome = 2))
stopifnot(inherits(unknown, "infercalcR_invalid_input"),
  grepl("unknown method arguments: data, outcome; accepted: x, group", conditionMessage(unknown),
    fixed = TRUE))
# Package-classed conditions pass through unchanged.
classed <- caught(run_inference(method_spec("variance_chisq_one_sample"), x = 1))
stopifnot(inherits(classed, "infercalcR_unsupported"),
  !inherits(classed, "infercalcR_method_error"))

# A specification prints its class, identity and settings and returns itself invisibly.
shown <- method_spec("t_mean_one_sample", conf.level = 0.9, options = list(mu0 = 0))
printed <- utils::capture.output(returned <- withVisible(print(shown)))
stopifnot(identical(printed, c("<method_spec> t_mean_one_sample", "  alternative: two.sided",
  "  conf.level:  0.9", "  missing:     error", "  engine:      base", "  options:     mu0 = 0")),
  !returned$visible, identical(returned$value, shown))
reference_lines <- utils::capture.output(print(method_spec("cochran_q", engine = "reference")))
stopifnot(identical(reference_lines[c(1L, 5L, 6L)],
  c("<method_spec> cochran_q", "  engine:      reference", "  options:     none")))
cat("PASS method identity in errors, required arguments and specification printing\n")

# The callable helpers name themselves in errors raised by their procedures.
helper <- withCallingHandlers(caught(parallel_analysis(data.frame(id = 1:5, a = 1:5), "a",
  independent_units = TRUE, continuous_measurements = TRUE, B = 19L)),
  warning = function(w) stop("unexpected warning: ", conditionMessage(w)))
stopifnot(inherits(helper, "infercalcR_method_error"),
  identical(helper$method_id, "efa_common_factor_ml.parallel_analysis"),
  startsWith(conditionMessage(helper), "Method efa_common_factor_ml.parallel_analysis: "))
cat("PASS method identity in helper errors\n")
