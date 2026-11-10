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

catalogue <- method_catalogue()
stopifnot(all(c("engines", "reference") %in% names(catalogue)),
  all(catalogue$engines %in% c("base", "single", "base+reference", "none")),
  identical(catalogue$engines[catalogue$method_id == "variance_chisq_one_sample"], "none"),
  identical(catalogue$engines[catalogue$method_id == "t_means_paired"], "single"))
cat("PASS engine selection, recording, legacy specifications and missing originals\n")
