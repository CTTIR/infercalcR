#' Specify an observed method without selecting from the data
#' @export
method_spec <- function(method_id, alternative = "two.sided", conf.level = 0.95,
                        missing = "error", options = list(), engine = "base") {
  if (!.ic_string(method_id)) .ic_abort("method_id must be one nonempty string")
  cat <- method_catalogue()
  if (!method_id %in% cat$method_id) .ic_abort(paste("unknown method_id:", method_id))
  if (!.ic_string(alternative) || !alternative %in% c("two.sided", "greater", "less")) .ic_abort("invalid alternative")
  if (!is.numeric(conf.level) || length(conf.level) != 1L || !is.finite(conf.level) || conf.level <= 0 || conf.level >= 1) .ic_abort("conf.level must be in (0, 1)")
  if (!.ic_string(missing) || !missing %in% c("error", "omit")) .ic_abort("missing must be error or omit")
  if (!is.list(options) || (length(options) && (is.null(names(options)) || any(!nzchar(names(options))) || anyDuplicated(names(options))))) .ic_abort("options must be a uniquely named list")
  allowed <- cat$option_names[[match(method_id, cat$method_id)]]
  if (length(setdiff(names(options), allowed))) .ic_abort(paste("unknown options:", paste(setdiff(names(options), allowed), collapse = ", ")))
  .ic_validate_options(method_id, options)
  if (!.ic_string(engine) || !engine %in% .ic_engines) .ic_abort("engine must be \"base\" or \"reference\"")
  structure(list(method_id = method_id, variant_id = method_id, alternative = alternative,
    conf.level = conf.level, missing = missing, options = options, engine = engine,
    contract_version = "1.0.0"), class = "method_spec")
}

#' Execute a prespecified observed procedure
#' @export
run_inference <- function(spec, ..., result_id = NULL) {
  if (!inherits(spec, "method_spec")) .ic_abort("spec must come from method_spec()")
  # Specifications saved by infercalcR 0.1.0 have no engine element and run with the base engine.
  legacy <- is.null(spec[["engine"]])
  current <- method_spec(spec$method_id, spec$alternative, spec$conf.level, spec$missing, spec$options,
    if (legacy) "base" else spec[["engine"]])
  expected <- current
  if (legacy) expected$engine <- NULL
  if (!identical(spec, expected)) .ic_abort("method specification was altered")
  .ic_run_method(current, ..., result_id = result_id)
}

#' List executable and explicitly unsupported methods
#' @export
method_catalogue <- function() {
  ids <- names(.ic_method_info)
  out <- data.frame(method_id = c(ids, "variance_chisq_one_sample"),
    backend = c(vapply(.ic_method_info, `[[`, "", "backend"), "none"),
    implemented = c(rep(TRUE, length(ids)), FALSE), stringsAsFactors = FALSE)
  out$engines <- .ic_catalogue_engines(out$method_id, out$implemented)
  out$reference <- .ic_catalogue_reference(out$method_id)
  out$option_names <- I(c(lapply(.ic_method_info, `[[`, "options"), list(character())))
  j <- match("descriptive_summary", out$method_id)
  out$option_names[[j]] <- unique(c(out$option_names[[j]], "variables", "group", "labels", "units", "types"))
  out
}

.ic_bind_arguments <- function(fun, args) {
  formal_names <- names(formals(fun))
  nm <- names(args)
  if (is.null(nm)) nm <- rep("", length(args))
  if (anyNA(nm) || anyDuplicated(nm[nzchar(nm)])) .ic_abort("arguments must have unique, nonmissing names")
  if (any(!nm[nzchar(nm)] %in% formal_names)) .ic_abort(paste("unknown method arguments:", paste(setdiff(nm[nzchar(nm)], formal_names), collapse = ", ")))
  free <- setdiff(formal_names, nm[nzchar(nm)])
  unnamed <- which(!nzchar(nm))
  if (length(unnamed) > length(free)) .ic_abort("too many method arguments")
  nm[unnamed] <- free[seq_along(unnamed)]
  names(args) <- nm
  args
}

.ic_run_method <- function(spec, ..., result_id = NULL) {
  if (spec$method_id == "variance_chisq_one_sample") .ic_abort("This advisor-only method has no implemented observed adapter", "unsupported")
  dots <- list(...)
  if (!is.null(result_id) && !.ic_string(result_id)) .ic_abort("result_id must be one nonempty string")
  if (any(c("alternative", "conf.level") %in% names(dots))) .ic_abort("Set alternative and conf.level in method_spec(), not in run_inference()")
  if (any(names(dots) %in% names(spec$options))) .ic_abort("A method argument was supplied both in options and run_inference")
  requested <- if (is.null(spec[["engine"]])) "base" else spec[["engine"]]
  if (spec$method_id == "descriptive_summary" && "data" %in% names(dots)) {
    engine <- .ic_engine_check(spec$method_id, requested)
    if (spec$alternative != "two.sided") .ic_abort("descriptions do not have a directional alternative")
    out <- do.call(describe_data, c(dots, spec$options))
    out$metadata$engine <- engine
    if (!is.null(result_id)) out$metadata$result_id <- result_id
    return(out)
  }
  info <- .ic_method_info[[spec$method_id]]
  fun <- .ic_method_function(spec$method_id)
  packages <- strsplit(info$backend, ",", fixed = TRUE)[[1L]]
  for (package in setdiff(packages, c("", "stats", "utils", "base"))) {
    if (!requireNamespace(package, quietly = TRUE)) .ic_abort(paste("Method", spec$method_id, "requires the optional", package, "package"), "missing_backend")
  }
  engine <- .ic_engine_check(spec$method_id, requested)
  if (identical(engine$engine, "reference")) fun <- .ic_engine_info[[spec$method_id]]$adapter
  args <- c(dots, spec$options)
  # Prediction development does not test an alternative; its native API requires NULL.
  if (info$has_alternative) args$alternative <- if (grepl("^prediction_", spec$method_id)) NULL else spec$alternative
  if ("conf.level" %in% info$arguments) args$conf.level <- spec$conf.level
  if (!info$hypothesis && spec$alternative != "two.sided") .ic_abort("This method does not provide a directional hypothesis test")
  args <- .ic_bind_arguments(fun, args)
  if (spec$missing == "omit") {
    explicit <- identical(args$complete_cases, TRUE) || identical(args$missing, "complete_cases") ||
      any(args$missing_outcomes %in% c("available_case_MAR", "available_case_MCAR")) || identical(args$missing, "pairwise")
    if (!isTRUE(explicit)) .ic_abort("missing='omit' requires a supported explicit method policy (complete_cases, MAR, MCAR or pairwise); it never selects one")
  }
  # R 4.1 may materialize shared deferred factor levels during these models'
  # factor operations. Isolate the representation change without changing inference.
  if (spec$method_id %in% c("regression_multinomial", "regression_ordinal_proportional_odds"))
    args <- unserialize(serialize(args, NULL))
  warnings <- character()
  native <- withCallingHandlers(do.call(fun, args), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
  })
  out <- .ic_native_result(native, spec, args, warnings)
  out$metadata$engine <- engine
  if (!is.null(result_id)) out$metadata$result_id <- result_id
  out
}

#' Exploratory common-factor parallel analysis
#' @export
parallel_analysis <- function(data, variables, independent_units = FALSE,
    continuous_measurements = FALSE, B = 199L, seed = 104729L,
    quantile_probability = .95, complete_cases = FALSE, permutations = NULL) {
  native <- .obs_efa_common_factor_ml_parallel_analysis(data, variables, independent_units,
    continuous_measurements, B, seed, quantile_probability, complete_cases, permutations)
  spec <- list(method_id = "efa_common_factor_ml.parallel_analysis", alternative = NA_character_, conf.level = NA_real_,
    options = list(variables = variables, independent_units = independent_units,
      continuous_measurements = continuous_measurements, B = B, seed = seed,
      quantile_probability = quantile_probability, complete_cases = complete_cases))
  .ic_native_result(native, spec, list(data = data), character())
}

#' Predict from a finalized prediction-development result
#' @export
predict_inference <- function(result, newdata, interval = "none", conf.level = NULL) {
  validate_inference(result)
  id <- result_metadata(result)$method_id
  if (!id %in% c("prediction_model_linear", "prediction_model_logistic")) .ic_abort("predict_inference requires a prediction-development result")
  interval <- match.arg(interval, if (id == "prediction_model_linear") c("none", "mean", "prediction") else c("none", "risk"))
  native <- result$payload$native
  if (is.null(conf.level)) conf.level <- native$requested_confidence_level
  fun <- if (id == "prediction_model_linear") .obs_prediction_model_linear_predict_new else .obs_prediction_model_logistic_predict_new
  out <- fun(native, newdata, interval = interval, conf.level = conf.level)
  if (is.numeric(out)) out <- list(status = "ok", estimate = out, interval = "none")
  n <- length(out$estimate)
  target <- if (id == "prediction_model_logistic") "event_probability" else if (interval == "prediction") "new_outcome" else "conditional_mean"
  interval_type <- if (interval == "none") "none" else if (target == "new_outcome") "prediction" else "confidence"
  interval_method <- if (interval == "none") "not_requested" else if (id == "prediction_model_linear") {
    if (interval == "prediction") "normal-model t prediction interval (includes new-outcome residual variation)" else
      "normal-model t confidence interval for the conditional mean"
  } else .ic_text(out$method, "ordinary-bootstrap percentile interval for fitted event risk")
  rows <- data.frame(row_id = paste0("prediction:", seq_len(n)), component = "prediction",
    estimand = if (target == "new_outcome") "one independent new outcome" else if (target == "conditional_mean") "conditional mean of a new outcome" else "event probability for a new case",
    estimate = as.double(out$estimate), conf.low = if (is.null(out$lower)) rep(NA_real_, n) else out$lower,
    conf.high = if (is.null(out$upper)) rep(NA_real_, n) else out$upper,
    conf.level = if (is.null(out$lower)) NA_real_ else conf.level,
    interval.method = interval_method,
    interval.scope = if (is.null(out$lower)) NA_character_ else "pointwise", interval.scale = if (id == "prediction_model_linear") "outcome" else "probability",
    status = .ic_native_status(out$status), reason = if (is.null(out$reason)) NA_character_ else out$reason,
    stringsAsFactors = FALSE)
  new_inference_result(rows, list(method_id = paste0(id, ".predict_new"), source_result_id = result$metadata$result_id,
    interval = interval, interval_target = target, interval_type = interval_type,
    interval_interpretation = out$interpretation, source_status = out$status, assumptions = out$assumptions), payload = list(native = out))
}

.ic_validate_options <- function(method_id, options) {
  flags <- c("exact", "location_shift", "independent_units", "common_slopes", "standardize", "standardise",
    "complete_cases", "order_prespecified", "independent_binomial_groups", "independent_splits", "predeclared",
    "continuous_measurements", "model_assumptions_justified", "independent_clusters", "random_intercept_justified",
    "proportional_odds_justified", "scores_prespecified")
  for (name in intersect(names(options), flags)) {
    x <- options[[name]]
    if (!is.logical(x) || length(x) != 1L || is.na(x)) .ic_abort(paste(name, "must be one nonmissing logical value"))
  }
  numeric_options <- c("mu0", "mu", "ratio0", "p0", "auc", "sigma", "cutoff", "prevalence", "n_total", "tolerance",
    "max_subdivisions", "quantile_probability", "mc.level", "seed", "B", "k", "factors", "components", "n_excluded")
  for (name in intersect(names(options), numeric_options)) {
    x <- options[[name]]
    if (is.null(x) && name %in% c("prevalence")) next
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x)) .ic_abort(paste(name, "must be one finite number"))
    if (name %in% c("seed", "B", "k", "factors", "components", "n_excluded", "max_subdivisions", "n_total") && (x < 0 || x != floor(x))) .ic_abort(paste(name, "must be a nonnegative integer"))
  }
  if ("missing" %in% names(options) && (!.ic_string(options$missing) || !options$missing %in% c("error", "complete_cases", "pairwise"))) .ic_abort("unsupported method-specific missing policy")
  if ("missing_outcomes" %in% names(options)) {
    policies <- if (method_id == "gee_logistic_exchangeable") c("error", "available_case_MCAR") else c("error", "available_case_MAR")
    if (!.ic_string(options$missing_outcomes) || !options$missing_outcomes %in% policies) .ic_abort("missing_outcomes must explicitly match the method's MCAR or MAR assumption")
  }
  invisible(options)
}
