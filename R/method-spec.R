#' Specify an observed method without selecting from the data
#' @export
method_spec <- function(method_id, alternative = "two.sided", conf.level = 0.95,
                        missing = "error", options = list()) {
  if (!.ic_string(method_id)) .ic_abort("method_id must be one nonempty string")
  cat <- method_catalogue()
  if (!method_id %in% cat$method_id) .ic_abort(paste("unknown method_id:", method_id))
  if (!.ic_string(alternative) || !alternative %in% c("two.sided", "greater", "less")) .ic_abort("invalid alternative")
  if (!is.numeric(conf.level) || length(conf.level) != 1L || !is.finite(conf.level) || conf.level <= 0 || conf.level >= 1) .ic_abort("conf.level must be in (0, 1)")
  if (!.ic_string(missing) || !missing %in% c("error", "omit")) .ic_abort("missing must be error or omit")
  if (!is.list(options) || (length(options) && (is.null(names(options)) || any(!nzchar(names(options))) || anyDuplicated(names(options))))) .ic_abort("options must be a uniquely named list")
  allowed <- cat$option_names[[match(method_id, cat$method_id)]]
  if (length(setdiff(names(options), allowed))) .ic_abort(paste("unknown options:", paste(setdiff(names(options), allowed), collapse = ", ")))
  structure(list(method_id = method_id, variant_id = method_id, alternative = alternative,
    conf.level = conf.level, missing = missing, options = options, contract_version = "1.0.0"), class = "method_spec")
}

#' Execute a prespecified observed procedure
#' @export
run_inference <- function(spec, ...) {
  if (!inherits(spec, "method_spec")) .ic_abort("spec must come from method_spec()")
  valid <- method_spec(spec$method_id, spec$alternative, spec$conf.level, spec$missing, spec$options)
  if (!identical(spec, valid)) .ic_abort("method specification was altered")
  .ic_run_method(spec, ...)
}

#' List executable and explicitly unsupported methods
#' @export
method_catalogue <- function() {
  ids <- names(.ic_method_info)
  out <- data.frame(method_id = c(ids, "variance_chisq_one_sample"),
    backend = c(vapply(.ic_method_info, `[[`, "", "backend"), "none"),
    implemented = c(rep(TRUE, length(ids)), FALSE), stringsAsFactors = FALSE)
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

.ic_run_method <- function(spec, ...) {
  if (spec$method_id == "variance_chisq_one_sample") .ic_abort("This advisor-only method has no implemented observed adapter", "unsupported")
  dots <- list(...)
  result_id <- dots$result_id
  dots$result_id <- NULL
  if (!is.null(result_id) && !.ic_string(result_id)) .ic_abort("result_id must be one nonempty string")
  if (any(c("alternative", "conf.level") %in% names(dots))) .ic_abort("Set alternative and conf.level in method_spec(), not in run_inference()")
  if (any(names(dots) %in% names(spec$options))) .ic_abort("A method argument was supplied both in options and run_inference")
  if (spec$method_id == "descriptive_summary" && "data" %in% names(dots)) {
    if (spec$alternative != "two.sided") .ic_abort("descriptions do not have a directional alternative")
    out <- do.call(describe_data, c(dots, spec$options))
    if (!is.null(result_id)) out$metadata$result_id <- result_id
    return(out)
  }
  info <- .ic_method_info[[spec$method_id]]
  fun <- .ic_method_function(spec$method_id)
  packages <- strsplit(info$backend, ",", fixed = TRUE)[[1L]]
  for (package in setdiff(packages, c("", "stats", "utils", "base"))) {
    if (!requireNamespace(package, quietly = TRUE)) .ic_abort(paste("Method", spec$method_id, "requires the optional", package, "package"), "missing_backend")
  }
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
  warnings <- character()
  native <- withCallingHandlers(do.call(fun, args), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w))
  })
  out <- .ic_native_result(native, spec, args, warnings)
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
  native <- result$payload$native
  if (is.null(conf.level)) conf.level <- native$requested_confidence_level
  fun <- if (id == "prediction_model_linear") .obs_prediction_model_linear_predict_new else .obs_prediction_model_logistic_predict_new
  out <- fun(native, newdata, interval = interval, conf.level = conf.level)
  if (is.numeric(out)) out <- list(status = "ok", estimate = out, interval = "none")
  n <- length(out$estimate)
  rows <- data.frame(row_id = paste0("prediction:", seq_len(n)), component = "prediction",
    estimand = if (id == "prediction_model_linear") "conditional mean of a new outcome" else "event probability for a new case",
    estimate = as.double(out$estimate), conf.low = if (is.null(out$lower)) rep(NA_real_, n) else out$lower,
    conf.high = if (is.null(out$upper)) rep(NA_real_, n) else out$upper,
    conf.level = if (is.null(out$lower)) NA_real_ else conf.level,
    interval.method = if (is.null(out$method)) if (interval == "none") "not_requested" else "normal-model t" else out$method,
    interval.scope = "pointwise", interval.scale = if (id == "prediction_model_linear") "outcome" else "probability",
    status = .ic_native_status(out$status), reason = if (is.null(out$reason)) NA_character_ else out$reason,
    stringsAsFactors = FALSE)
  new_inference_result(rows, list(method_id = paste0(id, ".predict_new"), source_result_id = result$metadata$result_id,
    interval = interval, source_status = out$status, assumptions = out$assumptions), payload = list(native = out))
}
