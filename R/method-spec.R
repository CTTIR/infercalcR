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
  out <- data.frame(method_id = "descriptive_summary", backend = "base/stats", implemented = TRUE,
                    stringsAsFactors = FALSE)
  out$option_names <- I(list(c("variables", "group", "labels", "units", "types")))
  out
}
.ic_run_method <- function(spec, ...) {
  if (spec$method_id == "descriptive_summary") return(do.call(describe_data, c(list(...), spec$options)))
  .ic_abort("method adapter not implemented", "unsupported")
}
