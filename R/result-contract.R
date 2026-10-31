# Shared plain-data contract, revision 1.
.ic_abort <- function(message, code = "invalid_input") {
  stop(structure(list(message = message, call = NULL),
                 class = c(paste0("infercalcR_", code), "error", "condition")))
}
.ic_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
.ic_statuses <- c("ok", "undefined", "not_applicable", "unsupported", "invalid_input",
                  "numerical_failure", "infeasible", "reference_pending", "unresolved")
.ic_columns <- list(row_id = character(), variable_id = character(), component = character(),
  estimand = character(), contrast = character(), estimate = double(), statistic = double(),
  df = double(), conf.low = double(), conf.high = double(), conf.level = double(),
  interval.method = character(), interval.scale = character(), interval.scope = character(),
  p.value = double(), p.adjusted = double(), p.reported = double(),
  p.method = character(), family_id = character(), family_size = integer(),
  null.value = double(), alternative = character(), n = double(), n.excluded = double(),
  status = character(), reason = character())

#' Construct a common result
#' @export
new_inference_result <- function(data, metadata, kind = "inference", payload = list()) {
  if (!is.data.frame(data) || anyDuplicated(names(data))) .ic_abort("data must be a data.frame with unique column names")
  if (!is.list(metadata) || is.null(names(metadata)) || anyDuplicated(names(metadata)) ||
      !.ic_string(metadata$method_id)) .ic_abort("metadata needs a nonempty method_id and unique names")
  if (!.ic_string(kind) || !kind %in% c("inference", "planning", "classification", "descriptive")) .ic_abort("invalid result kind")
  if (!is.list(payload)) .ic_abort("payload must be a list")
  n <- nrow(data)
  for (nm in setdiff(names(.ic_columns), names(data))) {
    prototype <- .ic_columns[[nm]]
    data[[nm]] <- rep(switch(typeof(prototype), character = NA_character_,
                            integer = NA_integer_, double = NA_real_), n)
  }
  if (all(is.na(data$row_id))) data$row_id <- paste0(metadata$method_id, ":", seq_len(n))
  if (all(is.na(data$status))) data$status <- rep("ok", n)
  if (all(is.na(data$p.reported))) data$p.reported <- data$p.value
  if (is.null(metadata$variant_id)) metadata$variant_id <- metadata$method_id
  if (is.null(metadata$result_id)) metadata$result_id <- paste0(metadata$method_id, ":result")
  metadata$contract_version <- "1.0.0"
  metadata$kind <- kind
  if (is.null(metadata$provenance)) metadata$provenance <- list(package = "infercalcR",
    version = as.character(utils::packageVersion("infercalcR")), R = as.character(getRversion()))
  if (is.null(metadata$validation)) metadata$validation <- "not_assessed"
  ans <- structure(list(data = data, metadata = metadata, payload = payload),
                   class = c(paste0(kind, "_result"), "inference_result"))
  class(ans) <- unique(class(ans))
  validate_inference(ans)
  ans
}

#' Validate a common result
#' @export
validate_inference <- function(x) {
  if (!inherits(x, "inference_result") || !is.list(x) || !is.data.frame(x$data) ||
      !is.list(x$metadata) || !is.list(x$payload)) .ic_abort("malformed inference result")
  d <- x$data
  if (length(setdiff(names(.ic_columns), names(d))) || anyDuplicated(names(d))) .ic_abort("missing or duplicate result columns")
  for (nm in names(.ic_columns)) {
    expected <- typeof(.ic_columns[[nm]])
    if (expected == "character" && !is.character(d[[nm]])) .ic_abort(paste(nm, "must be character"))
    if (expected %in% c("double", "integer") && !is.numeric(d[[nm]])) .ic_abort(paste(nm, "must be numeric"))
  }
  if (anyNA(d$row_id) || any(!nzchar(d$row_id)) || anyDuplicated(d$row_id)) .ic_abort("row IDs must be nonempty and unique")
  if (anyNA(d$status) || any(!d$status %in% .ic_statuses)) .ic_abort("unknown computation status")
  for (nm in c("p.value", "p.adjusted", "p.reported")) {
    p <- d[[nm]]
    if (any(is.nan(p)) || any(!is.na(p) & (!is.finite(p) | p < 0 | p > 1))) .ic_abort(paste(nm, "must be NA or in [0, 1]"))
  }
  if (any(!is.na(d$conf.level) & (!is.finite(d$conf.level) | d$conf.level <= 0 | d$conf.level >= 1))) .ic_abort("invalid confidence level")
  if (any(!is.na(d$conf.low) & !is.na(d$conf.high) & d$conf.low > d$conf.high)) .ic_abort("reversed confidence limits")
  for (nm in c("n", "n.excluded", "family_size")) {
    v <- d[[nm]]
    if (any(!is.na(v) & (!is.finite(v) | v < 0 | v != floor(v)))) .ic_abort(paste(nm, "must be a nonnegative count or NA"))
  }
  if (!.ic_string(x$metadata$method_id) || !.ic_string(x$metadata$variant_id) ||
      !.ic_string(x$metadata$result_id) || !identical(x$metadata$contract_version, "1.0.0") ||
      !x$metadata$kind %in% c("inference", "planning", "classification", "descriptive")) .ic_abort("invalid result metadata")
  invisible(x)
}

#' Extract finalized numerical rows
#' @export
result_data <- function(x, ...) UseMethod("result_data")
#' @export
result_data.inference_result <- function(x, ...) { validate_inference(x); x$data }
#' @export
result_data.default <- function(x, ...) .ic_abort("no public result_data method for this object", "unsupported")
#' Extract result metadata
#' @export
result_metadata <- function(x, ...) UseMethod("result_metadata")
#' @export
result_metadata.inference_result <- function(x, ...) { validate_inference(x); x$metadata }
#' @export
result_metadata.default <- function(x, ...) .ic_abort("no public result_metadata method for this object", "unsupported")
#' Convert an existing result without fitting another analysis
#' @export
as_inference <- function(x, ...) UseMethod("as_inference")
#' @export
as_inference.inference_result <- function(x, ...) { validate_inference(x); x }
#' @export
as_inference.default <- function(x, ...) .ic_abort("no explicit as_inference adapter for this object", "unsupported")
#' @export
print.inference_result <- function(x, ...) {
  validate_inference(x)
  cat(sprintf("<%s> %s\n", x$metadata$kind, x$metadata$method_id))
  print(x$data[, c("row_id", "estimand", "estimate", "p.reported", "status"), drop = FALSE], row.names = FALSE)
  invisible(x)
}

#' Adjust a declared complete hypothesis family
#' @export
adjust_inference <- function(x, method = "holm", family_id, rows = NULL) {
  validate_inference(x)
  if (!.ic_string(method) || !method %in% stats::p.adjust.methods || !.ic_string(family_id)) .ic_abort("declare an adjustment method and family_id")
  d <- x$data
  if (is.null(rows)) rows <- d$row_id
  if (!is.character(rows) || !length(rows) || anyNA(rows) || anyDuplicated(rows) || any(!rows %in% d$row_id)) .ic_abort("rows must identify a nonempty unique complete family")
  i <- match(rows, d$row_id)
  if (any(!is.na(d$p.adjusted[i])) || any(!is.na(d$family_id[i]))) .ic_abort("family already has adjustment metadata; do not adjust twice")
  if (anyNA(d$p.value[i])) .ic_abort("all declared family members need raw p-values")
  d$p.adjusted[i] <- stats::p.adjust(d$p.value[i], method = method, n = length(i))
  d$p.reported[i] <- d$p.adjusted[i]
  d$p.method[i] <- method
  d$family_id[i] <- family_id
  d$family_size[i] <- length(i)
  x$data <- d
  x$metadata$adjustment <- c(x$metadata$adjustment, list(list(family_id = family_id,
    rows = rows, size = length(i), method = method, interval_adjustment = "unchanged")))
  validate_inference(x)
  x
}
