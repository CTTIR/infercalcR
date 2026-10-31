# Extracted from inferguidR e91938f77eaa991b69989b121bfd32c124c95db0.
# Original source and MIT license: Raban Heller. See tools/extraction/source-map.csv.

ig_status_codes <- function() {
  c("ok", "infeasible", "unsupported", "invalid_input",
    "numerical_failure", "reference_pending")
}

ig_status <- function(code, reason = "", details = list()) {
  if (!is.character(code) || length(code) != 1L || is.na(code) ||
      !(code %in% ig_status_codes())) {
    stop("`code` must be one of: ", paste(ig_status_codes(), collapse = ", "),
         call. = FALSE)
  }
  if (is.null(reason)) reason <- ""
  if (!is.character(reason) || length(reason) != 1L || is.na(reason)) {
    stop("`reason` must be a single string", call. = FALSE)
  }
  if (code != "ok" && !nzchar(trimws(reason))) {
    stop("a status other than 'ok' needs a non-empty `reason`", call. = FALSE)
  }
  if (is.null(details)) details <- list()
  if (!is.list(details) || (length(details) > 0L && !.ig_is_named(details))) {
    stop("`details` must be a named list", call. = FALSE)
  }
  structure(list(code = code, reason = reason,
                 details = .ig_named_or_empty(.ig_sort_names(details))),
            class = "ig_status")
}

is_ig_status <- function(x) inherits(x, "ig_status")

format.ig_status <- function(x, ...) {
  if (identical(x$code, "ok") && !nzchar(x$reason)) return("ok")
  sprintf("%s: %s", x$code, x$reason)
}

print.ig_status <- function(x, ...) {
  cat("<ig_status> ", format(x), "\n", sep = "")
  problems <- x$details$problems
  if (length(problems)) {
    cat(paste0("  - ", unlist(problems), collapse = "\n"), "\n", sep = "")
  }
  invisible(x)
}

.ig_status_from_list <- function(x) {
  if (!is.list(x) || is.null(x$code)) {
    stop("status record must be an object with a `code`", call. = FALSE)
  }
  ig_status(x$code, if (is.null(x$reason)) "" else x$reason,
            if (is.null(x$details)) list() else x$details)
}

.ig_status_to_list <- function(x) {
  list(code = x$code, reason = x$reason,
       details = .ig_named_or_empty(x$details))
}

.ig_abort <- function(code, reason, details = list(), call = NULL) {
  status <- ig_status(code, reason, details)
  msg <- reason
  problems <- details$problems
  if (length(problems) > 1L) {
    msg <- paste0(reason, "\n", paste0("* ", unlist(problems), collapse = "\n"))
  }
  cond <- structure(
    class = c(paste0("inferguidR_", code), paste0("infercalcR_", code),
              "infercalcR_error", "inferguidR_error", "error", "condition"),
    list(message = msg, call = call, status = status)
  )
  stop(cond)
}

.ig_abort_invalid <- function(problems, context = "invalid input") {
  problems <- unique(as.character(problems))
  reason <- if (length(problems) == 1L) {
    paste0(context, ": ", problems)
  } else {
    sprintf("%s: %d problems found", context, length(problems))
  }
  .ig_abort("invalid_input", reason, list(problems = as.list(problems)))
}

.ig_is_named <- function(x) {
  nms <- names(x)
  !is.null(nms) && !anyNA(nms) && all(nzchar(nms))
}

.ig_is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

.ig_is_flag <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)

.ig_empty_object <- function() structure(list(), names = character(0))

.ig_named_or_empty <- function(x) {
  if (is.null(x) || length(x) == 0L) .ig_empty_object() else x
}

.ig_sort_names <- function(x) {
  if (!is.list(x) || (is.object(x) && !inherits(x, "AsIs"))) return(x)
  if (length(x) == 0L) return(x)
  nms <- names(x)
  if (!is.null(nms)) x <- x[order(nms, method = "radix")]
  for (i in seq_along(x)) {
    if (!is.null(x[[i]])) x[i] <- list(.ig_sort_names(x[[i]]))
  }
  x
}

`%||%` <- function(x, y) if (is.null(x)) y else x
