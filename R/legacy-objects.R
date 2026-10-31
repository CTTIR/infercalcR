# Extracted from inferguidR e91938f77eaa991b69989b121bfd32c124c95db0.
# Original source and MIT license: Raban Heller. See tools/extraction/source-map.csv.

.ig_classification_vocab <- list(
  input_kind = c("theoretical_binormal", "scored_data", "counts_2x2"),
  direction = c("higher_is_positive", "lower_is_positive"),
  comparator = c(">=", ">", "<=", "<"),
  score_scale = c("probability", "raw_score"),
  evaluation_split = c("same_data", "held_out_tuning", "held_out_test", "external",
                       "resampling", "not_applicable"),
  selection = c("manual", "max_youden", "max_f1", "max_mcc", "constraint", "capacity",
                "cost_utility"),
  selection_data = c("same_data", "held_out_tuning", "predeclared_rule"),
  prevalence_kind = c("observed", "scenario")
)

.ig_new_classification_spec <- function(input_kind, models, operating_rule,
                                        prevalence = list(kind = "observed", value = NULL, source = NULL),
                                        counts = NULL, theory = NULL, data_fingerprint = NULL,
                                        uncertainty = list(method = NULL, level = NULL,
                                                           seed = NULL, replicates = NULL)) {
  x <- list(schema_version = .ig_schema_versions[["classification_spec"]],
            object_type = "classification_spec",
            input_kind = input_kind, models = models, operating_rule = operating_rule,
            prevalence = prevalence, counts = counts, theory = theory,
            data_fingerprint = data_fingerprint, uncertainty = uncertainty)
  problems <- .ig_classification_spec_problems(x)
  if (length(problems)) .ig_abort_invalid(problems, "invalid classification specification")
  x <- .ig_doubles(.ig_sort_names(x))
  x$spec_hash <- .ig_hash_value(x)
  structure(x[order(names(x), method = "radix")], class = c("classification_spec", "ig_frozen"))
}

.ig_new_observed_test_spec <- function(variant_id, inputs = list(), options = list(),
                                       data_fingerprint = NULL) {
  p <- character()
  if (!.ig_is_string(variant_id) || !grepl("^[a-z0-9]+(_[a-z0-9]+)*$", variant_id)) {
    p <- c(p, "variant_id must be a lower-case identifier")
  }
  if (!is.list(inputs) || (length(inputs) && !.ig_is_named(inputs))) p <- c(p, "inputs must be a named list")
  if (!is.list(options) || (length(options) && !.ig_is_named(options))) p <- c(p, "options must be a named list")
  if (!is.null(data_fingerprint) && !.ig_is_string(data_fingerprint)) p <- c(p, "data_fingerprint must be a string or NULL")
  if (length(p)) .ig_abort_invalid(p, "invalid observed-test specification")
  x <- list(schema_version = .ig_schema_versions[["observed_test_spec"]],
            object_type = "observed_test_spec", variant_id = variant_id,
            inputs = .ig_named_or_empty(.ig_sort_names(inputs)),
            options = .ig_named_or_empty(.ig_sort_names(options)),
            data_fingerprint = data_fingerprint)
  x <- .ig_doubles(.ig_sort_names(x))
  x$spec_hash <- .ig_hash_value(x)
  structure(x[order(names(x), method = "radix")], class = c("observed_test_spec", "ig_frozen"))
}

.ig_check_aux_spec <- function(spec, kind) {
  body <- .ig_unclass_frozen(spec)
  stored <- body$spec_hash
  body$spec_hash <- NULL
  if (!.ig_is_string(stored) || !identical(.ig_hash_value(body), stored)) {
    .ig_abort("invalid_input", sprintf("the %s was modified after creation (hash mismatch)", class(spec)[1]))
  }
  if (kind == "classification") {
    problems <- .ig_classification_spec_problems(body)
    if (length(problems)) .ig_abort_invalid(problems, "invalid classification specification")
  }
  invisible(TRUE)
}

.ig_classification_spec_problems <- function(x) {
  v <- .ig_classification_vocab
  p <- character()
  enum_ok <- function(val, allowed) .ig_is_string(val) && val %in% allowed
  if (!enum_ok(x$input_kind, v$input_kind)) {
    p <- c(p, sprintf("input_kind must be one of: %s", paste(v$input_kind, collapse = ", ")))
  }
  if (!is.list(x$models) || !length(x$models) || !is.null(names(x$models))) {
    p <- c(p, "models must be a non-empty list of model records")
  } else {
    ids <- character()
    for (i in seq_along(x$models)) {
      m <- x$models[[i]]
      where <- sprintf("models[%d]", i)
      for (f in c("model_id", "name", "score_meaning", "positive_label", "tie_rule",
                  "missing_value_policy")) {
        if (!.ig_is_string(m[[f]])) p <- c(p, sprintf("%s$%s must be a string", where, f))
      }
      if (!enum_ok(m$direction, v$direction)) p <- c(p, sprintf("%s$direction must be one of: %s", where, paste(v$direction, collapse = ", ")))
      if (!enum_ok(m$comparator, v$comparator)) p <- c(p, sprintf("%s$comparator must be one of: %s", where, paste(v$comparator, collapse = ", ")))
      if (!enum_ok(m$score_scale, v$score_scale)) p <- c(p, sprintf("%s$score_scale must be one of: %s", where, paste(v$score_scale, collapse = ", ")))
      if (!enum_ok(m$evaluation_split, v$evaluation_split)) p <- c(p, sprintf("%s$evaluation_split must be one of: %s", where, paste(v$evaluation_split, collapse = ", ")))
      if (!is.null(m$source_hash) && !(.ig_is_string(m$source_hash) && grepl("^[0-9a-f]{64}$", m$source_hash))) {
        p <- c(p, sprintf("%s$source_hash must be a SHA-256 hex string or NULL", where))
      }
      ids <- c(ids, if (.ig_is_string(m$model_id)) m$model_id else NA_character_)
    }
    if (anyDuplicated(stats::na.omit(ids))) p <- c(p, "model_id values must be unique")
  }
  r <- x$operating_rule
  if (!is.list(r)) {
    p <- c(p, "operating_rule must be a list")
  } else {
    if (!is.null(r$score_cutoff) && !(is.numeric(r$score_cutoff) && length(r$score_cutoff) == 1L &&
                                       is.finite(r$score_cutoff))) {
      p <- c(p, "operating_rule$score_cutoff must be a finite number or NULL")
    }
    if (!enum_ok(r$selection, v$selection)) p <- c(p, sprintf("operating_rule$selection must be one of: %s", paste(v$selection, collapse = ", ")))
    if (!enum_ok(r$selection_data, v$selection_data)) p <- c(p, sprintf("operating_rule$selection_data must be one of: %s", paste(v$selection_data, collapse = ", ")))
    if (identical(r$selection, "manual") && is.null(r$score_cutoff)) {
      p <- c(p, "a manual selection needs operating_rule$score_cutoff")
    }
  }
  pr <- x$prevalence
  if (!is.list(pr) || !enum_ok(pr$kind, v$prevalence_kind)) {
    p <- c(p, "prevalence$kind must be 'observed' or 'scenario'")
  } else if (!is.null(pr$value) && !(is.numeric(pr$value) && length(pr$value) == 1L &&
                                       is.finite(pr$value) && pr$value >= 0 && pr$value <= 1)) {
    p <- c(p, "prevalence$value must be a probability or NULL")
  } else if (identical(pr$kind, "scenario") && is.null(pr$value)) {
    p <- c(p, "a prevalence scenario needs prevalence$value")
  }
  if (identical(x$input_kind, "counts_2x2")) {
    cnt <- x$counts
    ok <- is.list(cnt) && all(c("tp", "fn", "fp", "tn") %in% names(cnt)) &&
      all(vapply(cnt[c("tp", "fn", "fp", "tn")], function(z) {
        is.numeric(z) && length(z) == 1L && is.finite(z) && z >= 0 && z == round(z)
      }, logical(1)))
    if (!ok) p <- c(p, "counts_2x2 input needs counts tp, fn, fp, tn as non-negative whole numbers")
  }
  if (identical(x$input_kind, "theoretical_binormal")) {
    th <- x$theory
    ok <- is.list(th) && is.numeric(th$auc) && length(th$auc) == 1L && is.finite(th$auc) &&
      th$auc > 0 && th$auc < 1 && is.numeric(th$sigma) && length(th$sigma) == 1L &&
      is.finite(th$sigma) && th$sigma > 0
    if (!ok) p <- c(p, "theoretical_binormal input needs theory$auc in (0, 1) and theory$sigma > 0")
  }
  if (identical(x$input_kind, "scored_data") && !.ig_is_string(x$data_fingerprint)) {
    p <- c(p, "scored_data input needs a data_fingerprint (hash of the local data; no raw rows)")
  }
  p
}

.ig_finite_paths <- function(x, path) {
  if (is.null(x)) return(character())
  if (is.list(x)) {
    nms <- names(x)
    out <- character()
    for (i in seq_along(x)) {
      sub <- if (is.null(nms)) paste0(path, "[", i, "]") else paste0(path, "$", nms[i])
      out <- c(out, .ig_finite_paths(x[[i]], sub))
    }
    return(out)
  }
  if (is.numeric(x) && any(is.finite(x))) return(path)
  character()
}

.ig_doubles <- function(x) {
  if (is.list(x)) {
    nms <- names(x)
    out <- lapply(x, .ig_doubles)
    names(out) <- nms
    return(out)
  }
  if (is.integer(x) && !is.factor(x)) {
    nm <- names(x)
    x <- as.double(x)
    names(x) <- nm
  }
  x
}

.ig_r_literal <- function(v) {
  one <- if (is.character(v)) {
    vapply(v, function(s) encodeString(s, quote = "\""), "", USE.NAMES = FALSE)
  } else if (is.integer(v)) {
    paste0(v, "L")
  } else if (is.double(v)) {
    .ig_format_double(v)
  } else if (is.logical(v)) {
    ifelse(v, "TRUE", "FALSE")
  } else {
    stop("cannot write a literal for type ", typeof(v), call. = FALSE)
  }
  if (length(one) == 1L) one else paste0("c(", paste(one, collapse = ", "), ")")
}

.ig_unclass_frozen <- function(x) {
  out <- unclass(x)
  attr(out, "class") <- NULL
  out
}

.ig_abort_frozen <- function(x) {
  .ig_abort("invalid_input", sprintf(
    "%s objects are immutable; create a new one (for specifications use update())",
    class(x)[1]))
}

`$<-.ig_frozen` <- function(x, name, value) .ig_abort_frozen(x)

`[[<-.ig_frozen` <- function(x, i, ..., value) .ig_abort_frozen(x)

`[<-.ig_frozen` <- function(x, i, ..., value) .ig_abort_frozen(x)
