# Extracted from inferguidR e91938f77eaa991b69989b121bfd32c124c95db0.
# Original source and MIT license: Raban Heller. See tools/extraction/source-map.csv.

.ig_nonfinite_codes <- c("positive_infinity", "negative_infinity", "not_a_number")

.ig_json_max_bytes <- 64 * 1024^2

.ig_decimal_pattern <- "^[+-]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$"

.ig_parse_decimal <- function(txt) {
  .ic_legacy_need("jsonlite")
  if (!length(txt)) return(double())
  if (!is.character(txt) || anyNA(txt) || !all(grepl(.ig_decimal_pattern, txt, perl = TRUE))) {
    .ig_abort("invalid_input", "number text must be a plain decimal number")
  }
  negative <- startsWith(txt, "-")
  body <- sub("^[+-]", "", txt)
  exponent <- sub("^[^eE]*", "", body)
  mantissa <- sub("[eE].*$", "", body)
  whole <- sub("^0+", "", sub("\\..*$", "", mantissa))
  whole[!nzchar(whole)] <- "0"
  fraction <- ifelse(grepl(".", mantissa, fixed = TRUE), sub("^[^.]*\\.", "", mantissa), "")
  json <- paste0(whole, ifelse(nzchar(fraction), paste0(".", fraction), ""), exponent)
  value <- as.double(unlist(jsonlite::parse_json(paste0("[", paste(json, collapse = ","), "]")),
                            use.names = FALSE))
  ifelse(negative, -value, value)
}

.ig_format_double <- function(x) {
  out <- as.character(x)
  fin <- which(is.finite(x))
  if (!length(fin)) return(out)
  xf <- x[fin]
  txt <- sprintf("%.15g", xf)
  bad <- .ig_parse_decimal(txt) != xf
  if (any(bad)) {
    txt[bad] <- sprintf("%.16g", xf[bad])
    bad <- .ig_parse_decimal(txt) != xf
    if (any(bad)) txt[bad] <- sprintf("%.17g", xf[bad])
  }
  txt[xf == 0] <- "0"
  out[fin] <- txt
  out
}

.ig_nonfinite_marker <- function(code) {
  sprintf('{"nonfinite":"%s","value":null}', code)
}

.ig_json_atomic_elements <- function(x, path) {
  .ic_legacy_need("jsonlite")
  if (is.factor(x)) x <- as.character(x)
  if (is.logical(x)) {
    out <- ifelse(x, "true", "false")
    out[is.na(x)] <- "null"
    return(out)
  }
  if (is.integer(x)) {
    out <- as.character(x)
    out[is.na(x)] <- "null"
    return(out)
  }
  if (is.double(x)) {
    fin <- is.finite(x)
    if (all(fin)) return(.ig_format_double(x))
    out <- character(length(x))
    out[fin] <- .ig_format_double(x[fin])
    out[is.na(x) & !is.nan(x)] <- "null"
    out[is.nan(x)] <- .ig_nonfinite_marker("not_a_number")
    out[!is.na(x) & x == Inf] <- .ig_nonfinite_marker("positive_infinity")
    out[!is.na(x) & x == -Inf] <- .ig_nonfinite_marker("negative_infinity")
    return(out)
  }
  if (is.character(x)) {
    x <- enc2utf8(x)
    # Printable ASCII needs no escaping except quotes, backslashes and HTML
    # closing-tag prefixes. Keep all other strings on the established encoder.
    safe <- !is.na(x) & !grepl('[^ -~]|["\\\\<]', x, useBytes = TRUE)
    out <- rep("null",length(x))
    if(any(safe)) out[safe] <- paste0('"',x[safe],'"')
    escaped <- which(!is.na(x) & !safe)
    if(length(escaped)) out[escaped] <- vapply(x[escaped],function(s)
      as.character(jsonlite::toJSON(s,auto_unbox=TRUE)),character(1),USE.NAMES=FALSE)
    return(out)
  }
  stop(sprintf("cannot serialise a value of type '%s' at %s", typeof(x),
               if (nzchar(path)) path else "<root>"), call. = FALSE)
}

.ig_json_object <- function(keys, values, path) {
  if (anyNA(keys) || !all(nzchar(keys))) {
    stop(sprintf("partially named list at %s cannot be serialised",
                 if (nzchar(path)) path else "<root>"), call. = FALSE)
  }
  if (anyDuplicated(keys)) {
    stop(sprintf("duplicate key '%s' at %s", keys[anyDuplicated(keys)],
                 if (nzchar(path)) path else "<root>"), call. = FALSE)
  }
  ord <- order(keys, method = "radix")
  key_txt <- .ig_json_atomic_elements(keys[ord], path)
  paste0("{", paste0(key_txt, ":", values[ord], collapse = ","), "}")
}

.ig_json_value <- function(x, path = "") {
  if (is.null(x)) return("null")
  if (inherits(x, "ig_status")) x <- .ig_status_to_list(x)
  if (is.function(x) || is.environment(x) || inherits(x, "formula")) {
    stop(sprintf("cannot serialise a %s at %s", class(x)[1],
                 if (nzchar(path)) path else "<root>"), call. = FALSE)
  }
  if (is.data.frame(x)) x <- as.list(x)
  if (is.list(x)) {
    nms <- names(x)
    if (length(x) == 0L) return(if (is.null(nms)) "[]" else "{}")
    if (is.null(nms)) {
      parts <- vapply(seq_along(x), function(i) {
        .ig_json_value(x[[i]], paste0(path, "[", i, "]"))
      }, character(1))
      return(paste0("[", paste(parts, collapse = ","), "]"))
    }
    parts <- vapply(seq_along(x), function(i) {
      .ig_json_value(x[[i]], paste0(path, "/", nms[i]))
    }, character(1))
    return(.ig_json_object(nms, parts, path))
  }
  if (!is.atomic(x)) {
    stop(sprintf("cannot serialise a value of type '%s' at %s", typeof(x),
                 if (nzchar(path)) path else "<root>"), call. = FALSE)
  }
  elements <- .ig_json_atomic_elements(unname(x), path)
  if (!is.null(names(x))) {
    return(.ig_json_object(names(x), elements, path))
  }
  if (length(x) == 1L && !inherits(x, "AsIs")) return(elements)
  paste0("[", paste(elements, collapse = ","), "]")
}

.ig_canonical_json <- function(x) {
  enc2utf8(.ig_json_value(.ig_to_plain(x)))
}

.ig_sha256 <- function(txt) {
  .ic_legacy_need("digest")
  digest::digest(enc2utf8(txt), algo = "sha256", serialize = FALSE)
}

.ig_hash_value <- function(x) .ig_sha256(.ig_canonical_json(x))

.ig_json_parse <- function(txt) {
  .ic_legacy_need("jsonlite")
  if (inherits(txt, "json")) txt <- unclass(txt)
  if (!is.character(txt) || length(txt) < 1L || anyNA(txt)) {
    .ig_abort("invalid_input", "JSON input must be a character string")
  }
  txt <- paste(txt, collapse = "\n")
  if (nchar(txt, type = "bytes") > .ig_json_max_bytes) {
    .ig_abort("invalid_input", sprintf("JSON input exceeds %d bytes", .ig_json_max_bytes))
  }
  parsed <- tryCatch(
    jsonlite::parse_json(txt, simplifyVector = FALSE),
    error = function(e) {
      .ig_abort("invalid_input", paste("input is not valid JSON:", conditionMessage(e)))
    }
  )
  dup <- .ig_find_duplicate_keys(parsed)
  if (length(dup)) {
    .ig_abort_invalid(sprintf("duplicate key '%s'", dup),
                      "JSON objects must not repeat keys")
  }
  .ig_decode_nonfinite(.ig_json_restore_numbers(parsed, txt))
}

.ig_json_restore_numbers <- function(parsed, txt) {
  pattern <- '"(?:[^"\\\\]++|\\\\.)*+"(*SKIP)(*F)|-?(?:0|[1-9][0-9]*)(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?'
  positions <- gregexpr(pattern, txt, perl = TRUE)
  tokens <- regmatches(txt, positions)[[1L]]
  values <- .ig_parse_decimal(tokens)
  cursor <- 0L
  restore <- function(x) {
    if (is.numeric(x)) {
      cursor <<- cursor + 1L
      if (length(x) != 1L || cursor > length(values))
        .ig_abort("invalid_input", "JSON numeric token structure mismatch")
      value <- values[[cursor]]
      if (is.integer(x) && is.finite(value) && value == x) x else value
    } else if (is.list(x)) {
      for (i in seq_along(x)) x[i] <- list(restore(x[[i]]))
      x
    } else x
  }
  out <- restore(parsed)
  if (cursor != length(values))
    .ig_abort("invalid_input", "JSON numeric token count mismatch")
  out
}

.ig_find_duplicate_keys <- function(x, path = "") {
  if (!is.list(x)) return(character())
  out <- character()
  nms <- names(x)
  if (!is.null(nms)) {
    d <- unique(nms[duplicated(nms)])
    if (length(d)) out <- c(out, paste0(path, "/", d))
  }
  for (i in seq_along(x)) {
    sub <- if (is.null(nms)) paste0(path, "[", i, "]") else paste0(path, "/", nms[i])
    out <- c(out, .ig_find_duplicate_keys(x[[i]], sub))
  }
  out
}

.ig_is_nonfinite_marker <- function(x) {
  is.list(x) && length(x) == 2L && setequal(names(x), c("nonfinite", "value")) &&
    is.null(x$value) && .ig_is_string(x$nonfinite) &&
    x$nonfinite %in% .ig_nonfinite_codes
}

.ig_decode_nonfinite <- function(x) {
  if (.ig_is_nonfinite_marker(x)) {
    return(switch(x$nonfinite,
                  positive_infinity = Inf,
                  negative_infinity = -Inf,
                  not_a_number = NaN))
  }
  if (is.list(x)) {
    for (i in seq_along(x)) {
      val <- .ig_decode_nonfinite(x[[i]])
      if (!is.null(val)) x[[i]] <- val
    }
  }
  x
}

