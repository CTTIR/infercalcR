# Common views contain finalized quantities; conversion never runs inference.
.ic_plain_legacy <- function(x) {
  if (is.data.frame(x)) return(as.data.frame(lapply(x, .ic_plain_legacy), stringsAsFactors = FALSE))
  if (is.list(x)) {
    out <- lapply(x, .ic_plain_legacy)
    attributes(out) <- list(names = names(x))
    return(out)
  }
  if (is.factor(x)) return(as.character(x))
  attributes(x) <- NULL
  x
}

#' Boschloo inference without historical identity dependencies
#' @export
boschloo_inference <- function(x1, n1, x2, n2, variant, alpha = NULL,
                                groups = c("group 1", "group 2"), event = "event",
                                control = boschloo_control()) {
  native <- .bl_test_result(x1, n1, x2, n2, if (missing(variant)) NULL else variant,
                            alpha, groups, event, control, identity = FALSE)
  as_inference(native)
}

#' @export
as_inference.boschloo_test <- function(x, ...) {
  if (length(list(...))) .ic_abort("unused arguments in Boschloo conversion")
  st <- x$status$code
  if (identical(x$decision, "unresolved")) st <- "unresolved"
  d <- x$data
  row <- data.frame(row_id = "comparison", component = "two_group_test",
    estimand = "event probability difference (group 1 minus group 2)",
    contrast = paste(d$group_labels, collapse = " - "),
    estimate = d$proportion_1 - d$proportion_2,
    p.value = x$p_value, p.upper = x$p_upper, null.value = 0,
    alternative = switch(x$variant$variant, less = "less", greater = "greater", "two.sided"),
    n = d$n1 + d$n2, n.excluded = 0,
    status = st, reason = if (st == "unresolved") "numerical bounds straddle alpha" else x$status$reason,
    decision = x$decision %||% NA_character_, stringsAsFactors = FALSE)
  new_inference_result(row, list(method_id = x$variant$advisor_variant_id,
    variant_id = x$variant$variant_id, method = x$variant$label,
    orientation = d[c("group_labels", "event_label", "orientation")],
    settings = list(control = x$settings, alpha = x$alpha, null = x$variant$null),
    sample_sizes = c(group_1 = d$n1, group_2 = d$n2),
    validation = x$variant$validation_status,
    numerical_bounds = "p.value is attained lower bound; p.upper is rigorous numerical upper bound, not a sampling interval",
    assumptions = x$variant$design), payload = list(native = .ic_plain_legacy(x)))
}
result_data.boschloo_test <- function(x, ...) result_data(as_inference(x), ...)
result_metadata.boschloo_test <- function(x, ...) result_metadata(as_inference(x), ...)

.ic_classifier_status <- function(x, reason_code = NULL) {
  out <- x
  out[x == "positive_infinity"] <- "undefined"
  out[x == "not_computed"] <- "unsupported"
  if (!is.null(reason_code)) {
    out[x == "not_computed" & !is.na(reason_code) & reason_code == "not_requested"] <- "not_applicable"
    out[x == "not_computed" & !is.na(reason_code) & reason_code == "outside_double_range"] <- "numerical_failure"
  }
  out[is.na(out) | !out %in% .ic_statuses] <- "undefined"
  out
}

#' @export
as_inference.classification_result <- function(x, ...) {
  if (inherits(x, "inference_result")) return(as_inference.inference_result(x, ...))
  if (length(list(...))) .ic_abort("unused arguments in classifier conversion")
  if (!is.list(x$models) || !is.list(x$status)) .ic_abort("malformed legacy classification result")
  rows <- list()
  add <- function(v) { rows[[length(rows) + 1L]] <<- v }
  variant_key <- function(z) ifelse(is.na(z), "", z)
  excluded <- x$diagnostics$n_excluded_rows %||% NA_real_
  for (m in x$models) {
    for (kind in c("metrics", "summaries")) {
      tab <- .ig_cls_frame(m[[kind]], kind)
      if (!nrow(tab)) next
      ints <- .ig_cls_frame(m$intervals, "intervals")
      for (i in seq_len(nrow(tab))) {
        r <- tab[i, , drop = FALSE]
        v <- if ("variant" %in% names(r)) variant_key(r$variant) else ""
        ii <- which(ints$metric_id == r$metric_id & variant_key(ints$variant) == v)
        if (!length(ii)) ii <- NA_integer_
        for (j in ii) {
          ci <- if (is.na(j)) NULL else ints[j, , drop = FALSE]
          op <- .ig_cls_frame(m$operating_point, "operating_point")
          n <- if (nrow(op) && m$count_kind %in% c("observed", "frequency_weighted")) op$n_total[1] else NA_real_
          row <- data.frame(row_id = paste(m$model_id,kind,r$metric_id,v,if(is.na(j)) "point" else paste(ci$method,j,sep="-"),sep=":"),
            variable_id = m$model_id, component = kind, estimand = r$metric_id,
            estimate = r$value, conf.low = if(is.null(ci)) NA_real_ else ci$lower,
            conf.high = if(is.null(ci)) NA_real_ else ci$upper,
            conf.level = if(is.null(ci)) NA_real_ else ci$level,
            interval.method = if(is.null(ci)) NA_character_ else ci$method,
            interval.scale = if(is.null(ci)) NA_character_ else "metric",
            interval.scope = if(is.null(ci)) NA_character_ else "pointwise",
            n = n, n.excluded = excluded, status = .ic_classifier_status(r$status, r$reason_code),
            reason = r$reason, source_status = r$status,
            interval.status = if(is.null(ci)) NA_character_ else ci$status,
            interval.reason = if(is.null(ci)) NA_character_ else ci$reason,
            conditioning = if(is.null(ci)) NA_character_ else ci$conditioning,
            denominator = if("denominator" %in% names(r)) r$denominator else NA_character_,
            denominator_value = if("denominator_value" %in% names(r)) r$denominator_value else NA_real_,
            estimate_kind = r$estimate_kind, variant = v,
            stringsAsFactors = FALSE)
          add(row)
        }
      }
    }
    if (is.null(m$metrics) && is.null(m$summaries)) {
      add(data.frame(row_id=paste0(m$model_id,":unavailable"),variable_id=m$model_id,
        component="model",estimand="unavailable",status=.ic_classifier_status(m$status$code),
        reason=m$status$reason,stringsAsFactors=FALSE))
    }
  }
  cmp <- .ig_cls_frame(x$comparisons,"comparisons")
  for (i in seq_len(nrow(cmp))) {
    r <- cmp[i,,drop=FALSE]
    add(data.frame(row_id=paste("paired_auc",r$model_a,r$model_b,sep=":"),
      component="paired_model_comparison",estimand="ROC AUC difference (model A minus model B)",
      contrast=paste(r$model_a,r$model_b,sep=" - "),estimate=r$difference,
      statistic=r$z,p.value=r$p_value,null.value=0,alternative="two.sided",
      conf.low=r$lower,conf.high=r$upper,conf.level=r$level,
      interval.method=r$method,interval.scale="AUC difference",interval.scope="pointwise",
      status=.ic_classifier_status(r$inference_status, r$reason_code),reason=r$reason,
      stringsAsFactors=FALSE))
  }
  if (!length(rows)) .ic_abort("classification result has no retained numerical rows", "unsupported")
  # Constructors fill schema columns; combine their normalized frames safely.
  frames <- lapply(rows,function(r)new_inference_result(r,list(method_id="classification"),kind="classification")$data)
  extra <- unique(unlist(lapply(frames,names)))
  for (i in seq_along(frames)) for(nm in setdiff(extra,names(frames[[i]]))) {
    prototype <- frames[[which(vapply(frames,function(z)nm %in% names(z),logical(1)))[1]]][[nm]]
    frames[[i]][[nm]] <- if(is.numeric(prototype)) NA_real_ else NA_character_
  }
  d <- do.call(rbind,lapply(frames,function(z)z[,extra,drop=FALSE]));rownames(d)<-NULL
  new_inference_result(d, list(method_id="classification",variant_id=x$input_kind,
    result_id=if(!is.null(x$spec_hash))paste0("classification:",x$spec_hash) else "classification:result",
    evaluation=.ic_plain_legacy(x$evaluation),exclusions=.ic_plain_legacy(x$diagnostics),
    orientation=lapply(x$models,function(m)m[c("model_id","direction","comparator","score_scale","tie_rule","evaluation_split")]),
    settings=.ic_plain_legacy(x$settings),uncertainty=.ic_plain_legacy(x$uncertainty),
    prevalence=.ic_plain_legacy(x$prevalence),validation=.ic_plain_legacy(x$validation),
    limitations=.ic_plain_legacy(x$limitations)),kind="classification",payload=list(native=.ic_plain_legacy(x)))
}
result_data.classification_result <- function(x, ...) {
  if(inherits(x,"inference_result")) return(result_data.inference_result(x,...))
  result_data(as_inference(x),...)
}
result_metadata.classification_result <- function(x, ...) {
  if(inherits(x,"inference_result")) return(result_metadata.inference_result(x,...))
  result_metadata(as_inference(x),...)
}
