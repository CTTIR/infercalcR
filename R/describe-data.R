#' Describe selected variables without hypothesis tests
#' @export
describe_data <- function(data, variables = setdiff(names(data), group), group = NULL,
                          labels = NULL, units = NULL, types = NULL) {
  if (!is.data.frame(data) || anyDuplicated(names(data))) .ic_abort("data must be a data.frame with unique names")
  if (!is.character(variables) || !length(variables) || anyNA(variables) || anyDuplicated(variables) || any(!variables %in% names(data))) .ic_abort("variables must name distinct columns")
  if (!is.null(group) && (!.ic_string(group) || !group %in% names(data))) .ic_abort("group must name one column")
  for (arg in list(labels = labels, units = units, types = types)) {
    if (!is.null(arg) && (is.null(names(arg)) || anyDuplicated(names(arg)) || any(!names(arg) %in% variables))) .ic_abort("labels, units and types must be uniquely named by selected variables")
  }
  if (!is.null(types) && any(!types %in% c("continuous", "categorical"))) .ic_abort("types must be continuous or categorical")
  g <- if (is.null(group)) rep("all", nrow(data)) else as.character(data[[group]])
  groups <- unique(g)
  if (!length(groups)) groups <- "all"
  rows <- list()
  for (j in seq_along(groups)) {
    keep <- if (is.na(groups[j])) is.na(g) else !is.na(g) & g == groups[j]
    for (v in variables) {
      x <- data[[v]][keep]
      if (!is.atomic(x) || !is.null(dim(x))) .ic_abort("descriptive columns must be atomic vectors")
      type <- if (!is.null(types) && v %in% names(types)) types[[v]] else if (is.numeric(x) && !is.factor(x)) "continuous" else "categorical"
      if (type == "continuous" && !is.numeric(x)) .ic_abort("continuous variables must be numeric")
      n <- sum(!is.na(x)); miss <- sum(is.na(x)); total <- length(x)
      add <- function(stat, estimate, level = NA_character_, proportion = NA_real_, status = "ok") {
        rows[[length(rows) + 1L]] <<- data.frame(row_id = paste0("description:", match(v, variables), ":", j, ":", length(rows) + 1L),
          variable_id = v, component = "description", estimand = stat, estimate = as.double(estimate),
          n = as.double(n), n.excluded = 0, status = status,
          reason = if (status == "ok") NA_character_ else "insufficient nonmissing observations",
          group = groups[j], group_missing = is.na(groups[j]), variable_type = type,
          label = if (!is.null(labels) && v %in% names(labels)) as.character(labels[[v]]) else v,
          unit = if (!is.null(units) && v %in% names(units)) as.character(units[[v]]) else NA_character_,
          level = level, proportion = proportion, n.missing = miss, n.total = total,
          stringsAsFactors = FALSE)
      }
      if (type == "continuous") {
        if (any(is.infinite(x))) .ic_abort("infinite descriptive measurements are not supported")
        z <- x[!is.na(x)]
        vals <- if (n) c(mean = mean(z), sd = if (n > 1L) stats::sd(z) else NA_real_,
          median = stats::median(z), q1 = unname(stats::quantile(z, .25)), q3 = unname(stats::quantile(z, .75)),
          min = min(z), max = max(z)) else stats::setNames(rep(NA_real_, 7), c("mean", "sd", "median", "q1", "q3", "min", "max"))
        for (nm in names(vals)) add(nm, vals[[nm]], status = if (is.na(vals[[nm]])) "undefined" else "ok")
      } else {
        levels <- if (is.factor(x)) levels(x) else sort(unique(as.character(x[!is.na(x)])))
        if (!length(levels)) add("count", NA_real_, status = "undefined")
        for (lev in levels) {
          count <- sum(!is.na(x) & as.character(x) == lev)
          add("count", count, lev, if (n) count / n else NA_real_)
        }
      }
    }
  }
  new_inference_result(do.call(rbind, rows), list(method_id = "descriptive_summary",
    population = "variable-specific nonmissing observations within each explicit group",
    missingness = "Missing values counted, excluded from summaries and categorical proportion denominators; missing groups retained.",
    group_variable = group, variables = variables, tests = "none", quantile_type = 7L), kind = "descriptive")
}
