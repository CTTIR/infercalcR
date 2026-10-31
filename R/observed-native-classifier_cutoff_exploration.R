# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.classifier_cutoff_exploration.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_classifier_cutoff_exploration_validate_data <- function(d) {
    stopifnot(is.data.frame(d), all(c("id", "truth", "score") %in% names(d)), nrow(d) >= 2L, is.character(d$id), 
        !anyNA(d$id), all(nzchar(d$id)), !anyDuplicated(d$id), is.numeric(d$truth), !anyNA(d$truth), 
        all(d$truth %in% c(0, 1)), is.numeric(d$score), all(is.finite(d$score)))
    invisible(d)
}

.obs_classifier_cutoff_exploration_make_spec <- function(d, cutoff, direction, comparator, split, origin, 
    conf.level, intervals) {
    .obs_classifier_cutoff_exploration_validate_data(d)
    stopifnot(is.character(direction), length(direction) == 1L, direction %in% c("higher_is_positive", 
        "lower_is_positive"), is.character(comparator), length(comparator) == 1L, comparator %in% if (direction == 
        "higher_is_positive") 
        c(">=", ">")
    else c("<=", "<"))
    classification_spec(d, truth = "truth", positive_label = 1, negative_label = 0, scores = "score", 
        id = "id", models = classifier_model("score", score_meaning = "declared numeric classifier score"), 
        direction = direction, comparator = comparator, score_scale = "raw_score", sampling = "cross_sectional", 
        evaluation_split = split, rule = cutoff_rule(cutoff = cutoff, selection_data = origin), uncertainty = classification_uncertainty(level = conf.level, 
            proportion_methods = if (intervals) 
                "clopper_pearson"
            else character(), auc_method = "none"))
}

.obs_classifier_cutoff_exploration_analyse <- function(data, direction, comparator) {
    spec <- .obs_classifier_cutoff_exploration_make_spec(data, 0, direction, comparator, "same_data", 
        "same_data", 0.95, FALSE)
    result <- evaluate_cutoff(spec, include_curves = TRUE)
    list(method = "Descriptive cutoff exploration", status = result$status$code, decision_sets = classification_table(result, 
        "curve"), summaries = classification_table(result, "summaries"), specification = spec, result = result, 
        limitations = "All attainable decision sets are enumerated on these data. No optimal cutoff is certified, no held-out validation or sampling interval is claimed. Scores must belong to independent units; no weights, clusters, missing-data deletion or probability calibration is implied.")
}

