# Numerical definitions extracted from the existing implementation.
# Source: inferguidR e91938f77eaa991b69989b121bfd32c124c95db0, tpl.classifier_heldout_validation.v1.R
# Example execution and plotting are deliberately outside this module.

.obs_classifier_heldout_validation_validate_data <- function(d) {
    stopifnot(is.data.frame(d), all(c("id", "truth", "score") %in% names(d)), nrow(d) >= 2L, is.character(d$id), 
        !anyNA(d$id), all(nzchar(d$id)), !anyDuplicated(d$id), is.numeric(d$truth), !anyNA(d$truth), 
        all(d$truth %in% c(0, 1)), is.numeric(d$score), all(is.finite(d$score)))
    invisible(d)
}

.obs_classifier_heldout_validation_make_spec <- function(d, cutoff, direction, comparator, split, origin, 
    conf.level, intervals) {
    .obs_classifier_heldout_validation_validate_data(d)
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

.obs_classifier_heldout_validation_analyse <- function(tuning, evaluation, direction, comparator, independent_splits, 
    conf.level = 0.95) {
    stopifnot(isTRUE(independent_splits))
    .obs_classifier_heldout_validation_validate_data(tuning)
    .obs_classifier_heldout_validation_validate_data(evaluation)
    stopifnot(length(unique(tuning$truth)) == 2L)
    train <- .obs_classifier_heldout_validation_make_spec(tuning, 0, direction, comparator, "held_out_tuning", 
        "held_out_tuning", conf.level, FALSE)
    test <- .obs_classifier_heldout_validation_make_spec(evaluation, 0, direction, comparator, "held_out_test", 
        "held_out_tuning", conf.level, TRUE)
    selected <- select_cutoff(train, "max_youden", tie_break = "fewest_alerts")
    result <- evaluate_cutoff(test, cutoff = selected, include_curves = TRUE)
    list(method = "Declared held-out evaluation of a tuning-selected Youden rule", status = result$status$code, 
        selected_rule = selected, operating_point = result$models[[1]]$operating_point, metrics = classification_table(result, 
            "metrics"), intervals = classification_table(result, "intervals"), summaries = classification_table(result, 
            "summaries"), specification = classification_evaluated_spec(result), result = result, limitations = "The prespecified example maximises Youden J on tuning data only, with fewest-alerts tie break. Evaluation outcomes never select the rule. Known shared unit IDs are rejected; disjoint IDs and a declaration cannot prove independence or exclude model-training leakage. Identifiers must share a namespace. Conditional marginal Clopper-Pearson intervals omit training and tuning uncertainty; no resampling validation is implemented here.")
}

