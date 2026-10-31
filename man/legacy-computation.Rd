\name{legacy-computation}
\alias{attach_classification_data}
\alias{binormal_model}
\alias{binormal_rates}
\alias{boschloo_compare_fisher}
\alias{boschloo_control}
\alias{boschloo_plotdata_lattice}
\alias{boschloo_plotdata_nuisance}
\alias{boschloo_region}
\alias{boschloo_test}
\alias{boschloo_variants}
\alias{calculator_functions}
\alias{calibration_summary}
\alias{classification_auc}
\alias{classification_evaluated_spec}
\alias{classification_metrics}
\alias{classification_plot_data}
\alias{classification_spec}
\alias{classification_table}
\alias{classification_uncertainty}
\alias{classifier_model}
\alias{compare_classifiers}
\alias{cutoff_rule}
\alias{cutoff_sweep}
\alias{distribution_calculator}
\alias{evaluate_cutoff}
\alias{ig_status}
\alias{ig_status_codes}
\alias{is_ig_status}
\alias{metric_dictionary}
\alias{net_benefit}
\alias{ppv_gap_max_prevalence}
\alias{pr_summary}
\alias{predictive_values_at_prevalence}
\alias{prevalence_for_ppv_target}
\alias{prevalence_scenario}
\alias{select_cutoff}
\alias{boschloo_inference}
\title{Diagnostic and Exact Numerical Calculations}
\description{Preserved numerical procedures extracted from inferguidR, with explicit inputs, typed statuses and method settings. Calculation and display are separate.}
\usage{
attach_classification_data(spec = , data = )

binormal_model(auc = , sigma = 1)

binormal_rates(cutoff = , auc = , sigma = 1)

boschloo_compare_fisher(n1 = , n2 = , alpha = 0.05, variant = , p1 = NULL, p2 = NULL, fisher_convention = c("same", 
    "minlike"), control = boschloo_control())

boschloo_control(tie_tolerance = 1e-07, grid_points = 1001L, nuisance_tol = 1e-09, polish_tol = 1e-12, 
    max_levels = 60L, ambiguity_band = c(1e-12, 1e-06), cache = TRUE)

boschloo_plotdata_lattice(n1 = , n2 = , alpha = 0.05, variant = , p1 = NULL, p2 = NULL, observed = NULL, 
    pvalues = TRUE, control = boschloo_control())

boschloo_plotdata_nuisance(x = , grid_points = 401L, control = NULL)

boschloo_region(n1 = , n2 = , alpha = 0.05, variant = , control = boschloo_control())

boschloo_test(x1 = , n1 = , x2 = , n2 = , variant = , alpha = NULL, groups = c("group 1", "group 2"), 
    event = "event", control = boschloo_control())

boschloo_variants()

calculator_functions()

calibration_summary(spec = , bins = 10)

classification_auc(spec = , partial_auc = NULL, level = NULL)

classification_evaluated_spec(x = )

classification_metrics(x = , count_kind = c("observed", "frequency_weighted", "expected"), 
    lang = c("en", "de"))

classification_plot_data(x = , panel = .ig_cls_panels, model_id = NULL, basis = c("observed", 
    "scenario"), lang = c("en", "de"), ppv_levels = c(0.25, 0.5, 0.75), f1_levels = c(0.2, 
    0.4, 0.6, 0.8), ppv_target = NULL, metric_x = "fpr", metric_y = "tpr", realised_rules = NULL, 
    bins = 40)

classification_spec(data = NULL, truth = NULL, positive_label = NULL, scores = NULL, counts = NULL, 
    theory = NULL, rule = NULL, cutoff = NULL, models = NULL, direction = NULL, comparator = NULL, 
    score_scale = c("raw_score", "probability"), evaluation_split = NULL, negative_label = NULL, 
    prevalence = NULL, prevalence_source = NULL, sampling = NULL, weights = NULL, weight_type = "frequency", 
    cluster = NULL, id = NULL, missing = c("error", "exclude"), uncertainty = classification_uncertainty())

classification_table(x = , table = c("metrics", "intervals", "summaries", "operating_point", 
    "curve", "selection", "bounds", "tied", "scenario", "calibration", "net_benefit", "comparisons", 
    "limitations"), model_id = NULL, lang = c("en", "de"))

classification_uncertainty(level = 0.95, proportion_methods = c("wilson", "clopper_pearson"), 
    auc_method = c("delong", "none"), bootstrap_replicates = 0, seed = NULL, bootstrap_strata = c("class", 
        "none"), bootstrap_metrics = c("tpr", "fpr", "ppv", "npv", "youden_j", "f1", "mcc", 
        "roc_auc", "average_precision"))

classifier_model(model_id = , name = model_id, version = NULL, score_meaning = NULL, population = NULL, 
    unit = NULL, source_hash = NULL)

compare_classifiers(spec = , level = NULL)

cutoff_rule(cutoff = NULL, selection = NULL, selection_data = NULL, constraints = NULL, optimise = NULL, 
    capacity = NULL, costs = NULL, tie_break = NULL, prevalence_basis = NULL)

cutoff_sweep(x = , metrics = .ig_cls_default_sweep_metrics, model_id = NULL, grid = 401, basis = c("observed", 
    "scenario"))

distribution_calculator(expression = )

evaluate_cutoff(spec = , cutoff = NULL, model_id = NULL, partial_auc = NULL, threshold_probability = NULL, 
    calibration_bins = 10, include_curves = TRUE)

ig_status(code = , reason = "", details = list())

ig_status_codes()

is_ig_status(x = )

metric_dictionary(lang = c("en", "de"), scope = NULL)

net_benefit(spec = , threshold_probability = )

ppv_gap_max_prevalence(tpr = , fpr = )

pr_summary(spec = , prevalence = NULL)

predictive_values_at_prevalence(tpr = , fpr = , prevalence = )

prevalence_for_ppv_target(tpr = , fpr = , target = )

prevalence_scenario(x = , prevalence = , n_total = NULL, source = c("assumed", "external"), 
    lang = c("en", "de"))

select_cutoff(spec = , objective = NULL, constraints = NULL, optimise = NULL, capacity = NULL, 
    costs = NULL, tie_break = NULL, prevalence_basis = NULL, ... = )

boschloo_inference(x1 = , n1 = , x2 = , n2 = , variant = , alpha = NULL, groups = c("group 1", 
    "group 2"), event = "event", control = boschloo_control())
}
\arguments{
\item{spec}{A classification specification returned by classification_spec().}
\item{data}{A data frame containing observations, with explicit truth and score columns.}
\item{auc}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{sigma}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{cutoff}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{n1}{Number of independent binomial observations in group 1.}
\item{n2}{Number of independent binomial observations in group 2.}
\item{alpha}{Decision level strictly between zero and one; NULL omits a decision where supported.}
\item{variant}{Explicit Boschloo variant: minlike, central, less, or greater. Classifier variants are retained in their records.}
\item{p1}{Event probability in group 1.}
\item{p2}{Event probability in group 2.}
\item{fisher_convention}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{control}{Numerical settings from boschloo_control().}
\item{tie_tolerance}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{grid_points}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{nuisance_tol}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{polish_tol}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{max_levels}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{ambiguity_band}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{cache}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{observed}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{pvalues}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{x}{Input data, finalized result, or numeric values as specified by the selected function.}
\item{x1}{Number of events in group 1.}
\item{x2}{Number of events in group 2.}
\item{groups}{Two distinct group labels in group 1, group 2 order.}
\item{event}{Explicit event label.}
\item{bins}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{partial_auc}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{level}{Confidence level strictly between zero and one.}
\item{count_kind}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{lang}{Display-label language, en or de.}
\item{panel}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{model_id}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{basis}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{ppv_levels}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{f1_levels}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{ppv_target}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{metric_x}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{metric_y}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{realised_rules}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{truth}{Name of the observed truth column.}
\item{positive_label}{Explicit positive-class value; never inferred.}
\item{scores}{Score column name or named score-column vector.}
\item{counts}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{theory}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{rule}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{models}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{direction}{Declared score direction, higher_is_positive or lower_is_positive.}
\item{comparator}{Declared cutoff comparison, consistent with direction.}
\item{score_scale}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{evaluation_split}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{negative_label}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{prevalence}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{prevalence_source}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{sampling}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{weights}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{weight_type}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{cluster}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{id}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{missing}{Explicit missingness policy; see the preserved specification settings.}
\item{uncertainty}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{table}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{proportion_methods}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{auc_method}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{bootstrap_replicates}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{seed}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{bootstrap_strata}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{bootstrap_metrics}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{name}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{version}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{score_meaning}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{population}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{unit}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{source_hash}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{selection}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{selection_data}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{constraints}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{optimise}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{capacity}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{costs}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{tie_break}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{prevalence_basis}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{metrics}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{grid}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{expression}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{threshold_probability}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{calibration_bins}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{include_curves}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{code}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{reason}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{details}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{scope}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{tpr}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{fpr}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{target}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{n_total}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{source}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{objective}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{...}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
}
\details{Boschloo tests use Fisher ordering and bounded nuisance maximization. The central variant doubles the smaller separately maximized one-sided value. Rejection uses the numerical upper bound; this bound is not a sampling confidence interval. The shared Boschloo inference path requires only standard R. Historical hashed specifications and the legacy calculator decimal parser require the optional packages digest and/or jsonlite and give an explicit missing-dependency error when absent.

Classifier specifications preserve the positive class, direction, cutoff origin, missing-value exclusions, evaluation split and raw-data fingerprint. Raw observations are not retained in the common reporting payload. Basic count metrics, binormal rates and static dictionary access do not require contributed packages. Undefined metrics retain statuses and denominator reasons.

Provider functions are public interfaces for validated package consumers. Historical canonical primitives accept already converted plain data; guidance-owned schema dispatch remains in inferguidR. No provider evaluates code from input strings. Calculator expressions use a restricted parser.

The inherited validation metadata records prior scoped checks; package extraction and adapter parity do not certify all scientific designs.}
\value{Computational functions return typed lists or ordinary data frames; common adapters return inference_result objects. Metadata providers return plain lists or vectors, probability kernels return numeric values, and validators signal typed errors for invalid inputs.}
\examples{
r <- boschloo_inference(1, 5, 4, 7, variant = "less")
result_data(r)
classification_metrics(c(tp = 8, fn = 2, fp = 3, tn = 7))
}
