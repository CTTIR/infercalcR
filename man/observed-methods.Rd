\name{observed-methods}
\alias{observed-methods}
\title{Explicit observed methods and finalized numerical results}
\description{Execute a prespecified procedure through \code{method_spec()} and
\code{run_inference()}. Each procedure retains its existing finite-sample,
numerical, missingness and backend policies.}
\details{Wilcoxon intervals retain the native-reported confidence level. Older R
versions can record a nominal or rounded level whose exact attainment differs;
newer versions can return a fully unbounded conservative interval. The common
interval-method label and metadata identify this convention. No level, endpoint,
or captured native diagnostic is replaced.

Arguments listed below are supplied by name to \code{run_inference()}; method
controls may instead be declared in the named \code{options} list in
\code{method_spec()}. Alternative and confidence level belong in the specification.
An optional \code{result_id} argument to \code{run_inference()} supplies an
explicit external result key for joins. Default IDs are stable only within one
object, so supply distinct IDs when joining separate results.
For classifier-template views, native payloads omit the original specification
and attached source result; finalized aggregate fields remain, with the raw-data-free
classifier conversion payload stored separately. The legacy teaching wrappers
continue to use public classifier APIs for their original specification shape.
The method never selects an alternative or a test from observed diagnostic p-values.
\code{result_data()} returns explicit effect, coefficient, omnibus or descriptive
rows. \code{result_metadata()} records method identity, settings, population and
source status. \code{result$payload$native} retains the complete finalized native
output, including quantities that do not fit a scalar row. Numeric p-value bounds
and Monte Carlo intervals are separate from sampling intervals.

For paired and repeated inputs, use the documented wide data with a confirmed
unique subject identifier. Missing values are rejected unless that procedure's
explicit complete-case, MAR, MCAR or pairwise policy is selected. Setting
\code{missing = "omit"} alone never selects a missingness assumption. Unknown
sample sizes remain unknown. Group, event, contrast and sign conventions follow
the named procedure and its explicit inputs.

The base/standard-R procedures require no contributed calculation package.
Gower/PAM uses \pkg{cluster}; Gaussian mixed models use \pkg{nlme}; NB2 and ordinal
models use \pkg{MASS}; multinomial models use \pkg{nnet}; logistic random-intercept
models use \pkg{lme4}; exchangeable GEE uses \pkg{geepack}. Missing backends produce
an explicit error and never a statistical substitute. Legacy classifier objects
may use the separately documented canonical JSON/fingerprint dependencies.

The binary logistic procedure retains profile-likelihood slope intervals and
separate Wald columns. Generic fitted-glm conversion instead explicitly offers
Wald intervals. Factorial ANOVA retains raw and Holm-adjusted term p-values for
the declared whole family. ANCOVA intervals and contrasts are marginal and
unadjusted. Rank location intervals are not general median-difference intervals.
Prediction-development bootstraps refit preprocessing and preserve failed
replicates; no external-validation claim follows. Structure/clustering outputs
are exploratory and carry no fabricated hypothesis test.

\code{variance_chisq_one_sample} is an advisor-only identity and gives an
unsupported condition. The following 68 implemented identities retain these
native input signatures (\code{alternative} and \code{conf.level}, when shown,
are supplied through the specification):
\itemize{
\item \code{ancova_baseline_adjusted}: \code{data, covariates, group_levels, common_slopes, conf.level, missing, alternative}.
\item \code{anova_factorial}: \code{data, factor_columns, independent_units, equal_variance_justified, alternative, conf.level, complete_cases}.
\item \code{anova_one_way_welch}: \code{x, group, alternative, conf.level}.
\item \code{anova_one_way}: \code{x, group, alternative, conf.level}.
\item \code{anova_repeated_one_way_gg}: \code{data, occasion_columns, independent_subjects, alternative, conf.level, complete_cases}.
\item \code{binomial_exact_one_sample}: \code{events, n, p0, alternative, conf.level}.
\item \code{boschloo_one_sided}: \code{table, alternative, conf.level}.
\item \code{boschloo_two_sided_central}: \code{table, alternative, conf.level}.
\item \code{boschloo_two_sided_minlike}: \code{table, alternative, conf.level}.
\item \code{bowker_symmetry}: \code{counts, alternative, conf.level, n_excluded}.
\item \code{brunner_munzel}: \code{x, y, alternative, conf.level}.
\item \code{chisq_goodness_of_fit_mc}: \code{observed, probabilities, seed, B, conf.level, mc.level}.
\item \code{chisq_goodness_of_fit}: \code{counts, p, alternative, conf.level}.
\item \code{chisq_independence_rxc}: \code{counts, alternative, conf.level}.
\item \code{classifier_binormal_theory}: \code{auc, sigma, cutoff, prevalence, n_total}.
\item \code{classifier_cutoff_exploration}: \code{data, direction, comparator}.
\item \code{classifier_heldout_validation}: \code{tuning, evaluation, direction, comparator, independent_splits, conf.level}.
\item \code{classifier_predeclared_rule}: \code{data, cutoff, direction, comparator, predeclared, conf.level}.
\item \code{cluster_gower_pam}: \code{data, variables, types, weights, k, independent_units, alternative, conf.level, missing, B, seed}.
\item \code{cluster_hierarchical_ward}: \code{data, variables, k, standardize, independent_units, alternative, conf.level, complete_cases, B, seed}.
\item \code{cluster_kmeans}: \code{data, variables, k, standardize, independent_units, alternative, conf.level, complete_cases, B, seed}.
\item \code{cochran_armitage_trend}: \code{events, totals, scores, scores_prespecified, alternative, conf.level}.
\item \code{cochran_q}: \code{data, alternative, conf.level, missing}.
\item \code{correlation_kendall}: \code{data, alternative, conf.level}.
\item \code{correlation_pearson}: \code{data, alternative, conf.level}.
\item \code{correlation_spearman}: \code{data, alternative, conf.level}.
\item \code{descriptive_summary}: \code{x, measurement, conf.level, event}.
\item \code{diagnostic_2x2_fixed_cutoff}: \code{tp, fn, fp, tn, sampling, cutoff_origin, evaluation_split, conf.level, prevalence, prevalence_source}.
\item \code{efa_common_factor_ml}: \code{data, variables, factors, independent_units, continuous_measurements, alternative, conf.level, complete_cases}.
\item \code{fisher_exact_2x2}: \code{table, alternative, conf.level}.
\item \code{fisher_exact_rxc}: \code{counts, alternative, conf.level}.
\item \code{friedman}: \code{data, alternative, conf.level, missing}.
\item \code{gee_logistic_exchangeable}: \code{data, independent_clusters, model_assumptions_justified, alternative, conf.level, missing_outcomes}.
\item \code{glmm_logistic_random_intercept}: \code{data, independent_clusters, model_assumptions_justified, alternative, conf.level, missing_outcomes}.
\item \code{jonckheere_terpstra}: \code{data, group_order, order_prespecified, independent_units, seed, alternative, B, conf.level, mc.level, complete_cases}.
\item \code{kruskal_wallis}: \code{x, group, alternative, conf.level}.
\item \code{lmm_cluster_random_intercept}: \code{data, independent_clusters, random_intercept_justified, alternative, conf.level, missing_outcomes}.
\item \code{lmm_repeated_measures}: \code{data, independent_clusters, random_intercept_justified, alternative, conf.level, missing_outcomes}.
\item \code{mcnemar_asymptotic_cc}: \code{counts, alternative, conf.level}.
\item \code{mcnemar_asymptotic_uncorrected}: \code{counts, alternative, conf.level}.
\item \code{mcnemar_exact_conditional}: \code{counts, alternative, conf.level}.
\item \code{mcnemar_midp}: \code{counts, alternative, conf.level}.
\item \code{pca_components}: \code{data, variables, components, standardise, missing}.
\item \code{prediction_model_linear}: \code{data, predictors, independent_units, alternative, conf.level, B, seed, complete_cases, resamples}.
\item \code{prediction_model_logistic}: \code{data, predictors, independent_units, alternative, conf.level, B, seed, complete_cases, resamples}.
\item \code{prop_one_sample_score}: \code{events, n, p0, alternative, conf.level}.
\item \code{prop_two_independent_pearson_chisq}: \code{counts, alternative, conf.level}.
\item \code{rank_sum_mann_whitney}: \code{x, y, alternative, conf.level, exact, location_shift}.
\item \code{regression_linear_multiple}: \code{data, predictors, tested_predictors, alternative, conf.level, missing}.
\item \code{regression_linear_simple}: \code{data, predictor, alternative, conf.level, missing}.
\item \code{regression_logistic_binary}: \code{data, predictors, independent_units, alternative, conf.level, complete_cases}.
\item \code{regression_multinomial}: \code{data, predictors, independent_units, reference_category, alternative, conf.level, complete_cases}.
\item \code{regression_negative_binomial}: \code{data, predictors, independent_units, alternative, conf.level, complete_cases}.
\item \code{regression_ordinal_proportional_odds}: \code{data, predictors, independent_units, proportional_odds_justified, alternative, conf.level, complete_cases}.
\item \code{regression_poisson}: \code{data, predictors, independent_units, alternative, conf.level, complete_cases}.
\item \code{sign_test_one_sample}: \code{x, mu0, alternative, conf.level}.
\item \code{sign_test_paired}: \code{data, alternative, conf.level}.
\item \code{signed_rank_wilcoxon_one_sample}: \code{x, mu, alternative, conf.level, exact}.
\item \code{signed_rank_wilcoxon}: \code{data, alternative, conf.level, exact}.
\item \code{stuart_maxwell_marginal_homogeneity}: \code{counts, alternative, conf.level, n_excluded}.
\item \code{t_mean_one_sample}: \code{x, mu0, alternative, conf.level}.
\item \code{t_means_paired}: \code{data, alternative, conf.level}.
\item \code{t_means_two_independent_student}: \code{x, y, alternative, conf.level}.
\item \code{t_means_two_independent_welch}: \code{x, y, alternative, conf.level}.
\item \code{unconditional_exact_z_pooled}: \code{table, alternative, conf.level, independent_binomial_groups, tolerance, max_subdivisions}.
\item \code{variance_brown_forsythe}: \code{x, group, alternative, conf.level}.
\item \code{variance_f_test_two}: \code{x, y, ratio0, alternative, conf.level}.
\item \code{variance_fligner_killeen}: \code{x, group, alternative, conf.level}.
}
Exact native argument defaults and scope constraints are also recorded in
\file{methods/implemented.csv} and the package source. Source templates are
never evaluated at runtime.
}
\value{\code{run_inference()} returns a common \code{inference_result}, or a
\code{descriptive_result} or \code{classification_result} where appropriate.
No reporting package is required.}
\examples{
x <- c(2, 4, 5, 7, 9)
y <- c(1, 3, 4, 6, 8, 10)
spec <- method_spec("t_means_two_independent_welch")
result_data(run_inference(spec, x = x, y = y))
}
