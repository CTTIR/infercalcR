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

Thirteen methods have two engines. \code{engine = "base"} (the default) uses
infercalcR's own implementation, written from the method's publication;
\code{engine = "reference"} calls the validated original: the function
DescrTab2 uses for the test, Exact for Boschloo's test, or the established
package for tests DescrTab2 does not offer. \code{method_catalogue()} lists
each method's engines and original. The engines are benchmarked against each
other on the originals' help-page examples (\file{methods/engine-benchmarks.rds},
approved exceptions in \file{methods/engine-deviations.csv}).
\file{methods/provenance.csv} records for every method its validated source,
its original, the publication its base implementation follows and the
outcome of the check that no base implementation was adapted from the
code of a GPL-licensed original.
}
\value{\code{run_inference()} returns a common \code{inference_result}, or a
\code{descriptive_result} or \code{classification_result} where appropriate.
No reporting package is required.}
\references{
Armitage P (1955). Tests for linear trends in proportions and frequencies. \emph{Biometrics} 11(3):375--386. \doi{10.2307/3001775}

Boschloo RD (1970). Raised conditional level of significance for the 2 x 2-table when testing the equality of two probabilities. \emph{Statistica Neerlandica} 24(1):1--9. \doi{10.1111/j.1467-9574.1970.tb00104.x}

Box GEP (1954). Some theorems on quadratic forms applied in the study of analysis of variance problems, I. \emph{Annals of Mathematical Statistics} 25(2):290--302. \doi{10.1214/aoms/1177728786}

Brown MB, Forsythe AB (1974). Robust tests for the equality of variances. \emph{Journal of the American Statistical Association} 69(346):364--367. \doi{10.1080/01621459.1974.10482955}

Brunner E, Munzel U (2000). The nonparametric Behrens-Fisher problem: asymptotic theory and a small-sample approximation. \emph{Biometrical Journal} 42(1):17--25. \doi{10.1002/(SICI)1521-4036(200001)42:1<17::AID-BIMJ17>3.0.CO;2-U}

Clopper CJ, Pearson ES (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. \emph{Biometrika} 26(4):404--413. \doi{10.1093/biomet/26.4.404}

Cochran WG (1950). The comparison of percentages in matched samples. \emph{Biometrika} 37(3-4):256--266. \doi{10.1093/biomet/37.3-4.256}

Cochran WG (1954). Some methods for strengthening the common chi-squared tests. \emph{Biometrics} 10(4):417--451. \doi{10.2307/3001616}

Fagerland MW, Lydersen S, Laake P (2013). The McNemar test for binary matched-pairs data: mid-p and asymptotic are better than exact conditional. \emph{BMC Medical Research Methodology} 13:91. \doi{10.1186/1471-2288-13-91}

Greenhouse SW, Geisser S (1959). On methods in the analysis of profile data. \emph{Psychometrika} 24(2):95--112. \doi{10.1007/BF02289823}

Jonckheere AR (1954). A distribution-free k-sample test against ordered alternatives. \emph{Biometrika} 41(1-2):133--145. \doi{10.1093/biomet/41.1-2.133}

Lancaster HO (1961). Significance tests in discrete distributions. \emph{Journal of the American Statistical Association} 56(294):223--234. \doi{10.1080/01621459.1961.10482105}

Maxwell AE (1970). Comparing the classification of subjects by two independent judges. \emph{British Journal of Psychiatry} 116(535):651--655. \doi{10.1192/bjp.116.535.651}

McNemar Q (1947). Note on the sampling error of the difference between correlated proportions or percentages. \emph{Psychometrika} 12(2):153--157. \doi{10.1007/BF02295996}

Phipson B, Smyth GK (2010). Permutation p-values should never be zero: calculating exact p-values when permutations are randomly drawn. \emph{Statistical Applications in Genetics and Molecular Biology} 9(1):39. \doi{10.2202/1544-6115.1585}

Stuart A (1955). A test for homogeneity of the marginal distributions in a two-way classification. \emph{Biometrika} 42(3-4):412--416. \doi{10.1093/biomet/42.3-4.412}

Suissa S, Shuster JJ (1985). Exact unconditional sample sizes for the 2 x 2 binomial trial. \emph{Journal of the Royal Statistical Society, Series A} 148(4):317--327. \doi{10.2307/2981892}

Terpstra TJ (1952). The asymptotic normality and consistency of Kendall's test against trend, when ties are present in one ranking. \emph{Indagationes Mathematicae (Proceedings)} 55:327--333. \doi{10.1016/S1385-7258(52)50043-X}
}
\examples{
x <- c(2, 4, 5, 7, 9)
y <- c(1, 3, 4, 6, 8, 10)
spec <- method_spec("t_means_two_independent_welch")
result_data(run_inference(spec, x = x, y = y))
}
