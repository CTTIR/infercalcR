# Engine registry.
#
# A method can be computed by infercalcR's own base-R implementation ("base") or
# by the validated original implementation it is benchmarked against
# ("reference"). The original is the function DescrTab2 calls for that test, or
# the established package for tests DescrTab2 does not offer. Originals are
# optional (Suggests) and are never substituted for one another or for the base
# engine. Entries are explicit; nothing is discovered at run time.
#
# Each entry: package, functions (in that package), min_version, source (why
# this is the original) and adapter (same formals as the base function,
# returning the base function's native fields).
.ic_engine_info <- list(
  cochran_q = list(package = "DescTools", functions = "CochranQTest", min_version = "0.99.60",
    source = "DescrTab2 2.1.16 sig_test(): Cochran's Q test", adapter = .ref_cochran_q),
  cochran_armitage_trend = list(package = "DescTools", functions = "CochranArmitageTest", min_version = "0.99.60",
    source = "DescrTab2 2.1.16 sig_test(): Cochran-Armitage's test; one-sided p from the original's Z",
    adapter = .ref_cochran_armitage_trend),
  jonckheere_terpstra = list(package = "DescTools", functions = "JonckheereTerpstraTest", min_version = "0.99.60",
    source = "DescrTab2 2.1.16 sig_test(): Jonckheere-Terpstra's test (permutation route, nperm = B)",
    adapter = .ref_jonckheere_terpstra),
  mcnemar_exact_conditional = list(package = "exact2x2", functions = "exact2x2", min_version = "1.7.0",
    source = "DescrTab2 2.1.16 sig_test(): Exact McNemar's test (exact2x2, paired = TRUE, tsmethod = \"central\")",
    adapter = .ref_mcnemar_exact_conditional),
  mcnemar_midp = list(package = "exact2x2", functions = "exact2x2", min_version = "1.7.0",
    source = "established package: exact2x2 paired = TRUE, midp = TRUE; interval not compared (base reports Clopper-Pearson)",
    adapter = .ref_mcnemar_midp),
  boschloo_one_sided = list(package = "Exact", functions = "exact.test", min_version = "3.3",
    source = "Decision D12: Exact, not exact2x2::boschloo as in DescrTab2 (drops tied tables)",
    adapter = .ref_boschloo_one_sided),
  boschloo_two_sided_central = list(package = "Exact", functions = "exact.test", min_version = "3.3",
    source = "Decision D12: Exact; central p = min(1, 2 * min) of two one-sided Exact calls",
    adapter = .ref_boschloo_two_sided_central),
  boschloo_two_sided_minlike = list(package = "Exact", functions = "exact.test", min_version = "3.3",
    source = "Decision D12: Exact, tsmethod = \"square\"", adapter = .ref_boschloo_two_sided_minlike),
  unconditional_exact_z_pooled = list(package = "Exact", functions = "exact.test", min_version = "3.3",
    source = "established package: Exact, method = \"z-pooled\"", adapter = .ref_unconditional_exact_z_pooled),
  stuart_maxwell_marginal_homogeneity = list(package = "DescTools", functions = "StuartMaxwellTest",
    min_version = "0.99.60", source = "established package: DescTools", adapter = .ref_stuart_maxwell_marginal_homogeneity),
  variance_brown_forsythe = list(package = "DescTools", functions = "LeveneTest", min_version = "0.99.60",
    source = "established package: DescTools, center = median", adapter = .ref_variance_brown_forsythe),
  brunner_munzel = list(package = "brunnermunzel", functions = "brunnermunzel.test", min_version = "2.0",
    source = "established package: brunnermunzel (t approximation); interval compared for two-sided tests only",
    adapter = .ref_brunner_munzel),
  anova_repeated_one_way_gg = list(package = "stats", functions = "anova", min_version = "4.1.0",
    source = "R's stats::anova.mlm(test = \"Spherical\"); epsilon is not returned, so F and the GG p-value are compared",
    adapter = .ref_anova_repeated_one_way_gg))

# Methods whose test is computed by a stats function or fitted by stats or an
# optional model backend. That computation is the original, so they have one
# engine and either engine request runs it.
.ic_single_engine <- c(
  "ancova_baseline_adjusted", "anova_factorial", "anova_one_way", "anova_one_way_welch",
  "binomial_exact_one_sample", "bowker_symmetry", "chisq_goodness_of_fit",
  "chisq_goodness_of_fit_mc", "chisq_independence_rxc", "cluster_gower_pam",
  "cluster_hierarchical_ward", "cluster_kmeans", "correlation_kendall", "correlation_pearson",
  "correlation_spearman", "efa_common_factor_ml", "fisher_exact_2x2", "fisher_exact_rxc",
  "friedman", "gee_logistic_exchangeable", "glmm_logistic_random_intercept", "kruskal_wallis",
  "lmm_cluster_random_intercept", "lmm_repeated_measures", "mcnemar_asymptotic_cc",
  "mcnemar_asymptotic_uncorrected", "pca_components", "prediction_model_linear",
  "prediction_model_logistic", "prop_one_sample_score", "prop_two_independent_pearson_chisq",
  "rank_sum_mann_whitney", "regression_linear_multiple", "regression_linear_simple",
  "regression_logistic_binary", "regression_multinomial", "regression_negative_binomial",
  "regression_ordinal_proportional_odds", "regression_poisson", "sign_test_one_sample",
  "sign_test_paired", "signed_rank_wilcoxon", "signed_rank_wilcoxon_one_sample",
  "t_mean_one_sample", "t_means_paired", "t_means_two_independent_student",
  "t_means_two_independent_welch", "variance_f_test_two", "variance_fligner_killeen")

.ic_engines <- c("base", "reference")

.ic_package_version <- function(package) as.character(utils::packageVersion(package))

# Resolve the engine for one execution, or stop. Returns the record stored in
# result metadata: engine, package, version and call.
.ic_engine_check <- function(method_id, engine, registry = .ic_engine_info) {
  if (method_id %in% .ic_single_engine) {
    backends <- strsplit(.ic_method_info[[method_id]]$backend, ",", fixed = TRUE)[[1L]]
    backends <- setdiff(backends, c("", "base", "utils", "stats"))
    package <- if (length(backends)) backends[[1L]] else "stats"
    return(list(engine = "single", package = package, version = .ic_package_version(package),
      call = NA_character_))
  }
  if (identical(engine, "base"))
    return(list(engine = "base", package = "infercalcR", version = .ic_package_version("infercalcR"),
      call = paste0("infercalcR:::.obs_", method_id, "_analyse")))
  entry <- registry[[method_id]]
  if (is.null(entry))
    .ic_abort(paste0("No reference engine is registered for ", method_id, "; use engine = \"base\""), "unsupported")
  required <- paste0(entry$package, " (>= ", entry$min_version, ")")
  if (!requireNamespace(entry$package, quietly = TRUE))
    .ic_abort(paste0("engine = \"reference\" for ", method_id, " requires the optional package ", required,
      ", which is not installed"), "missing_backend")
  version <- utils::packageVersion(entry$package)
  if (version < entry$min_version)
    .ic_abort(paste0("engine = \"reference\" for ", method_id, " requires ", required, "; version ",
      version, " is installed"), "missing_backend")
  absent <- entry$functions[!vapply(entry$functions, exists, NA, envir = asNamespace(entry$package),
    inherits = FALSE)]
  if (length(absent))
    .ic_abort(paste0("engine = \"reference\" for ", method_id, " requires ", entry$package, "::",
      absent[[1L]], ", which version ", version, " does not provide"), "missing_backend")
  list(engine = "reference", package = entry$package, version = as.character(version),
    call = paste0(entry$package, "::", entry$functions, collapse = ", "))
}

.ic_catalogue_engines <- function(method_id, implemented) {
  ifelse(!implemented, "none", ifelse(method_id %in% names(.ic_engine_info), "base+reference",
    ifelse(method_id %in% .ic_single_engine, "single", "base")))
}

.ic_catalogue_reference <- function(method_id) {
  vapply(method_id, function(id) {
    entry <- .ic_engine_info[[id]]
    if (is.null(entry)) NA_character_ else paste0(entry$package, "::", entry$functions, collapse = ", ")
  }, "", USE.NAMES = FALSE)
}
