\name{kernel-providers}
\alias{boschloo_design_kernel}
\alias{binomial_region_probability}
\alias{binomial_table_space}
\alias{boschloo_region_record}
\alias{boschloo_kernel_status}
\alias{boschloo_control_record}
\alias{boschloo_control_from_record}
\alias{boschloo_validate_control}
\alias{boschloo_validate_design}
\alias{boschloo_variant_details}
\alias{boschloo_variant_record}
\alias{boschloo_engine_info}
\alias{boschloo_control_call}
\alias{fisher_ordering_probabilities}
\alias{boschloo_plot_data_record}
\alias{classification_dictionary}
\alias{classification_contract}
\alias{classification_reason}
\alias{classification_metric_ids}
\alias{classification_short_label}
\alias{classification_check_thresholds}
\alias{classification_check_bins}
\alias{classification_check_partial}
\alias{legacy_canonical_json}
\alias{legacy_hash_value}
\alias{legacy_sha256}
\alias{legacy_parse_json}
\alias{legacy_format_double}
\alias{legacy_parse_decimal}
\alias{legacy_double_values}
\alias{legacy_nonfinite_paths}
\alias{legacy_r_literal}
\alias{legacy_unclass_frozen}
\alias{legacy_check_spec}
\alias{legacy_json_limits}
\alias{normal_probability}
\alias{normal_quantile}
\alias{t_probability}
\alias{t_quantile}
\alias{chisq_probability}
\alias{chisq_quantile}
\alias{f_probability}
\alias{f_quantile}
\alias{binomial_probability}
\alias{log_probability_add}
\alias{log_probability_sum}
\alias{invert_log_cdf}
\alias{f_lower_log_bound}
\alias{noncentral_t_density}
\title{Explicit Shared Numerical and Compatibility Providers}
\description{Preserved numerical procedures extracted from inferguidR, with explicit inputs, typed statuses and method settings. Calculation and display are separate.}
\usage{
boschloo_design_kernel(n1, n2, alpha = 0.05, variant, control = boschloo_control())

binomial_region_probability(indices, n1, n2, p1, p2)

binomial_table_space(n1, n2)

boschloo_region_record(kernel, control = boschloo_control())

boschloo_kernel_status(kernel)

boschloo_control_record(control)

boschloo_control_from_record(record)

boschloo_validate_control(control)

boschloo_validate_design(n1, n2)

boschloo_variant_details(variant = NULL)

boschloo_variant_record(info)

boschloo_engine_info()

boschloo_control_call(control)

fisher_ordering_probabilities(n1, n2, variant, control = boschloo_control(),
  convention = "same")

boschloo_plot_data_record(plot_id, title, data, fixed, scope, reference = list(),
  notes = character(), status = ig_status("ok"))

classification_dictionary()

classification_contract()

classification_reason(code, lang = "en")

classification_metric_ids()

classification_short_label(id, lang = "en")

classification_check_thresholds(threshold_probability)

classification_check_bins(calibration_bins)

classification_check_partial(partial_auc)

legacy_canonical_json(x)

legacy_hash_value(x)

legacy_sha256(text)

legacy_parse_json(text)

legacy_format_double(x)

legacy_parse_decimal(text)

legacy_double_values(x)

legacy_nonfinite_paths(x, path = "")

legacy_r_literal(x)

legacy_unclass_frozen(x)

legacy_check_spec(spec, kind)

legacy_json_limits()

normal_probability(q, mean = 0, sd = 1, lower_tail = TRUE, log_p = FALSE)

normal_quantile(p, mean = 0, sd = 1, lower_tail = TRUE, log_p = FALSE)

t_probability(q, df, ncp = 0, lower_tail = TRUE, log_p = FALSE)

t_quantile(p, df, ncp = 0, lower_tail = TRUE, log_p = FALSE)

chisq_probability(q, df, ncp = 0, lower_tail = TRUE, log_p = FALSE)

chisq_quantile(p, df, ncp = 0, lower_tail = TRUE, log_p = FALSE)

f_probability(q, df1, df2, ncp = 0, lower_tail = TRUE, log_p = FALSE)

f_quantile(p, df1, df2, ncp = 0, lower_tail = TRUE, log_p = FALSE)

binomial_probability(q, size, prob, lower_tail = TRUE, log_p = FALSE)

log_probability_add(a, b)

log_probability_sum(x)

invert_log_cdf(logcdf, p, lower_tail, log_p, support, start)

f_lower_log_bound(q, df1, df2, ncp)

noncentral_t_density(x, df, ncp)
}
\arguments{
\item{n1}{Number of independent binomial observations in group 1.}
\item{n2}{Number of independent binomial observations in group 2.}
\item{alpha}{Decision level strictly between zero and one; NULL omits a decision where supported.}
\item{variant}{Explicit Boschloo variant: minlike, central, less, or greater. Classifier variants are retained in their records.}
\item{control}{Numerical settings from boschloo_control().}
\item{indices}{Distinct indices of rejected tables in x1-fastest two-binomial table order.}
\item{p1}{Event probability in group 1.}
\item{p2}{Event probability in group 2.}
\item{kernel}{Unmodified design kernel returned by boschloo_design_kernel().}
\item{record}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{info}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{convention}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{plot_id}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{title}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{data}{A data frame containing observations, with explicit truth and score columns.}
\item{fixed}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{scope}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{reference}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{notes}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{status}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{code}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{lang}{Display-label language, en or de.}
\item{id}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{threshold_probability}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{calibration_bins}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{partial_auc}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{x}{Input data, finalized result, or numeric values as specified by the selected function.}
\item{text}{A data-only canonical JSON document or decimal string, according to the function.}
\item{path}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{spec}{A classification specification returned by classification_spec().}
\item{kind}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{q}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{mean}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{sd}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{lower_tail}{Logical: use the lower tail when TRUE.}
\item{log_p}{Logical: return or interpret probabilities on the logarithmic scale.}
\item{p}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{df}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{ncp}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{df1}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{df2}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{size}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{prob}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{a}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{b}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{logcdf}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{support}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
\item{start}{Procedure-specific input, setting, or metadata field. Its accepted values are validated by the function; retained settings are included in the result.}
}
\details{Boschloo tests use Fisher ordering and bounded nuisance maximization. The central variant doubles the smaller separately maximized one-sided value. Rejection uses the numerical upper bound; this bound is not a sampling confidence interval. The shared Boschloo inference path requires only standard R. Historical hashed specifications and the legacy calculator decimal parser require the optional packages digest and/or jsonlite and give an explicit missing-dependency error when absent.

Classifier specifications preserve the positive class, direction, cutoff origin, missing-value exclusions, evaluation split and raw-data fingerprint. Raw observations are not retained in the common reporting payload. Basic count metrics, binormal rates and static dictionary access do not require contributed packages. Undefined metrics retain statuses and denominator reasons.

Provider functions are public interfaces for validated package consumers. Historical canonical primitives accept already converted plain data; guidance-owned schema dispatch remains in inferguidR. No provider evaluates code from input strings. Calculator expressions use a restricted parser.

The inherited validation metadata records prior scoped checks; package extraction and adapter parity do not certify all scientific designs.}
\value{Computational functions return typed lists or ordinary data frames; common adapters return inference_result objects. Metadata providers return plain lists or vectors, probability kernels return numeric values, and validators signal typed errors for invalid inputs.}
\examples{
k <- boschloo_design_kernel(3, 4, variant = "minlike")
binomial_region_probability(k$idx, 3, 4, .2, .8)
t_probability(2, df = 10)
}
