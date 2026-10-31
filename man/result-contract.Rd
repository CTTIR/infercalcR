\name{result-contract}
\alias{new_inference_result}
\alias{validate_inference}
\alias{result_data}
\alias{result_metadata}
\alias{as_inference}
\alias{adjust_inference}
\title{Finalized statistical result contract}
\description{Construct, validate and access plain-data statistical results. Numerical inference is finalized before presentation.}
\usage{
new_inference_result(data, metadata, kind = "inference", payload = list())
validate_inference(x)
result_data(x, ...)
result_metadata(x, ...)
as_inference(x, ...)
adjust_inference(x, method = "holm", family_id, rows = NULL)
}
\arguments{
\item{data}{A data frame of finalized numerical rows, with unique row IDs.}
\item{metadata}{A named list including method_id; optional variant_id, result_id, provenance, population, settings and validation evidence.}
\item{kind}{One of inference, planning, classification or descriptive.}
\item{payload}{Method-specific finalized quantities that do not fit scalar columns. No raw data are required.}
\item{x}{An existing result.}
\item{...}{Arguments to a documented adapter or accessor method.}
\item{method}{A method supported by \code{stats::p.adjust}.}
\item{family_id}{An explicit identifier of the complete declared hypothesis family.}
\item{rows}{Unique row IDs comprising the complete family; all rows by default.}
}
\details{Rows distinguish estimate, statistic, degrees of freedom, confidence limits and their level/method/scale/scope, raw p.value, p.adjusted and p.reported, family metadata, null/alternative, sample and exclusion counts, status and reason. NA means unavailable; legitimate unbounded confidence limits remain infinite. A numerical p-value bound is a separate method-specific quantity and is never a sampling confidence interval.

Adjustment rejects missing raw p-values and already adjusted families. It is performed once on the declared complete family before display filtering. It does not adjust confidence intervals. Results retain computation status separately from validation evidence. Result IDs are stable labels, not cryptographic hashes. RDS round trips are suitable only for trusted objects.}
\value{Constructors and adapters return an S3 inference_result; result_data returns a data.frame, result_metadata a list. Validation returns the object invisibly.}
\examples{
x <- new_inference_result(data.frame(row_id = c("a", "b"),
  p.value = c(.01, .04)), list(method_id = "example"))
result_data(adjust_inference(x, family_id = "primary"))
}
