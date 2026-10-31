\name{parallel_analysis}
\alias{parallel_analysis}
\alias{predict_inference}
\title{Common-factor exploration and prediction from stored development results}
\description{Two explicit computational helpers retained from existing procedures.}
\usage{
parallel_analysis(data, variables, independent_units = FALSE,
  continuous_measurements = FALSE, B = 199L, seed = 104729L,
  quantile_probability = .95, complete_cases = FALSE, permutations = NULL)
predict_inference(result, newdata, interval = "none", conf.level = NULL)
}
\arguments{
\item{data}{Data frame with unique \code{id} and numeric measurement columns.}
\item{variables}{Explicit measurement column names.}
\item{independent_units, continuous_measurements}{Required declarations for the permutation design.}
\item{B}{Prespecified number of replicates, subject to the method's computational limits.}
\item{seed}{Fixed reproducible seed; caller RNG state is restored.}
\item{quantile_probability}{Prespecified null-spectrum quantile, using quantile type 6.}
\item{complete_cases}{Explicit complete-case choice; missingness assumptions remain the analyst's responsibility.}
\item{permutations}{Optional validated column permutation indices.}
\item{result}{A finalized \code{prediction_model_linear} or \code{prediction_model_logistic} result.}
\item{newdata}{New numeric predictor values matching the fitted model.}
\item{interval}{\code{"none"}; for linear predictions \code{"mean"} or \code{"prediction"}; for logistic predictions \code{"risk"}.}
\item{conf.level}{Confidence level. NULL retains the development result's requested level.}
}
\details{Parallel analysis replaces the correlation diagonal by squared multiple
correlations and uses independent column permutations. It supplies exploratory
leading-factor guidance without fitting or selecting an ML factor model. Failed
replicates are retained and prevent a selected count.

Linear \code{"mean"} intervals are pointwise normal-model t confidence intervals
for the conditional mean. Linear \code{"prediction"} intervals instead cover one
independent new outcome and include new-outcome residual variation. Common-row
estimands and interval-method labels distinguish these targets; metadata also
records the interval target, type and native interpretation. Point predictions
without requested intervals have no interval scope. Logistic risk intervals
use already fitted bootstrap models and type-6 percentile limits, requiring at
least five expected replicates per tail. They concern risk estimation, not a
binary outcome, validation performance, or Monte Carlo precision. No model is
refitted by \code{predict_inference()}.}
\value{A common result with finalized rows, metadata and full native helper output in \code{payload$native}.}
