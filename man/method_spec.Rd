\name{method_spec}
\alias{method_spec}
\alias{method_catalogue}
\alias{run_inference}
\title{Prespecify and execute an observed statistical method}
\description{Use an explicit method identity, sidedness, confidence level and method-specific options. Method selection is never based on observed pretests.}
\usage{
method_spec(method_id, alternative = "two.sided", conf.level = 0.95,
  missing = "error", options = list())
method_catalogue()
run_inference(spec, ..., result_id = NULL)
}
\arguments{
\item{method_id}{An identity from method_catalogue().}
\item{alternative}{Prespecified two.sided, less or greater alternative.}
\item{conf.level}{Confidence level strictly between zero and one.}
\item{missing}{Explicit error or omit policy, subject to each procedure's supported scope.}
\item{options}{Uniquely named list of supported procedure settings. Unknown settings are errors.}
\item{spec}{A method_spec.}
\item{result_id}{Optional explicit stable identifier for joining distinct results.}
\item{...}{Observations and design arguments for the selected method.}
}
\value{A specification, capability data frame, or finalized common result respectively.}
\examples{run_inference(method_spec("descriptive_summary"), data = data.frame(x = 1:5))}
