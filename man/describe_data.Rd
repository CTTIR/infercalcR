\name{describe_data}
\alias{describe_data}
\title{Descriptive summaries without hypothesis tests}
\description{Compute continuous or categorical summaries with explicit denominators and missing counts using standard R.}
\usage{describe_data(data, variables = setdiff(names(data), group), group = NULL,
  labels = NULL, units = NULL, types = NULL)}
\arguments{
\item{data}{A data frame.}
\item{variables}{Distinct column names to summarize.}
\item{group}{Optional single grouping column name. Missing groups are retained explicitly.}
\item{labels, units}{Optional named display metadata keyed by variable name.}
\item{types}{Optional named continuous/categorical overrides. Ordered factors are categorical by default.}
}
\details{Numeric summaries are mean, sample standard deviation, median, quartiles (type 7), minimum and maximum. Categorical proportions use nonmissing observations within each group as their denominator; n.missing and n.total remain separate. Empty populations produce explicit unavailable summaries. Infinite measurements are rejected. No hypothesis tests or automatic p-value columns are calculated.}
\value{A descriptive_result inheriting from inference_result.}
\examples{describe_data(data.frame(value = c(1, 3, NA), category = factor(c("a", "b", "a"))))}
