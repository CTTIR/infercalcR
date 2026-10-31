\name{as_inference.htest}
\alias{as_inference.htest}
\alias{as_inference.lm}
\alias{as_inference.glm}
\title{Explicit conversion of supplied tests and fitted models}
\description{Convert recognized finalized test objects or summarize an existing fitted model once, without refitting.}
\usage{
\method{as_inference}{htest}(x, method_id = NULL, n = NA_real_, population,
  groups = NULL, event = NULL, n_excluded = NA_real_, ...)
\method{as_inference}{lm}(x, conf.level = .95, population, ...)
\method{as_inference}{glm}(x, conf.level = .95, population,
  interval = "wald", ...)
}
\arguments{
\item{x}{A recognized \code{htest}, \code{lm} or \code{glm} object.}
\item{method_id}{Optional identity, which must agree with the stored test.}
\item{n}{Known analysis sample counts, or explicit NA when unavailable. Never reconstructed from degrees of freedom.}
\item{population}{Required description of the supplied analysis population.}
\item{groups}{Two ordered labels for independent two-sample location, odds-ratio or variance-ratio contrasts.}
\item{event}{Explicit event definition for binary/proportion and odds-ratio tests.}
\item{n_excluded}{Known exclusion count, otherwise NA.}
\item{conf.level}{Requested confidence level for summaries of supplied models.}
\item{interval}{Fitted-glm conversion explicitly supports \code{"wald"}. The named logistic procedure provides profile-likelihood slope intervals separately.}
\item{...}{Reserved; unknown arguments are rejected.}
}
\details{Recognized htest objects include standard t, Wilcoxon, binomial,
proportion, Fisher, correlation, variance and common omnibus tests. Stored
estimates, intervals and p-values are preserved with method-specific estimands.
Two group means become their difference; no odds ratio is invented for a general
contingency table. Proportion tests across more than two groups retain an omnibus
row without a fabricated scalar probability. Unknown htest methods are rejected.

Linear models use classical model-based marginal t intervals. GLM conversion is
restricted to binomial-logit, Poisson-log and Gaussian-identity models; it uses
model-based covariance and explicitly labelled Wald or t intervals. It does not
invoke generic profile dispatch or switch covariance backends. Binomial slopes
may also be shown as odds ratios and Poisson slopes as rate ratios; intercepts
are not ratios. Exponentiation overflow is a numerical failure, distinct from a
legitimate unbounded sampling interval. Aliased coefficients remain unavailable.}
\value{A common finalized \code{inference_result}. The supplied source object is unchanged.}
\examples{
fit <- stats::t.test(c(1, 3, 4, 8), c(2, 4, 5, 6, 9))
as_inference(fit, n = c(4, 5), population = "Synthetic independent groups",
  groups = c("x", "y"))
}
