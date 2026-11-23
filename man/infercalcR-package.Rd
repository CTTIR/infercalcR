\name{infercalcR-package}
\alias{infercalcR-package}
\alias{infercalcR}
\docType{package}
\title{infercalcR: Statistical Inference and Diagnostic Calculations}
\description{Explicit statistical procedures, descriptive summaries, diagnostic
calculations and a common finalized result contract. A procedure is chosen
through an explicit method specification, never by a data-driven preliminary
test, and returns finalized estimates, intervals and p-values together with
metadata that records the method identity and numerical provenance.}
\details{The main entry points are:
\itemize{
\item \code{\link{method_spec}()}, \code{\link{run_inference}()} and
  \code{\link{method_catalogue}()} to prespecify and run a procedure; the
  procedures and their arguments are listed in \code{\link{observed-methods}}.
\item \code{\link{describe_data}()} for descriptive summaries without tests.
\item \code{\link{result_data}()}, \code{\link{result_metadata}()},
  \code{\link{as_inference}()} and \code{\link{adjust_inference}()} to read,
  convert and adjust results; see \code{\link{result-contract}}.
\item \code{\link{boschloo_test}()}, \code{\link{classification_spec}()},
  \code{\link{evaluate_cutoff}()} and \code{\link{distribution_calculator}()}
  for Boschloo's exact test, diagnostic accuracy and classifier metrics, and
  the distribution calculator; see \code{\link{legacy-computation}}.
\item \code{\link{parallel_analysis}()} and \code{\link{predict_inference}()}
  as exploration and prediction helpers.
}
The package depends only on the stats and utils packages. Optional backends
and reference engines are used only when installed and explicitly requested.}
\seealso{The vignettes \code{vignette("getting-started", package = "infercalcR")},
\code{vignette("classification", package = "infercalcR")} and
\code{vignette("exact-tests", package = "infercalcR")}, and the documentation
site \url{https://cttir.github.io/infercalcR/}.}
\author{Maintainer: R. Heller \email{raban.heller@uni-ulm.de}
(\href{https://orcid.org/0000-0001-8006-9742}{ORCID})}
\keyword{package}
