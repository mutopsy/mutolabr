% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/anovakun_tidy.r
\name{anovakun_tidy}
\alias{anovakun_tidy}
\title{A wrapper for anovakun (version 4.8.9) with improved usability and formatting}
\usage{
anovakun_tidy(
  dataset,
  design,
  alpha = 0.05,
  inc_allvar = FALSE,
  do_round = TRUE,
  ...
)
}
\arguments{
\item{dataset}{A data frame containing the input data in long (tidy) format.}

\item{design}{A character string specifying the experimental design (e.g., "As", "ABs", "sA", "sAB", "AsB", etc.).}

\item{alpha}{Significance level. Defaults to 0.05.}

\item{inc_allvar}{Logical. If \code{TRUE}, all columns will be included in the output. Defaults to \code{FALSE}.}

\item{do_round}{Logical. If \code{TRUE}, numeric values in the output will be rounded for readability. Defaults to \code{TRUE}.}

\item{...}{Additional arguments passed to \code{\link{anovakun_}}. The number of levels for each factor
is automatically calculated from \code{dataset} and \code{design}, and does not need to be specified manually.}
}
\value{
A data frame containing the results of ANOVA, including F-statistics, p-values, effect sizes,
and sphericity indices (if applicable).
}
\description{
This function serves as a wrapper around \code{\link{anovakun_}} (version 4.8.9),
streamlining common usage patterns in mutolab and formatting the results for easier interpretation.
}
\details{
While the core computation is delegated to the original \code{anovakun} function, this wrapper
automatically infers the number of factor levels, simplifies output, and adds formatting options such as rounding.

For full details of the original implementation of anovakun, refer to:
\url{https://riseki.cloudfree.jp/?ANOVA\%E5\%90\%9B}
}
\examples{
data_snakemr \%>\%
  anovakun_tidy("sABC")

}
\seealso{
\code{\link{anovakun_}} for the original function wrapped by this helper.
}
