% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trend_test.r
\name{trend_test}
\alias{trend_test}
\title{Perform Trend Analysis Using Contrast Test}
\usage{
trend_test(
  dat,
  order = 1:3,
  paired = FALSE,
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95,
  verbose = TRUE
)
}
\arguments{
\item{dat}{A data frame or matrix containing the data. Columns represent different levels of the independent variable.}

\item{order}{An integer vector specifying the trend orders to test. Defaults to \code{1:3}.}

\item{paired}{Logical. If \code{TRUE}, a paired contrast test is performed. Defaults to \code{FALSE}.}

\item{alternative}{A character string specifying the alternative hypothesis. One of \code{"two.sided"}, \code{"less"}, or \code{"greater"}. Defaults to \code{"two.sided"}.}

\item{conf.level}{Confidence level for the test. Defaults to \code{0.95}.}

\item{verbose}{Logical. If \code{TRUE}, displays messages. Defaults to \code{TRUE}.}
}
\value{
A data frame containing the results of the contrast tests, with an additional column indicating the trend order.
}
\description{
This function conducts a trend analysis by applying contrast tests to a given dataset.
}
\details{
The function constructs contrast vectors for specified trend orders using polynomial contrasts (\code{contr.poly}).
Then, \code{contrast_test()} is applied to test each contrast.
For more details on the testing procedure, refer to the documentation of \code{\link[=contrast_test]{contrast_test()}}.
}
\examples{
# Example dataset with 5 conditions
dat <- matrix(rnorm(50), nrow = 10, ncol = 5)

# Perform trend analysis for 1st and 2nd order trends
trend_test(dat, order = 1:2)

}
\seealso{
\code{\link[=contrast_test]{contrast_test()}} for details on the contrast testing procedure.
}
