% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/contrast_test.r
\name{contrast_test}
\alias{contrast_test}
\title{Perform Contrast Test}
\usage{
contrast_test(
  dat,
  weight = NULL,
  paired = FALSE,
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95,
  verbose = TRUE
)
}
\arguments{
\item{dat}{A data frame or tibble containing the data to be analyzed. Each column represents a variable. For unbalanced data, please fill missing values with NA.}

\item{weight}{A numeric vector of weights to apply to each variable in the contrast. The sum of the weights must be zero.}

\item{paired}{A logical indicating whether the data is paired (TRUE) or unpaired (FALSE). Default is FALSE.}

\item{alternative}{A character string specifying the type of test. Possible values are "two.sided" (default), "less", or "greater".}

\item{conf.level}{A numeric value indicating the confidence level for the confidence intervals. Default is 0.95.}

\item{verbose}{Logical. If \code{TRUE}, prints additional messages.}
}
\value{
A data frame containing the following columns:
\describe{
\item{weights}{The weights applied to each variable in the contrast.}
\item{estimate}{The weighted contrast estimate.}
\item{lower}{The lower bound of the confidence interval.}
\item{upper}{The upper bound of the confidence interval.}
\item{t}{The t-value for the contrast.}
\item{F}{The F-value for the contrast.}
\item{df_error}{The degrees of freedom for the error.}
\item{SS_contrast}{The sum of squares for the contrast component.}
\item{SS_error}{The sum of squares for the error.}
\item{SS_effect}{The sum of squares for the effect.}
\item{SS_total}{The toal sum of squares for the data.}
\item{p}{The p-value for the contrast test.}
\item{eta2}{The eta squared effect size.}
\item{peta2}{The partial eta squared effect size.}
\item{cohens_f}{The Cohen's f effect size.}
\item{n_total}{The total number of observations in the unpaired data.}
\item{n_total_na}{The number of missing values in the unpaired data.}
\item{n_pair}{The number of non-missing pairs in the paired data.}
\item{n_pair_na}{The number of missing pairs in the paired data.}
}
}
\description{
This function performs a contrast test on a given dataset to compare the means of multiple variables using weighted contrasts.
It calculates several statistics, including the t-value, F-value, p-value, confidence intervals, and effect sizes (Cohen's f and partial eta squared).
}
\details{
This function performs a contrast analysis on a given dataset using the provided weights. It computes statistics such as the t-value, F-value, confidence intervals, and effect sizes (Cohen's f and partial eta squared).
If the data is paired, pair-wise exclusion is applied. The function supports two-tailed and one-tailed tests depending on the value of the \code{alternative} parameter.
}
\examples{
# Example of linear trend analysis with unpaired data
dat <- data.frame(A = rnorm(100), B = rnorm(100), C = rnorm(100))
weight <- c(1, -1, 0)
contrast_test(dat, weight)

# Example with paired data
dat <- data.frame(A = rnorm(50), B = rnorm(50), C = rnorm(50))
weight <- c(1, -2, 1)
contrast_test(dat, weight, paired = TRUE)

}
\seealso{
\code{\link[=trend_test]{trend_test()}} for trend analysis using contrast tests.
}
