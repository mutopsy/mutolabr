% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/t_test_all.r
\name{t_test_all}
\alias{t_test_all}
\title{Perform Frequentist and Bayesian t-tests with Effect Size Estimation}
\usage{
t_test_all(
  x,
  y = NULL,
  var.label = c("x", "y"),
  paired = F,
  var.equal = FALSE,
  mu = 0,
  ci = c("freq", "bayes_central", "bayes_hdi"),
  alternative = c("two.sided", "less", "greater"),
  conf.level = 0.95,
  pd = FALSE,
  bf = FALSE,
  cor = TRUE,
  mean_x_EAP = FALSE,
  mean_x_MAP = FALSE,
  mean_x_MED = FALSE,
  diff_EAP = FALSE,
  diff_MAP = FALSE,
  diff_MED = FALSE,
  cohens_d = NULL,
  cohens_d_EAP = FALSE,
  cohens_d_MAP = FALSE,
  cohens_d_MED = FALSE,
  cohens_dz = TRUE,
  cohens_dz_EAP = FALSE,
  cohens_dz_MAP = FALSE,
  cohens_dz_MED = FALSE,
  rscale_est = Inf,
  rscale_bf = "medium",
  iterations = 10000,
  map_density_n = 512,
  verbose = TRUE
)
}
\arguments{
\item{x}{A numeric vector representing the first sample.}

\item{y}{An optional numeric vector representing the second sample. If \code{NULL}, a one-sample t-test is performed (default: \code{NULL}).}

\item{var.label}{A character vector of length two specifying labels for \code{x} and \code{y}.}

\item{paired}{Logical. If \code{TRUE}, a paired t-test is performed (default: \code{FALSE}).}

\item{var.equal}{Logical. If \code{TRUE}, the two-sample t-test assumes equal variances (default: \code{FALSE}).}

\item{mu}{A numeric value specifying the null hypothesis mean difference.}

\item{ci}{Character. Specifies the type of confidence or credible interval: \code{"freq"} (frequentist confidence interval),
\code{"bayes_central"} (Bayesian central credible interval), or \code{"bayes_hdi"} (highest density interval based on the posterior distribution) (default: \code{"freq"}).}

\item{alternative}{Character. Specifies the alternative hypothesis for the frequentist test: \code{"two.sided"}, \code{"less"}, or \code{"greater"} (default: \code{"two.sided"}).}

\item{conf.level}{Numeric. The confidence level for frequentist intervals or credibility level for Bayesian intervals (default: \code{0.95}).}

\item{pd}{Logical. If \code{TRUE}, computes the probability of direction (pd) based on posterior distributions (default: \code{FALSE}).}

\item{bf}{Logical. If \code{TRUE}, computes Bayes factors for the presence of a difference versus the null hypothesis (default: \code{FALSE}).}

\item{cor}{Logical. If \code{TRUE}, computes Pearson correlation for paired samples.}

\item{mean_x_EAP}{Logical. If \code{TRUE}, computes the expected a posteriori (EAP) estimate of the mean of \code{x} (default: \code{FALSE}).}

\item{mean_x_MAP}{Logical. If \code{TRUE}, computes the maximum a posteriori (MAP) estimate of the mean of \code{x} (default: \code{FALSE}).}

\item{mean_x_MED}{Logical. If \code{TRUE}, computes the median of the posterior distribution (MED) for the mean of \code{x} (default: \code{FALSE}).}

\item{diff_EAP}{Logical. If \code{TRUE}, computes the expected a posteriori (EAP) estimate of the mean difference (default: \code{FALSE}).}

\item{diff_MAP}{Logical. If \code{TRUE}, computes the maximum a posteriori (MAP) estimate of the mean difference (default: \code{FALSE}).}

\item{diff_MED}{Logical. If \code{TRUE}, computes the median of the posterior distribution (MED) for the mean difference (default: \code{FALSE}).}

\item{cohens_d}{Logical. If \code{TRUE}, computes Cohen's d for independent samples.}

\item{cohens_d_EAP}{Logical. If \code{TRUE}, computes the expected a posteriori (EAP) estimate of Cohen's d (default: \code{FALSE}).}

\item{cohens_d_MAP}{Logical. If \code{TRUE}, computes the maximum a posteriori (MAP) estimate of Cohen's d (default: \code{FALSE}).}

\item{cohens_d_MED}{Logical. If \code{TRUE}, computes the median of the posterior distribution (MED) for Cohen's d (default: \code{FALSE}).}

\item{cohens_dz}{Logical. If \code{TRUE}, computes Cohen's dz for paired samples.}

\item{cohens_dz_EAP}{Logical. If \code{TRUE}, computes the expected a posteriori (EAP) estimate of Cohen's dz (default: \code{FALSE}).}

\item{cohens_dz_MAP}{Logical. If \code{TRUE}, computes the maximum a posteriori (MAP) estimate of Cohen's dz (default: \code{FALSE}).}

\item{cohens_dz_MED}{Logical. If \code{TRUE}, computes the median of the posterior distribution (MED) for Cohen's dz (default: \code{FALSE}).}

\item{rscale_est}{Numeric or character. Specifies the Cauchy prior scale for Bayesian estimation of the posterior distribution.
Options: \code{"ultrawide"}, \code{"wide"}, \code{"medium"}, or a positive real number (default: \code{Inf}). Passed to \code{BayesFactor::ttestBF()}.}

\item{rscale_bf}{Numeric or character. Specifies the Cauchy prior scale for Bayes factor calculation.
Options: \code{"ultrawide"}, \code{"wide"}, \code{"medium"}, or a positive real number (default: \code{"medium"}). Passed to \code{BayesFactor::ttestBF()}.}

\item{iterations}{Integer. Number of MCMC iterations for Bayesian estimation.}

\item{map_density_n}{Integer. Number of bins for MAP density estimation.}

\item{verbose}{Logical. If \code{TRUE}, prints additional messages.}
}
\value{
A data frame containing test statistics, effect sizes, confidence intervals, and Bayesian estimates.
}
\description{
This function performs frequentist and Bayesian t-tests, with options for effect size estimation,
confidence intervals, Bayesian credible intervals, probability of direction (pd), and Bayes factors.
}
\examples{
set.seed(123)
x <- rnorm(30,0, 1)
y <- x + rnorm(30,0.5, 1)

# Welch's t-test with effect size estimation
t_test_all(x, y)

# Welch's t-test with Bayesian independent t-test (Bayes factor and pd)
# and 95\% highest density intervals
t_test_all(x, y, diff_MAP = TRUE, cohens_d_MAP = TRUE,
           bf = TRUE, pd = TRUE, ci = "bayes_hdi", rscale_bf = "medium")

# Paired t-test with effect size estimation
t_test_all(x, y, paired = TRUE)

# Paired t-test with Bayesian paired t-test (Bayes factor and pd)
# and 95\% central credible intervals
t_test_all(x, y, paired = TRUE, diff_MAP = TRUE, cohens_dz_MAP = TRUE,
           bf = TRUE, pd = TRUE, ci = "bayes_central", rscale_bf = "medium")

}
