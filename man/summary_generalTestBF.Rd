% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_generalTestBF.r
\name{summary_generalTestBF}
\alias{summary_generalTestBF}
\title{Summarize Results from generalTestBF}
\usage{
summary_generalTestBF(x, inc_ranef = FALSE)
}
\arguments{
\item{x}{An object returned by \code{generalTestBF()}, or a named list containing:
\describe{
\item{res_BF}{An object returned by \code{generalTestBF()}}
\item{variables}{Character vector of variable names}
\item{full_model_vars}{Character vector of variables in the full model}
}}

\item{inc_ranef}{Logical. Whether to output results of random effects.}
}
\value{
A tibble with one row per model comparison. Columns include:
\describe{
\item{effect}{The effect excluded from the full model}
\item{type}{Indicates whether the term is Fixed or random effect}
\item{BF}{Bayes factor for the null (exclusion) over the alternative hypothesis}
\item{error}{Estimated numerical error}
\item{log10_BF}{Base-10 logarithm of the Bayes factor}
\item{favor}{Indicates whether data favor the null or alternative hypothesis}
\item{evidence}{Strength of evidence ("anecdotal", "moderate", "strong", "very strong", "extreme")}
}
}
\description{
This function summarizes the output of \code{generalTestBF(..., whichModels = "top")} from the BayesFactor package.
It computes the Bayes factor (BF) in favor of excluding each variable from the full model,
and classifies the strength of evidence based on the log BF scale.
}
\examples{
library(BayesFactor)
set.seed(123)
dat <- data.frame(
 id = as.factor(rep(1:30, each = 4)),
 A = as.factor(rep(c("low", "high"), times = 60)),
 B = as.factor(rep(c("left", "right"), each = 2, times = 30)),
 y = rnorm(120)
)

res <- generalTestBF(
 y ~ A + B + A:B + id + A:id + B:id,
 data = dat,
 whichRandom = c("id", "A:id", "B:id"),
 whichModels = "top"
)

summary_generalTestBF(res)

}
