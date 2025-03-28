% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sd_desc.r
\name{sd_desc}
\alias{sd_desc}
\title{Compute Population Standard Deviation}
\usage{
sd_desc(x, na.rm = FALSE)
}
\arguments{
\item{x}{A numeric vector containing the data.}

\item{na.rm}{A logical value indicating whether to remove missing values (\code{NA}) before computation. Default is \code{FALSE}.}
}
\value{
A numeric value representing the population standard deviation of \code{x}.
}
\description{
This function calculates the population standard deviation of a numeric vector.
Unlike the default \code{sd()} function in R, which computes the sample standard deviation,
this function adjusts the calculation to use n instead of n-1 in the denominator.
}
\examples{
data <- c(10, 20, 30, 40, 50, NA)
sd_desc(data)          # Compute standard deviation including NA (returns NA)
sd_desc(data, na.rm = TRUE)  # Compute standard deviation ignoring NA

}
\seealso{
\code{\link{var_desc}} for computing the population variance.
}
