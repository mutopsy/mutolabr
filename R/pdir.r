#' Compute the Highest Density Interval (HDI)
#'
#' This function calculates the highest density interval (HDI) for a given
#' set of samples. The HDI is the interval that contains a specified
#' proportion (default 95%) of the highest density values from the distribution.
#' Note that this function does not support the case where two or more separate
#' HDIs exist.
#'
#' @param x A numeric vector of samples from the distribution.
#' @param prob A numeric value between 0 and 1 specifying the probability mass
#'   for the HDI (default is 0.95).
#'
#' @return A numeric vector containing the values within the HDI.
#'
#' @examples
#' # Generate some random posterior samples
#' set.seed(123)
#' samples <- rnorm(1000, mean = 0, sd = 1)
#'
#' # Compute the 95% HDI
#' hdi(samples, prob = 0.95)
#'
#' @export

pdir <- function(x, na.rm = FALSE){
  if(na.rm) x <- x[!is.na(x)]
  out <- mean(x > 0)
  if(out < 0.5) out <- 1 - out
  return(out)
}
