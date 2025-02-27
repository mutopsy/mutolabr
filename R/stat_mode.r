#' Compute the Mode of a Distribution
#'
#' This function estimates the mode (the most frequent value) of a given
#' numeric vector using kernel density estimation. The mode is identified
#' as the value where the kernel density is maximized.
#'
#' @param z A numeric vector containing the values from which the mode is to be estimated.
#' @param n An integer specifying the number of equally spaced grid points for kernel density estimation (default is 512).
#'
#' @return A numeric value representing the estimated mode of the distribution.
#'
#' @examples
#' # Generate random data from a normal distribution
#' set.seed(123)
#' data <- rnorm(1000)
#'
#' # Compute the mode of the distribution
#' stat_mode(data)
#'
#' @export

stat_mode <- function(z, n = 512) density(z, n = n)$x[which.max(density(z, n = n)$y)]
