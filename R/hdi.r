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

hdi <- function(x, prob = 0.95) {

  # vector size
  n <- length(x)

  # sort
  sorted_x <- sort(x)

  # interval length
  interval_length <- ceiling(prob * n)

  hdi_est <- NULL
  min_width <- Inf

  for (i in 1:(n - interval_length + 1)) {
    current_interval <- sorted_x[i:(i + interval_length - 1)]
    current_width <- diff(range(current_interval))

    if (current_width < min_width) {
      min_width <- current_width
      hdi_est <- range(current_interval)
    }
  }

  return(hdi_est)
}

hdi <- function(x, prob = 0.95) {

  # vector size
  n <- length(x)

  # sort
  sorted_x <- sort(x)

  # interval length
  interval_length <- ceiling(prob * n)

  hdi_est <- NULL
  min_width <- Inf

  for (i in 1:(n - interval_length + 1)) {
    current_interval <- sorted_x[i:(i + interval_length - 1)]
    current_width <- diff(range(current_interval))

    if (current_width < min_width) {
      min_width <- current_width
      hdi_est <- current_interval
    }
  }

  return(hdi_est)
}
