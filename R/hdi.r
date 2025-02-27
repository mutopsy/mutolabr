#' Compute the Highest Density Interval (HDI)
#'
#' This function calculates the highest density interval (HDI) for a given
#' set of samples. The HDI is the interval that contains a specified
#' proportion (default 95%) of the highest density values from the distribution.
#'
#' @param x A numeric vector of data values for which the HDI will be calculated.
#' @param prob The probability mass of the HDI to calculate. Default is 0.95.
#'
#' @return A numeric vector of length 2 representing the lower and upper bounds of the HDI.
#'
#' @details The function first sorts the input data and computes the necessary quantiles using inear interpolation.
#' It then identifies the interval that contains the highest density, ensuring that the HDI encompasses the specified probability mass.
#' Note that this function does not support the case where two or more separate
#' HDIs exist.
#'
#' @examples
#' set.seed(123)
#' data <- rnorm(1000)  # Generate random normal data
#' hdi2(data, prob = 0.95)
#'
#' @export

hdi <- function(x, prob = 0.95) {
  # Vector size
  n <- length(x)

  # Sort the data
  sorted_x <- sort(x)

  # Interval length (using n-1 to match quantile() behavior)
  interval_length <- (prob * (n - 1)) + 1

  # Calculate indices
  lower_idx <- (1:(n - interval_length + 1))
  upper_idx <- lower_idx + interval_length - 1

  # Fractional part of the indices
  lower_frac <- lower_idx - floor(lower_idx)
  upper_frac <- upper_idx - floor(upper_idx)

  # Calculate interpolated boundary values
  lower_val <- (1 - lower_frac) * sorted_x[floor(lower_idx)] + lower_frac * sorted_x[ceiling(lower_idx)]
  upper_val <- (1 - upper_frac) * sorted_x[floor(upper_idx)] + upper_frac * sorted_x[ceiling(upper_idx)]

  # Search for the minimum width of the HDI
  min_width <- Inf
  hdi_est <- c(NA, NA)

  for (i in seq_along(lower_val)) {
    current_width <- upper_val[i] - lower_val[i]

    if (current_width < min_width) {
      min_width <- current_width
      hdi_est <- c(lower_val[i], upper_val[i])
    }
  }

  return(hdi_est)
}
