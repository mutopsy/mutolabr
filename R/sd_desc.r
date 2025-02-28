#' Compute Population Standard Deviation
#'
#' This function calculates the population standard deviation of a numeric vector.
#' Unlike the default `sd()` function in R, which computes the sample standard deviation,
#' this function adjusts the calculation to use n instead of n-1 in the denominator.
#'
#' @param x A numeric vector containing the data.
#' @param na.rm A logical value indicating whether to remove missing values (`NA`) before computation. Default is `FALSE`.
#'
#' @return A numeric value representing the population standard deviation of `x`.
#'
#' @examples
#' data <- c(10, 20, 30, 40, 50, NA)
#' sd_desc(data)          # Compute standard deviation including NA (returns NA)
#' sd_desc(data, na.rm = TRUE)  # Compute standard deviation ignoring NA
#'
#' @seealso \code{\link{var_desc}} for computing the population variance.
#'
#' @export

sd_desc <- function(x, na.rm = FALSE) sqrt(var_desc(x, na.rm = na.rm))
