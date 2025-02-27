#' Compute Population Variance
#'
#' This function calculates the population variance of a numeric vector.
#' Unlike the default `var()` function in R, which computes the sample variance,
#' this function adjusts the calculation to use \( n \) instead of \( n-1 \) in the denominator.
#'
#' @param x A numeric vector containing the data.
#' @param na.rm A logical value indicating whether to remove missing values (`NA`) before computation. Default is `FALSE`.
#'
#' @return A numeric value representing the population variance of `x`.
#'
#' @examples
#' data <- c(10, 20, 30, 40, 50, NA)
#' var_desc(data)          # Compute variance including NA (returns NA)
#' var_desc(data, na.rm = TRUE)  # Compute variance ignoring NA
#'
#' @seealso \code{\link{sd_desc}} for computing the population standard deviation.
#'
#' @export

var_desc <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]

  n <- length(x)
  if (n == 0) return(NA)

  out <- var(x) * (n - 1) / n
  return(out)
}
