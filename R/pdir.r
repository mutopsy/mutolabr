#' Probability of Direction (pd) Calculation
#'
#' This function calculates the probability of direction (pd), which is the proportion of
#' the distribution of the same sign as its median’s and varies from 0.50 to 1.00.
#' This is an index of effect existence, proposed by Makowski et al. (2019).
#'
#' @param x A numeric vector of values.
#' @param na.rm A logical value indicating whether `NA` values should be removed before calculation.
#'              Default is `FALSE`.
#'
#' @return A numeric value between 0 and 1, representing the Probability of Direction (Pd).
#'         The value is calculated as the proportion of positive values in `x`, or 1 minus
#'         the proportion if it is less than 0.5.
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(1000)
#' pdir(x)  # Returns the Probability of Direction for x
#' pdir(x, na.rm = TRUE)  # Removes NA values before calculation
#'
#' @references
#' Makowski, D., Ben-Shachar, M. S., Chen, S. H. A., & Lüdecke, D. (2019). Indices of effect
#' existence and significance in the Bayesian framework. Frontiers in Psychology, 10, Article
#' 2767. https://doi.org/10.3389/fpsyg.2019.02767
#'
#' @export

pdir <- function(x, na.rm = FALSE){
  if(na.rm) x <- x[!is.na(x)]
  out <- mean(x > 0)
  if(out < 0.5) out <- 1 - out
  return(out)
}
