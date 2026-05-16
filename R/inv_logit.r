#' Inverse Logit Transformation
#'
#' Computes the inverse logit transformation.
#'
#' The inverse logit transformation is defined as
#'
#' \deqn{
#' \mathrm{logit}^{-1}(x) = \frac{1}{1 + e^{-x}}
#' }
#'
#' This function maps real numbers onto probabilities in the interval
#' \eqn{(0,1)}.
#'
#' @param x A numeric vector.
#'
#' @return A numeric vector containing the inverse logit-transformed values.
#'
#' @examples
#' inv_logit(c(-2, 0, 2))
#'
#' @export
inv_logit <- function(x) {

  if (!is.numeric(x)) {
    stop("`x` must be numeric.")
  }

  1 / (1 + exp(-x))
}

