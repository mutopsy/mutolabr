#' Logit Transformation
#'
#' Computes the logit transformation of probabilities.
#'
#' The logit transformation is defined as
#'
#' \deqn{
#' \mathrm{logit}(p) = \log \left( \frac{p}{1-p} \right)
#' }
#'
#' where \eqn{p} must satisfy \eqn{0 < p < 1}.
#'
#' @param p A numeric vector of probabilities.
#'
#' @return A numeric vector containing the logit-transformed values.
#'
#' @examples
#' logit(c(0.2, 0.5, 0.8))
#'
#' @export
logit <- function(p) {

  if (!is.numeric(p)) {
    stop("`p` must be numeric.")
  }

  if (any(p <= 0 | p >= 1, na.rm = TRUE)) {
    stop("All values of `p` must satisfy 0 < p < 1.")
  }

  log(p / (1 - p))
}
