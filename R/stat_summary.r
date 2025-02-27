#' Compute Summary Statistics
#'
#' This function calculates summary statistics for a given numeric vector,
#' including mean, standard deviation, quartiles, and missing value counts.
#' The function supports both population and sample standard deviation calculations.
#'
#' @param x A numeric vector containing the sample data.
#' @param descriptive A logical value indicating whether to use the descriptive
#'   (population) standard deviation (`TRUE`) or the sample standard deviation (`FALSE`, default).
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{mean}{The mean of the non-missing values in \code{x}.}
#'   \item{sd}{The standard deviation of the non-missing values in \code{x}.}
#'   \item{q25}{The 25th percentile (first quartile).}
#'   \item{q50}{The 50th percentile (median).}
#'   \item{q75}{The 75th percentile (third quartile).}
#'   \item{n_val}{The count of non-missing values.}
#'   \item{n_na}{The count of missing values.}
#' }
#'
#' @examples
#' # Generate example data
#' set.seed(123)
#' data <- c(rnorm(100, mean = 50, sd = 10), NA, NA)
#'
#' # Compute summary statistics
#' stat_summary(data)
#' stat_summary(data, descriptive = TRUE)
#'
#' @export

stat_summary <- function(x, descriptive = FALSE){
  x_val <- x[!is.na(x)]
  if(descriptive){
    sd2 <- sd_desc
  } else{
    sd2 <- sd
  }

  out <- data.frame(
    mean = mean(x_val),
    sd = sd2(x_val),
    q25 = quantile(x, probs = 0.25),
    q50 = quantile(x, probs = 0.50),
    q75 = quantile(x, probs = 0.75),
    n_val = length(x_val),
    n_na = sum(is.na(x))
  )

  return(out)
}
