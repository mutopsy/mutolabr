#' Convert radians to degrees
#'
#' This function converts an angle measured in radians to degrees.
#'
#' @param x A numeric vector representing an angle in radians.
#'
#' @return A numeric vector giving the angle in degrees.
#'
#' @examples
#' radian_to_degree(pi)     # 180
#' radian_to_degree(pi / 2) # 90
#'
#' @export
radian_to_degree <- function(x) x * 180 / pi
