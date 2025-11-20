#' Convert degrees to radians
#'
#' This function converts an angle measured in degrees to radians.
#'
#' @param x A numeric vector representing an angle in degrees.
#'
#' @return A numeric vector giving the angle in radians.
#'
#' @examples
#' degree_to_radian(180) # pi
#' degree_to_radian(90)  # pi/2
#'
#' @export
degree_to_radian <- function(x) x * pi / 180
