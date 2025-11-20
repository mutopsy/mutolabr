#' Convert Display Diagonal Size (inch) to Width and Height (cm)
#'
#' This function converts a display's diagonal length in inches into its
#' physical width and height in centimeters, assuming a specified aspect ratio.
#' It is useful for psychological experiments and visual stimulus control,
#' where precise physical screen dimensions are required.
#'
#' @param diagonal_inch Numeric vector. Diagonal size(s) of the display in inches.
#' @param aspect_ratio Numeric vector of length 2. The width-to-height ratio
#'   (default is \code{c(16, 9)} for a standard widescreen display).
#'
#' @return A data frame with two columns:
#'   \describe{
#'     \item{width_cm}{Width of the display in centimeters.}
#'     \item{height_cm}{Height of the display in centimeters.}
#'   }
#'
#' @examples
#' # Convert a 24-inch monitor with 16:9 aspect ratio
#' diaginch_to_cm(24)
#'
#' # Convert multiple displays at once
#' diaginch_to_cm(c(13, 24, 27))
#'
#' # Convert a 4:3 display
#' diaginch_to_cm(19, aspect_ratio = c(4, 3))
#'
#' @export

diaginch_to_cm <- function(diagonal_inch, aspect_ratio = c(16, 9)) {
  ar_w <- aspect_ratio[1]
  ar_h <- aspect_ratio[2]

  # Width and Height in inch
  width_inch <- diagonal_inch * ar_w / sqrt(ar_w^2 + ar_h^2)
  height_inch <- diagonal_inch * ar_h / sqrt(ar_w^2 + ar_h^2)

  # Width and Height in cm
  width_cm <- width_inch * 2.54
  height_cm <- height_inch * 2.54

  out <- data.frame(
    width_cm = width_cm,
    height_cm = height_cm
  )

  return(out)
}
