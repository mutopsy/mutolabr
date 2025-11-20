#' Convert between stimulus size (pixels) and visual angle (degrees)
#'
#' This function converts either:
#' \itemize{
#'   \item stimulus size in pixels (\code{stim_px}) to visual angle in degrees, or
#'   \item visual angle in degrees (\code{stim_angle_deg}) to stimulus size in pixels,
#' }
#' given the viewing distance and the monitor's physical and pixel dimensions.
#'
#' Exactly one of \code{stim_px} or \code{stim_angle_deg} must be supplied.
#' The function returns a data frame containing stimulus size in pixels,
#' visual angle in degrees, and the corresponding physical size in centimeters.
#'
#' @param distance_cm Viewing distance from the screen in centimeters.
#' @param monitor_cm Physical width of the monitor (cm) that corresponds to \code{monitor_px}.
#' @param monitor_px Horizontal pixel resolution corresponding to \code{monitor_cm}.
#' @param stim_px Stimulus size in pixels. Provide this to compute visual angle.
#' @param stim_angle_deg Stimulus size in degrees of visual angle.
#' Provide this to compute stimulus size in pixels.
#'
#' @return A data frame with three columns:
#' \describe{
#'   \item{stim_px}{Stimulus size in pixels.}
#'   \item{stim_angle_deg}{Stimulus visual angle in degrees.}
#'   \item{stim_cm}{Stimulus size in centimeters.}
#' }
#'
#' @examples
#' # Convert 100 px to degrees at 60 cm viewing distance
#' visualangle(
#'   distance_cm = 60,
#'   monitor_cm = 53,
#'   monitor_px = 1920,
#'   stim_px = 100
#' )
#'
#' # Convert 5 degrees to pixels
#' visualangle(
#'   distance_cm = 60,
#'   monitor_cm = 53,
#'   monitor_px = 1920,
#'   stim_angle_deg = 5
#' )
#'
#' @export

visualangle <- function(
    distance_cm,
    monitor_cm,
    monitor_px,
    stim_px = NULL,
    stim_angle_deg = NULL
) {
  cm_to_px <- function(x) x * monitor_px / monitor_cm
  px_to_cm <- function(x) x * monitor_cm / monitor_px

  # Check
  if (is.null(stim_px) && is.null(stim_angle_deg)) {
    stop("Either 'stim_px' or 'stim_angle_deg' must be provided.")
  }
  if (!is.null(stim_px) && !is.null(stim_angle_deg)) {
    stop("Only one of 'stim_px' or 'stim_angle_deg' should be provided.")
  }

  # px -> degree
  if (!is.null(stim_px)) {
    stim_cm <- px_to_cm(stim_px)
    stim_angle_radian <- 2 * atan(stim_cm / (2 * distance_cm))
    stim_angle_deg <- radian_to_degree(stim_angle_radian)
  }

  # degree -> px
  if (!is.null(stim_angle_deg)) {
    stim_angle_radian <- degree_to_radian(stim_angle_deg)
    stim_cm <- tan(stim_angle_radian / 2) * 2 * distance_cm
    stim_px <- cm_to_px(stim_cm)
  }

  out <- data.frame(
    stim_px = stim_px,
    stim_angle_deg = stim_angle_deg,
    stim_cm = stim_cm
  )

  return(out)
}
