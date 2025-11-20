#' Convert between visual angle (degrees) and stimulus size (pixels)
#'
#' This function converts either:
#' \itemize{
#'   \item stimulus size in pixels (\code{stim_px}) to visual angle in degrees, or
#'   \item visual angle in degrees (\code{stim_angle_deg}) to stimulus size in pixels
#' }
#' based on viewing distance and monitor characteristics.
#'
#' Exactly one of \code{stim_px} or \code{stim_angle_deg} must be provided.
#'
#' @param distance_cm Viewing distance in centimeters.
#' @param monitor_cm Physical monitor width in centimeters.
#' @param monitor_px Monitor resolution in pixels corresponding to \code{monitor_cm}.
#' @param stim_px Stimulus size in pixels. Provide this to compute visual angle.
#' @param stim_angle_deg Stimulus visual angle in degrees. Provide this to compute size in pixels.
#'
#' @return
#' If \code{stim_px} is provided, returns the corresponding visual angle (degrees).
#'
#' If \code{stim_angle_deg} is provided, returns the corresponding stimulus size (pixels).
#'
#' @examples
#' # Convert 200 px stimulus to degrees at 60 cm viewing distance
#' visualangle(
#'   distance_cm = 60,
#'   monitor_cm = 53,   # width of a typical 24-inch monitor
#'   monitor_px = 1920,
#'   stim_px = 200
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
    return(stim_angle_deg)
  }

  # degree -> px
  if (!is.null(stim_angle_deg)) {
    stim_angle_radian <- degree_to_radian(stim_angle_deg)
    stim_cm <- tan(stim_angle_radian / 2) * 2 * distance_cm
    stim_px <- cm_to_px(stim_cm)
    return(stim_px)
  }

  out <- data.frame(
    stim_px = stim_px,
    stim_angle_deg = stim_angle_deg,
    stim_cm = stim_cm
  )
}
