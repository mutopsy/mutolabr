#' Compute visual angle or stimulus size in pixels
#'
#' If \code{stim_px} is provided and \code{stim_angle_deg} is \code{NULL},
#' the function returns the visual angle of the stimulus in degrees.
#' If \code{stim_angle_deg} is provided and \code{stim_px} is \code{NULL},
#' the function returns the stimulus size in pixels that subtends the
#' specified visual angle.
#'
#' Exactly one of \code{stim_px} and \code{stim_angle_deg} must be non-NULL.
#'
#' @param distance_cm Viewing distance (cm).
#' @param monitor_cm Physical size of the monitor (cm) along the relevant
#'   dimension (e.g., width).
#' @param monitor_px Number of pixels along the same dimension as
#'   \code{monitor_cm}.
#' @param stim_px Stimulus size in pixels. Set to \code{NULL} if you want to
#'   compute \code{stim_px} from \code{stim_angle_deg}.
#' @param stim_angle_deg Stimulus size in degrees. Set to \code{NULL} if you
#'   want to compute \code{stim_angle_deg} from \code{stim_px}.
#'
#' @return
#'   If \code{stim_px} is provided: a numeric vector of visual angles (degrees). \cr
#'   If \code{stim_angle_deg} is provided: a numeric vector of stimulus sizes (pixels).
#'
#' @examples
#' # 1) px -> degree
#' visualangle(
#'   distance_cm = 60,
#'   monitor_cm = 53.1,
#'   monitor_px = 1920,
#'   stim_px = 200
#' )
#'
#' # 2) degree -> px
#' visualangle(
#'   distance_cm = 60,
#'   monitor_cm = 53.1,
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
}
