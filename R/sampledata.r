#' Example dataset: data_snakemr
#'
#' This dataset was used in a study on mental rotation involving cube stimuli with snake-like faces.
#'
#' @format A data frame with 480 rows and 5 variables:
#' \describe{
#'   \item{participant}{Participant ID}
#'   \item{angle}{Angular disparity between two Stimuli (in degrees)}
#'   \item{shape}{Stimulus shape (\code{"human"} or \code{"snake"})}
#'   \item{face}{Presence of face (\code{"absent"} or \code{"present"})}
#'   \item{rt}{Mean response time (in seconds)}
#' }
#'
#' @source Muto, H., & Nagai, M. (2020). Mental rotation of cubes with a snake face: The role of the human-body analogy revisited. \emph{Visual Cognition, 28}(2), 106–111. \url{https://doi.org/10.1080/13506285.2020.1727598}
#'
#' @usage data(data_snakemr)
#' @keywords datasets
"data_snakemr"

#' Example dataset: data_fict_between
#'
#' This is a fictitious dataset for 2*2 between-participants ANOVA.
#'
#' @format A data frame with 16 rows and 4 variables:
#' \describe{
#'   \item{participant}{Participant ID}
#'   \item{group}{Group (\code{"control"} or \code{"intervention"})}
#'   \item{gender}{Gender (\code{"woman"} or \code{"man"})}
#'   \item{n_correct}{Number of correct responses}
#' }
#'
#' @usage data(data_fict_between)
#' @keywords datasets
"data_fict_between"

#' Example dataset: data_fict_within
#'
#' This is a fictitious dataset for 2*2 within-participants ANOVA.
#'
#' @format A data frame with 64 rows and 4 variables:
#' \describe{
#'   \item{participant}{Participant ID}
#'   \item{condition}{Condition (\code{"control"} or \code{"experimental"})}
#'   \item{time}{Time (\code{"pre"} or \code{"post"})}
#'   \item{score}{Test score}
#' }
#'
#' @usage data(data_fict_within)
#' @keywords datasets
"data_fict_within"

#' Example dataset: data_fict_mixed
#'
#' This is a fictitious dataset for 2*2 mixed-design ANOVA.
#'
#' @format A data frame with 16 rows and 4 variables:
#' \describe{
#'   \item{participant}{Participant ID}
#'   \item{gender}{Gender (\code{"woman"} or \code{"man"})}
#'   \item{condition}{Condition (\code{"control"} or \code{"experimental"})}
#'   \item{rt}{Mean response time (in milliseconds)}
#' }
#'
#' @usage data(data_fict_mixed)
#' @keywords datasets
"data_fict_mixed"
