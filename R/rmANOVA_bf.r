#' Perform Bayesian Repeated-Measures ANOVA Using generalTestBF
#'
#' Performs a Bayesian repeated-measures ANOVA using the `generalTestBF(..., whichModels = "top")` function from the \pkg{BayesFactor} package,
#' assuming one subject column, multiple within-subject factor columns, and one dependent variable column.
#'
#' @param x A data frame in long format. The first column should be the subject identifier.
#'   The last column should be the dependent variable, and the columns in between should be within-subject factors.
#' @param rscaleFixed Prior scale for fixed effects. Default is `"medium"`.
#' @param rscaleRandom Prior scale for random effects. Default is `"nuisance"`.
#' @param rscaleCont Prior scale for continuous covariates. Default is `"medium"`.
#' @param rscaleEffects Optional vector of prior scales for individual effects.
#' @param method Method for computing Bayes factors. See `BayesFactor::generalTestBF()`. Default is `"auto"`.
#' @param progress Logical. Whether to display progress bar. Default is `TRUE`.
#' @param multicore Logical. Whether to use multicore processing. Default is `FALSE`.
#' @param seed Optional. A numeric seed to fix the random number generator state for reproducibility.
#'
#' @return A tibble summarizing Bayes factors for each model compared to the full model.
#'   Columns include:
#'   \describe{
#'     \item{effect}{The effect excluded from the full model}
#'     \item{BF}{Bayes factor for the null (exclusion) over the alternative hypothesis}
#'     \item{error}{Estimated numerical error}
#'     \item{log10_BF}{Base-10 logarithm of the Bayes factor}
#'     \item{favor}{Indicates whether data favor the null or alternative hypothesis}
#'     \item{evidence}{Strength of evidence ("anecdotal", "moderate", "strong", "very strong", "extreme")}
#'   }
#'
#' @details
#' The function currently supports designs with 2 or 3 within-subject factors only.
#' The subject column is automatically renamed to `"s"`, factor columns to `"fw1"`, `"fw2"`, etc.,
#' and the outcome variable to `"y"` internally for formula construction.
#'
#' @import dplyr
#' @importFrom BayesFactor generalTestBF
#' @importFrom withr with_seed
#' @importFrom stats as.formula
#' @seealso \code{\link[BayesFactor]{generalTestBF}}, \code{\link{summary_generalTestBF}}
#'
#' @examples
#' \dontrun{
#' library(BayesFactor)
#' # Simulated data with subject, 2 factors, and outcome
#' set.seed(123)
#' dat <- data.frame(
#'   id = rep(1:30, each = 4),
#'   A = rep(c("low", "high"), times = 60),
#'   B = rep(c("left", "right"), each = 2, times = 30),
#'   y = rnorm(120)
#' )
#' res <- rmANOVA_bf(dat)
#' print(res)
#' }
#'
#' @export

rmANOVA_bf <- function(
    x,
    rscaleFixed = "medium",
    rscaleRandom = "nuisance",
    rscaleCont = "medium",
    rscaleEffects = NULL,
    method = "auto",
    progress = TRUE,
    multicore = FALSE,
    seed = NULL
){

  variables <- colnames(x)[2:(ncol(x)-1)]

  colnames(x)[1] <- "s"
  colnames(x)[2:(ncol(x)-1)] <- paste0("fw", 1:(ncol(x)-2))
  colnames(x)[ncol(x)] <- "y"

  names(variables) <- colnames(x)[2:(ncol(x)-1)]

  if(length(variables) == 2){
    full_model_vars <- c("fw1", "fw2", "fw1:fw2", "s", "fw1:s", "fw2:s")
    rand_vars <- c("s", "fw1:s", "fw2:s")
  }

  if(length(variables) == 3){
    full_model_vars <- c("fw1", "fw2", "fw3", "fw1:fw2", "fw2:fw3", "fw1:fw3", "fw1:fw2:fw3", "s", "fw1:s", "fw2:s", "fw3:s", "fw1:fw2:s", "fw2:fw3:s", "fw1:fw3:s")
    rand_vars <- c("s", "fw1:s", "fw2:s", "fw3:s", "fw1:fw2:s", "fw2:fw3:s", "fw1:fw3:s")
  }

  data_for_bf <- x %>%
    as.data.frame() %>%
    mutate(s = as.factor(s))

  if (!is.null(seed)) {
    res_BF <- withr::with_seed(
      seed,
      generalTestBF(
        data = data_for_bf,
        as.formula(paste0("y ~ ", paste(full_model_vars, collapse = " + "))),
        whichRandom = rand_vars,
        rscaleFixed = rscaleFixed,
        rscaleRandom = rscaleRandom,
        rscaleCont = rscaleCont,
        rscaleEffects = rscaleEffects,
        method = method,
        whichModels = "top",
        progress = progress,
        multicore = multicore # When an error occurs, please replace "T" with "F".
      )
    )
  } else {
    res_BF <- generalTestBF(
      data = data_for_bf,
      as.formula(paste0("y ~ ", paste(full_model_vars, collapse = " + "))),
      whichRandom = rand_vars,
      rscaleFixed = rscaleFixed,
      rscaleRandom = rscaleRandom,
      rscaleCont = rscaleCont,
      rscaleEffects = rscaleEffects,
      method = method,
      whichModels = "top",
      progress = progress,
      multicore = multicore # When an error occurs, please replace "T" with "F".
    )
  }

  res_BF_list <- list(
    res_BF = res_BF,
    variables = variables,
    full_model_vars = full_model_vars,
    rand_vars = rand_vars
  )

  BF_df <- mutolabr::summary_generalTestBF(res_BF_list)

  return(BF_df)
}
