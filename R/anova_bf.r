#' Perform Bayesian ANOVA Using generalTestBF
#'
#' Bayesian (Mixed / Repeated-Measures) ANOVA Wrapper Using \code{BayesFactor::generalTestBF}
#'
#' Runs a Bayesian ANOVA for mixed or repeated-measures designs via
#' \code{BayesFactor::generalTestBF(..., whichModels = "top")}, given a compact
#' design string (e.g., \code{"sABC"}, \code{"ABsC"}). One subject column, one
#' dependent variable column, and factor columns in between are assumed.
#'
#' @param dataset A data frame in (long) ANOVA format:
#'   the **first** column is the subject identifier,
#'   the **last** column is the dependent variable,
#'   and the columns in between are factors (between/within are inferred from \code{design}).
#' @param design A single string specifying the design. It must contain
#'   exactly one \code{"s"} (for subjects). Letters left of \code{"s"} are
#'   between-subject factors; letters right of \code{"s"} are within-subject factors.
#'   Examples: \code{"sAB"} (two within), \code{"ABsC"} (two between, one within).
#' @param rscaleFixed Prior scale for fixed effects. Default \code{"medium"}.
#' @param rscaleRandom Prior scale for random effects. Default \code{"nuisance"}.
#' @param rscaleCont Prior scale for continuous covariates. Default \code{"medium"}.
#' @param rscaleEffects Optional vector of prior scales for individual effects.
#' @param method Method for computing Bayes factors; see \code{BayesFactor::generalTestBF()}.
#'   Default \code{"auto"}.
#' @param progress Logical; show progress bar? Default \code{TRUE}.
#' @param multicore Logical; use multicore processing? Default \code{FALSE}.
#' @param summarize Logical; if \code{TRUE} (default), return a summarized tibble;
#'   if \code{FALSE}, return the raw \code{BayesFactor} result.
#' @param inc_ranef Logical; when summarizing, also include random-effect terms
#'   in the table (passed to \code{mutolabr::summary_generalTestBF()}).
#' @param seed Optional numeric seed to reproduce Monte Carlo results; passed to
#'   \code{withr::with_seed()}.
#'
#' @return If \code{summarize = TRUE} (default), a tibble summarizing Bayes factors
#'   for exclusion of each term from the full model, with columns:
#'   \describe{
#'     \item{effect}{Effect removed from the full model (fixed or, optionally, random).}
#'     \item{BF}{Bayes factor for the null (exclusion) vs. alternative (full).}
#'     \item{error}{Estimated numerical error.}
#'     \item{log10_BF}{Base-10 logarithm of BF.}
#'     \item{favor}{Whether data favor the null or alternative.}
#'     \item{evidence}{Evidence category (e.g., "anecdotal", "moderate", ...).}
#'   }
#'   If \code{summarize = FALSE}, returns the \code{BayesFactor} object produced by
#'   \code{generalTestBF()}.
#'
#' @details
#' Internally the function:
#' \itemize{
#'   \item Validates \code{design} (\code{"s"} must appear exactly once) and checks that
#'   the number of factor columns matches the count implied by \code{design}.
#'   \item Renames columns to \code{"s"} (subject), \code{"fb1","fb2",...} (between),
#'   \code{"fw1","fw2",...} (within), and \code{"y"} (outcome), then coerces factors.
#'   \item Drops rows with missing values and unused factor levels.
#'   \item Builds the fixed-effect formula including all main effects and all interactions
#'   among \code{fb*} and \code{fw*}.
#'   \item Sets random effects to \code{s} and \code{s:fw} combinations for all within-factor
#'   subsets **except** the highest-order within interaction (which coincides with the residual
#'   error term in classical ANOVA). No between-factor terms are included as random effects.
#'   \item Standardizes \code{y} to mean 0 and SD 1 prior to analysis.
#' }
#' The Bayes factors are computed with \code{whichModels = "top"} (full model vs.
#' models with one term removed) and summarized via \code{mutolabr::summary_generalTestBF()}.
#'
#' @seealso \code{\link[BayesFactor]{generalTestBF}},
#'   \code{\link{summary_generalTestBF}}
#'
#' @import dplyr
#' @import tidyr
#' @importFrom BayesFactor generalTestBF
#' @importFrom withr with_seed
#' @importFrom stats as.formula
#' @importFrom stringr str_count
#'
#' @examples
#' # Example 1: between-participants design (design = "ABs")
#'
#' anova_bf(data_fict_between, design = "ABs")
#'
#' # Example 2: within-participants design (design = "sAB")
#'
#' anova_bf(data_fict_within, design = "sAB")
#'
#' # Example 3: mixed design (between = A, within = C)
#'
#' anova_bf(data_fict_mixed, design = "AsB")
#'
#' @export

anova_bf <- function(
    dataset,
    design,
    rscaleFixed = "medium",
    rscaleRandom = "nuisance",
    rscaleCont = "medium",
    rscaleEffects = NULL,
    method = "auto",
    progress = TRUE,
    multicore = FALSE,
    summarize = TRUE,
    inc_ranef = FALSE,
    seed = NULL
){

  ## Calculate numbers of factors and levels

  ncol <- ncol(dataset)

  nlevel <- dataset[,2:(ncol-1)] %>%
    dplyr::summarise(across(everything(), ~ length(unique(.)))) %>%
    unlist() %>%
    as.numeric()

  nfct <- length(nlevel)

  nfct_between <- as.numeric(regexpr("s", design)) - 1
  nfct_within <- nchar(design)  - as.numeric(regexpr("s", design))

  if(nfct_between + nfct_within != nfct){
    stop("The specified design does not match the given dataset.")
  }

  if (str_count(design, "s") != 1L) {
    stop("design must contain exactly one 's' (e.g., 'sABC' or 'ABsC').")
  }

  ## Rename variables

  x <- dataset

  colnames(x)[1] <- "s"
  if(nfct_between!=0) colnames(x)[2:(nfct_between+1)] <- paste0("fb", 1:nfct_between)
  if(nfct_within!=0) colnames(x)[(1+nfct_between+1):(1+nfct_between+nfct_within)] <- paste0("fw", 1:nfct_within)

  colnames(x)[ncol(dataset)] <- "y"

  variables <- colnames(dataset)[2:(ncol(dataset)-1)]
  names(variables) <- colnames(x)[2:(ncol(x)-1)]

  if (nfct_between > 0) x[paste0("fb", seq_len(nfct_between))] <- lapply(x[paste0("fb", seq_len(nfct_between))], factor)
  if (nfct_within  > 0) x[paste0("fw", seq_len(nfct_within ))] <- lapply(x[paste0("fw", seq_len(nfct_within ))], factor)
  x$s <- factor(x$s)

  ## Omit NA

  x <- x %>%
    dplyr::select(s, dplyr::starts_with("fb"), dplyr::starts_with("fw"), y) %>%
    tidyr::drop_na() %>%
    droplevels()

  ## Specify fixed effect terms

  fctname_b <- NULL
  fctname_w <- NULL

  if(nfct_between != 0) fctname_b <- paste0("fb", 1:nfct_between)
  if(nfct_within != 0) fctname_w <- paste0("fw", 1:nfct_within)

  fctname <- c(fctname_b, fctname_w)

  fixed_vars <- unlist(lapply(1:length(fctname), function(k) {
    apply(combn(fctname, k), 2, paste, collapse=":")
  }))


  ## Specify random effect terms

  rand_vars <- "s"

  if (nfct_within >= 1) {
    if (nfct_within == 1) {
      within_terms <- fctname_w
    } else {
      within_terms <- unlist(lapply(1:(nfct_within - 1), function(k) {
        apply(combn(fctname_w, k), 2, paste, collapse=":")
      }), use.names = FALSE)
    }
    rand_vars <- c(rand_vars, paste(within_terms, "s", sep=":"))
  } else{
    rand_vars <- NULL
  }

  full_model_vars <- c(fixed_vars, rand_vars)

  ## Perform Bayesian ANOVA

  data_for_bf <- x %>%
    as.data.frame() %>%
    mutate(
      s = as.factor(s),
      y = (y - mean(y))/sd(y)
    )

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
        multicore = multicore
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
      multicore = multicore
    )
  }

  if(summarize){
    res_BF_list <- list(
      res_BF = res_BF,
      variables = variables,
      full_model_vars = full_model_vars,
      rand_vars = rand_vars
    )

    BF_df <- mutolabr::summary_generalTestBF(res_BF_list, inc_ranef = inc_ranef)

    return(BF_df)

  } else{
    return(res_BF)
  }
}
