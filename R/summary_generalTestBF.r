#' Summarize Results from generalTestBF
#'
#' This function summarizes the output of `generalTestBF(..., whichModels = "top")` from the BayesFactor package.
#' It computes the Bayes factor (BF) in favor of excluding each variable from the full model,
#' and classifies the strength of evidence based on the log BF scale.
#'
#' @param x An object returned by `generalTestBF()`, or a named list containing:
#'   \describe{
#'     \item{res_BF}{An object returned by `generalTestBF()`}
#'     \item{variables}{Character vector of variable names}
#'     \item{full_model_vars}{Character vector of variables in the full model}
#'   }
#' @param inc_ranef Logical. Whether to output results of random effects.
#'
#' @return A tibble with one row per model comparison. Columns include:
#' \describe{
#'   \item{effect}{The effect excluded from the full model}
#'   \item{type}{Indicates whether the term is Fixed or random effect}
#'   \item{BF}{Bayes factor for the null (exclusion) over the alternative hypothesis}
#'   \item{error}{Estimated numerical error}
#'   \item{log10_BF}{Base-10 logarithm of the Bayes factor}
#'   \item{favor}{Indicates whether data favor the null or alternative hypothesis}
#'   \item{evidence}{Strength of evidence ("anecdotal", "moderate", "strong", "very strong", "extreme")}
#' }
#'
#' @importFrom dplyr transmute arrange mutate if_else
#' @importFrom tibble as_tibble
#' @importFrom stringr str_replace_all
#' @importFrom stats setNames
#'
#' @examples
#' library(BayesFactor)
#' set.seed(123)
#' dat <- data.frame(
#'  id = as.factor(rep(1:30, each = 4)),
#'  A = as.factor(rep(c("low", "high"), times = 60)),
#'  B = as.factor(rep(c("left", "right"), each = 2, times = 30)),
#'  y = rnorm(120)
#' )
#'
#' res <- generalTestBF(
#'  y ~ A + B + A:B + id + A:id + B:id,
#'  data = dat,
#'  whichRandom = c("id", "A:id", "B:id"),
#'  whichModels = "top"
#' )
#'
#' summary_generalTestBF(res)
#'
#' @export

summary_generalTestBF <- function(x, inc_ranef = FALSE){
  if(is.list(x)){
    res_BF <- x$res_BF
    variables <- x$variables
    full_model_vars <- x$full_model_vars
  } else{
    res_BF <- x
    variables <- colnames(res_BF@data)[2:(ncol(res_BF@data)-1)]
    full_model_vars <- gsub(" ", "", res_BF@denominator@shortName)
    full_model_vars <- unlist(strsplit(full_model_vars, "\\+")) %>% unique()
  }

  fixed <- full_model_vars[!str_detect(full_model_vars, "s")]
  rn <- res_BF %>% as.data.frame() %>% rownames()

  BF_df <- res_BF %>%
    as_tibble() %>%
    transmute(
      effect = sapply(
        rn,
        function(model_formula) {
          current_vars <- strsplit(gsub("y ~ ", "", model_formula), split = " \\+ ")[[1]]
          excluded <- setdiff(full_model_vars, current_vars)
          paste(excluded, collapse = ", ")
        }
      ),
      effect = factor(effect, levels = full_model_vars),
      BF = 1/bf,
      error = error %>% signif(2),
      log10_BF = log(BF, 10),
      BF = BF %>% signif(3)
    ) %>%
    arrange(effect) %>%
    mutate(type = if_else(effect %in% fixed, "fixed", "random"), .after = "effect")

  names(variables) <- colnames(res_BF@data)[2:(length(variables)+1)]

  BF_df <- BF_df %>%
    mutate(
      effect = str_replace_all(effect, variables),
      favor = if_else(BF > 1, "alt.", "null"),
      evidence = "anecdotal",
      evidence = if_else(abs(log10_BF) > log(3, 10), "moderate", evidence),
      evidence = if_else(abs(log10_BF) > log(10, 10), "strong", evidence),
      evidence = if_else(abs(log10_BF) > log(30, 10), "very strong", evidence),
      evidence = if_else(abs(log10_BF) > log(100, 10), "extreme", evidence)
    )

  if(!inc_ranef){
    BF_df <- BF_df %>%
      dplyr::filter(type == "fixed")
  }

  return(BF_df)
}
