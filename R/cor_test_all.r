#' Perform Correlation Tests with Frequentist and Bayesian Methods
#'
#' This function computes pairwise correlations for all variables in a given dataset,
#' supporting both frequentist and Bayesian approaches. It provides multiple correlation estimates,
#' confidence intervals, Bayesian credible intervals, probability of direction (pd), and Bayes factors.
#'
#' @param dat A data frame or matrix containing numeric variables for correlation analysis.
#' @param cor Logical. If `TRUE`, computes frequentist correlation coefficients (default: `TRUE`).
#' @param cor_EAP Logical. If `TRUE`, computes the expected a posteriori (EAP) estimate of the correlation coefficient (default: `FALSE`).
#' @param cor_MAP Logical. If `TRUE`, computes the maximum a posteriori (MAP) estimate of the correlation coefficient (default: `FALSE`).
#' @param cor_MED Logical. If `TRUE`, computes the median of the posterior distribution (MED) for the correlation coefficient (default: `FALSE`).
#' @param pd Logical. If `TRUE`, computes the probability of direction (pd) based on posterior distributions (default: `FALSE`).
#' @param bf Logical. If `TRUE`, computes Bayes factors for the presence of correlation versus the null hypothesis (default: `FALSE`).
#' @param ci Character. Specifies the type of confidence or credible interval:
#' `"freq"` (frequentist confidence interval), `"bayes_central"` (Bayesian central credible interval),
#' or `"bayes_hdi"` (highest density interval based on the posterior distribution) (default: `"freq"`).
#' @param triangle Character. Specifies which part of the correlation matrix to return:
#' `"upper"`, `"lower"`, or `"full"` (default: `"upper"`).
#' @param alternative Character. Specifies the alternative hypothesis for the frequentist test:
#' `"two.sided"`, `"less"`, or `"greater"` (default: `"two.sided"`).
#' @param method Character. Specifies the correlation method for the frequentist test:
#' `"pearson"`, `"kendall"`, or `"spearman"` (default: `"pearson"`).
#' @param exact Logical or `NULL`. If `TRUE`, computes exact p-values for small sample sizes when using Spearman or Kendall correlations.
#' @param conf.level Numeric. The confidence level for frequentist intervals or credibility level for Bayesian intervals (default: `0.95`).
#' @param alpha Numeric. Significance level. Defaults to 0.05.
#' @param continuity Logical. If `TRUE`, applies a continuity correction for Kendall correlations (default: `FALSE`).
#' @param rscale_est Numeric or character. Specifies the Cauchy prior scale for Bayesian estimation of the posterior distribution.
#' Options: `"ultrawide"`, `"wide"`, `"medium"`, or a positive real number (default: `"ultrawide"`).
#' Passed to `BayesFactor::correlationBF()`.
#' @param rscale_bf Numeric or character. Specifies the Cauchy prior scale for Bayes factor calculation.
#' Options: `"ultrawide"`, `"wide"`, `"medium"`, or a positive real number (default: `"ultrawide"`).
#' Passed to `BayesFactor::correlationBF()`.
#' @param iterations Integer. Number of MCMC samples for Bayesian estimation (default: `10000`).
#' @param map_density_n Integer. Number of bins for MAP density estimation (default: `512`).
#' @param detailed Logical. Whether to return detailed results (\code{TRUE}) or
#'   minimal output (\code{FALSE}, default).
#' @param fullbayes Logical. Whether to show only Bayesian results (\code{TRUE}) or both frequentist and Bayesian results (\code{FALSE}, default).
#' @param ... Additional arguments passed to `cor.test()`.
#'
#' @return A list containing:
#' \describe{
#'   \item{all}{A data frame with all computed correlation statistics.}
#'   \item{table_XX}{A data frame corresponding to a table named "table_XX",
#'   where "XX" is derived from the output variables (e.g., `"table_cor"`, `"table_p"`, `"table_BF10"`).
#'   The content of the table depends on the provided inputs.}
#' }
#'
#' @examples
#' # Frequentist analysis
#' results <- cor_test_all(mtcars[, 1:5])
#' results$all  # View detailed results in a tidy format
#' results$table_cor  # View correlation matrix
#'
#' # Calculation of Bayesian statistics
#' results <- cor_test_all(mtcars[, 1:5], cor_MAP = TRUE, ci = "bayes_hdi",
#'                         bf = TRUE, pd = TRUE, rscale_est = "ultrawide",
#'                         rscale_bf = "ultrawide")
#' results$all  # View detailed results in a tidy format
#' results$table_BF10  # View Bayes factor matrix
#' @import stats
#' @importFrom dplyr mutate arrange %>% filter if_else transmute left_join rename select n everything join_by
#' @importFrom tidyr drop_na
#' @importFrom BayesFactor correlationBF
#' @export

cor_test_all <- function(
    dat,
    cor = TRUE, cor_EAP = FALSE, cor_MAP = FALSE, cor_MED = FALSE, pd = FALSE, bf = FALSE,
    ci = c("freq","bayes_central",  "bayes_hdi"),
    triangle = c("upper", "lower", "full"),
    alternative = c("two.sided", "less", "greater"),
    method = c("pearson", "kendall", "spearman"),
    exact = NULL, conf.level = 0.95, alpha = 0.05, continuity = FALSE,
    rscale_est = "ultrawide", rscale_bf = "ultrawide",
    iterations = 10000, map_density_n = 512, detailed = FALSE, fullbayes = FALSE,
    ...
){

  method <- method[1]
  alternative <- alternative[1]
  ci <- ci[1]
  triangle <- triangle[1]

  if(fullbayes){
    if(ci == "freq") ci <- "bayes_hdi"
    if(!pd & !bf){
      bf <- TRUE
      pd <- TRUE
    }
  }

  var.label <- colnames(dat)
  d.varnames <- data.frame(
    varname = var.label
  ) %>%
    dplyr::mutate(num = 1:dplyr::n())

  nvar <- ncol(dat)

  out <- list(
    row = 1:nvar,
    col = 1:nvar
  ) %>%
    expand.grid() %>%
    dplyr::filter(col > row) %>%
    dplyr::arrange(row, col) %>%
    dplyr::mutate(
      cor = NA_real_,
      cor_EAP = NA_real_,
      cor_MAP = NA_real_,
      cor_MED = NA_real_,
      lower = NA_real_,
      upper = NA_real_,
      t = NA_real_,
      df = NA_real_,
      S = NA_real_,
      z = NA_real_,
      p = NA_real_,
      sig = NA_character_,
      pd = NA_real_,
      BF10 = NA_real_,
      log10_BF10 = NA_real_,
      favor = NA_character_,
      evidence = NA_character_,
      n_pair = NA_real_,
      n_na = NA_real_
    )


  for(i in 1:ncol(dat)){
    for(j in 1:ncol(dat)){
      if(j > i){
        interval <- c(NA_real_, NA_real_)
        mat <- dat[,c(i,j)] %>%
          tidyr::drop_na() %>% # Pair-wise exclusion
          as.matrix()

        out[out$row == i & out$col == j,]$n_pair <- nrow(mat)
        out[out$row == i & out$col == j,]$n_na <- nrow(dat) - nrow(mat)

        # Frequentist

        if(cor){
          list_cortest <- cor.test(
            mat[,1], mat[,2],
            alternative = alternative, method = method,
            exact = exact, conf.level = conf.level, continuity = continuity
          )

          out[out$row == i & out$col == j,]$cor <- list_cortest$estimate
          out[out$row == i & out$col == j,]$p <- list_cortest$p.value

          if(method == "pearson"){
            out[out$row == i & out$col == j,]$t <- list_cortest$statistic
            out[out$row == i & out$col == j,]$df <- list_cortest$parameter

            if(ci == "freq"){
              interval <- list_cortest$conf.int
            }
          }

          if(method == "spearman"){
            out[out$row == i & out$col == j,]$S <- list_cortest$statistic
          }

          if(method == "kendall"){
            out[out$row == i & out$col == j,]$z <- list_cortest$statistic
          }

        }

        # Bayesian estimation

        if(cor_EAP|cor_MAP|cor_MED|pd|ci %in% c("bayes_central", "bayes_hdi")){

          suppressMessages(
            mcmcsample <- BayesFactor::correlationBF(
              mat[,1], mat[,2], rscale = rscale_est,
              posterior = T, iterations = iterations,
              progress = F
            )
          )

          if(cor_EAP){
            out[out$row == i & out$col == j,]$cor_EAP = mean(mcmcsample[,"rho"])
          }

          if(cor_MAP){
            out[out$row == i & out$col == j,]$cor_MAP = mode_stat(mcmcsample[,"rho"], map_density_n)
          }
          if(cor_MED){
            out[out$row == i & out$col == j,]$cor_MED = median(mcmcsample[,"rho"])
          }

          if(pd){
            out[out$row == i & out$col == j,]$pd = pdir(mcmcsample[,"rho"], na.rm = T)
          }

          if(ci == "bayes_central"){
            interval <- c(
              quantile(mcmcsample[,"rho"], (1 - conf.level)/2),
              quantile(mcmcsample[,"rho"], (1 - conf.level)/2 + conf.level)
            )
          }
          if(ci == "bayes_hdi"){
            interval <- hdi(mcmcsample[,"rho"], prob = conf.level)
          }


        }

        out[out$row == i & out$col == j,]$lower <- interval[1]
        out[out$row == i & out$col == j,]$upper <- interval[2]

        # Bayes factor

        if(bf){
          suppressMessages(
            bf_est <- BayesFactor::correlationBF(
              mat[,1], mat[,2], rscale = rscale_bf
            )
          )

          out[out$row == i & out$col == j,]$BF10 <- exp(bf_est@bayesFactor$bf)
        }

      }
    }
  }

  out$sig <- dplyr::if_else(out$p < alpha, "*", "ns")


  if(fullbayes){
    out <- out %>%
      dplyr::mutate(
        t = NA_real_, df = NA_real_, p = NA_real_, sig = NA_character_
      )
  }

  if(bf){
    out <- out %>%
      dplyr::mutate(
        log10_BF10 = log10(BF10),
        favor = dplyr::if_else(BF10 > 1, "alt.", "null"),
        evidence = "anecdotal",
        evidence = dplyr::if_else(abs(log10_BF10) > log(3, 10), "moderate", evidence),
        evidence = dplyr::if_else(abs(log10_BF10) > log(10, 10), "strong", evidence),
        evidence = dplyr::if_else(abs(log10_BF10) > log(30, 10), "very strong", evidence),
        evidence = dplyr::if_else(abs(log10_BF10) > log(100, 10), "extreme", evidence)
      )
  }

  if(!detailed){
    out <- out %>%
      dplyr::transmute(
        row = row,
        col = col,
        cor = cor %>% round(3),
        cor_EAP = cor_EAP %>% round(3),
        cor_MAP = cor_MAP %>% round(3),
        cor_MED = cor_MED %>% round(3),
        lower = lower %>% round(3),
        upper = upper %>% round(3),
        t = t %>% round(2),
        df = df,
        p = p %>% round(3),
        sig = sig,
        pd = pd %>% round(3),
        BF10 = BF10 %>% round(3),
        log10_BF10 = log10_BF10,
        favor = favor,
        evidence = evidence,
        n_pair = n_pair
      )
  }

  out <- out %>%
    select_if(~ !any(is.na(.))) %>%
    dplyr::left_join(d.varnames, by = dplyr::join_by(x$row == y$num)) %>%
    dplyr::rename(var_row = "varname") %>%
    dplyr::left_join(d.varnames, by = dplyr::join_by(x$col == y$num)) %>%
    dplyr::rename(var_col = "varname") %>%
    dplyr::select("var_row", "var_col", dplyr::everything())

  # Making table formats
  out_with_table <- list()
  out_with_table[[1]] <- out
  mattmp <- matrix(nrow = ncol(dat), ncol = ncol(dat))
  colnames(mattmp) <- var.label
  rownames(mattmp) <- var.label

  out <- out[, setdiff(names(out), c("sig", "favor", "evidence"))]

  for(i in 2:(ncol(out)-4+1)) out_with_table[[i]] <- mattmp

  names(out_with_table) <- c("all", paste0("table_", colnames(out)[-(1:4)]))

  for(k in 2:(length(out_with_table))){
    for(i in 1:ncol(dat)){
      for(j in 1:ncol(dat)){
        if(j > i){
          if(triangle %in% c("upper", "full")){
            out_with_table[[k]][i,j] <- as.numeric(out[out$row == i & out$col == j,][k+3])
          }
          if(triangle %in% c("lower", "full")){
            out_with_table[[k]][j,i] <- as.numeric(out[out$row == i & out$col == j,][k+3])
          }
        }
      }
    }
    if(triangle == "full" & grepl("cor", names(out_with_table)[k])){
      diag(out_with_table[[k]]) <- 1
    }
  }

  return(out_with_table)
}
