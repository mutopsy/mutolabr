#' Perform Correlation Tests with Frequentist and Bayesian Methods
#'
#' This function computes pairwise correlations for all variables in a given dataset.
#' It supports both frequentist and Bayesian approaches, providing multiple correlation estimates,
#' frequentist confidence intervals, Bayesian credible intervals based on posterior distributions,
#' probability of direction (pd), and Bayes factors.
#'
#' @param dat A data frame or matrix containing numeric variables for correlation analysis.
#' @param cor Logical. If `TRUE`, compute frequentist correlation coefficients. Default is `TRUE`.
#' @param cor_EAP Logical. If `TRUE`, compute the Expected A Posteriori (EAP) estimate of the correlation coefficient. Default is `FALSE`.
#' @param cor_MAP Logical. If `TRUE`, compute the Maximum A Posteriori (MAP) estimate of the correlation coefficient. Default is `FALSE`.
#' @param cor_MED Logical. If `TRUE`, compute the median of the posterior distribution of the correlation coefficient. Default is `FALSE`.
#' @param pd Logical. If `TRUE`, compute the probability of direction (pd) based on posterior distributions. Default is `FALSE`.
#' @param bf Logical. If `TRUE`, compute Bayes factors favoring the presence of correlation over the absence of correlation. Default is `FALSE`.
#' @param ci Character. Specifies the type of confidence or credible interval: `"freq"` (frequentist confidence interval),
#' `"bayes_central"` (Bayesian central credible interval), or `"bayes_hdi"` (highest density interval based on the posterior distribution). Default is `"freq"`.
#' @param triangle Character. Specifies which part of the correlation matrix to return: `"upper"`, `"lower"`, or `"full"`. Default is `"upper"`.
#' @param alternative Character. Specifies the alternative hypothesis for frequentist analysis: `"two.sided"`, `"less"`, or `"greater"`. Default is `"two.sided"`.
#' @param method Character. Specifies the correlation method for frequentist analysis: `"pearson"`, `"kendall"`, or `"spearman"`. Default is `"pearson"`.
#' @param exact Logical or `NULL`. If `TRUE`, computes exact p-values for small sample sizes when using Spearman or Kendall correlations.
#' @param conf.level Numeric. Confidence level for frequentist confidence intervals or credibility levels for Bayesian credible intervals. Default is `0.95`.
#' @param continuity Logical. If `TRUE`, applies a continuity correction for Kendall correlations. Default is `FALSE`.
#' @param rscale_est Numeric or character. Scale of the Cauchy prior for Bayesian estimation of the posterior distribution. Options: `"ultrawide"`, `"wide"`, `"medium"`, or a positive real number. Default is `"ultrawide"`. This argument is passed to `BayesFactor::correlationBF()`.
#' @param rscale_bf Numeric or character. Scale of the Cauchy prior for Bayes factor calculation. Options: `"ultrawide"`, `"wide"`, `"medium"`, or a positive real number. Default is `"ultrawide"`. This argument is passed to `BayesFactor::correlationBF()`.
#' @param iterations Integer. Number of samples to draw from the posterior distribution for Bayesian estimation. Default is `10000`.
#' @param ... Additional arguments passed to `cor.test()`.
#'
#' @return A list containing:
#' \describe{
#'   \item{all}{A data frame with all computed correlation statistics.}
#'   \item{table_XX}{A data frame corresponding to a table named "table_XX",
#'   where "XX" is derived from the output variables (e.g., "table_cor",
#'   "table_p", "table_BF10"). The content of the table depends on the provided inputs.}
#' }
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' results <- cor_test_all(mtcars)
#' results$all  # View detailed correlation statistics
#' results$table_cor  # View correlation matrix
#' }
#'
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import BayesFactor
#' @export

cor_test_all <- function(
    dat,
    cor = T, cor_EAP = F, cor_MAP = F, cor_MED = F, pd = F, bf = F,
    ci = c("freq","bayes_central",  "bayes_hdi"),
    triangle = c("upper", "lower", "full"),
    alternative = c("two.sided", "less", "greater"),
    method = c("pearson", "kendall", "spearman"),
    exact = NULL, conf.level = 0.95, continuity = FALSE,
    rscale_est = "ultrawide", rscale_bf = "ultrawide",
    iterations = 10000,
    ...
){

  library(tidyverse)
  method <- method[1]
  alternative <- alternative[1]
  ci <- ci[1]
  triangle <- triangle[1]

  var.label <- colnames(dat)
  d.varnames <- tibble(
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
      T = NA_real_,
      p = NA_real_,
      pd = NA_real_,
      BF10 = NA_real_,
      n_pair = NA_real_,
      n_na = NA_real_
    )

  interval <- c(NA_real_, NA_real_)

  for(i in 1:ncol(dat)){
    for(j in 1:ncol(dat)){
      if(j > i){
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
            out[out$row == i & out$col == j,]$T <- list_cortest$statistic
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
            map_dens <- function(z) density(z)$x[which.max(density(z)$y)]
            out[out$row == i & out$col == j,]$cor_MAP = map_dens(mcmcsample[,"rho"])
          }
          if(cor_MED){
            out[out$row == i & out$col == j,]$cor_MED = median(mcmcsample[,"rho"])
          }

          if(pd){
            pd_posterior <- function(x){
              out <- mean(x > 0)
              if(out < 0.5) out <- 1 - out
              return(out)
            }

            out[out$row == i & out$col == j,]$pd = pd_posterior(mcmcsample[,"rho"])
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

  out <- out %>%
    select_if(~ !any(is.na(.))) %>%
    left_join(d.varnames, by = join_by(row == num)) %>%
    dplyr::rename(var_row = varname) %>%
    left_join(d.varnames, by = join_by(col == num)) %>%
    dplyr::rename(var_col = varname) %>%
    dplyr::select(var_row, var_col, everything())

  # Making table formats
  out_with_table <- list()
  out_with_table[[1]] <- out
  mattmp <- matrix(nrow = ncol(dat), ncol = ncol(dat))
  colnames(mattmp) <- var.label
  rownames(mattmp) <- var.label

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
