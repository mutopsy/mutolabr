#' Perform Partial Correlation Tests with Frequentist Methods
#'
#' Computes pairwise partial correlations among all variables in a given dataset,
#' controlling for one or more covariates using frequentist methods.
#' The function returns correlation estimates along with confidence intervals.
#' Note: Bayesian methods are not supported in the current version.

#'
#' @param dat A data frame or matrix containing numeric variables of interest.
#' @param control A data frame, matrix, or numeric vector containing covariates to control for.
#' @param cor Logical. If `TRUE`, computes frequentist correlation coefficients (default: `TRUE`).
#' @param triangle Character. Specifies which part of the correlation matrix to return:
#' `"upper"`, `"lower"`, or `"full"` (default: `"upper"`).
#' @param alternative Character. Specifies the alternative hypothesis for the frequentist test:
#' `"two.sided"`, `"less"`, or `"greater"` (default: `"two.sided"`).
#' @param method Character. Specifies the correlation method for the frequentist test:
#' `"pearson"`, `"kendall"`, or `"spearman"` (default: `"pearson"`).
#' @param conf.level Numeric. The confidence level for confidence intervals (default: `0.95`).
#'
#' @return A list containing:
#' \describe{
#'   \item{all}{A data frame with all computed correlation statistics.}
#'   \item{table_XX}{A data frame corresponding to a table named "table_XX",
#'   where "XX" is derived from the output variables (e.g., `"table_cor"`, `"table_p"`).
#'   The content of the table depends on the provided inputs.}
#' }
#'
#' @examples
#' results <- pcor_test_all(mtcars[, 1:3], control = mtcars[, 4:5])
#' results$all  # View detailed results in a tidy format
#' results$table_cor  # View partial correlation matrix
#'
#' @import stats
#' @import dplyr
#' @import tidyr
#' @import ppcor
#' @export

pcor_test_all <- function(
    dat,
    control = NULL,
    triangle = c("upper", "lower", "full"),
    alternative = c("two.sided", "less", "greater"),
    method = c("pearson", "kendall", "spearman"),
    conf.level = 0.95,
    ...
){

  cor <- TRUE
  ci <- "freq"

  if(sum(is.na(control)) != 0) {
    stop("Covariates contain NA values!")
  }

  method <- method[1]
  alternative <- alternative[1]
  triangle <- triangle[1]

  var.label <- colnames(dat)
  d.varnames <- data.frame(
    varname = var.label
  ) %>%
    dplyr::mutate(num = 1:dplyr::n())

  nvar <- ncol(dat)

  if(is.null(control)){
    k <- 0
  } else{
    control <- as.data.frame(control)
    k <- ncol(control)
  }

  out <- list(
    row = 1:nvar,
    col = 1:nvar
  ) %>%
    expand.grid() %>%
    dplyr::filter(col > row) %>%
    dplyr::arrange(row, col) %>%
    dplyr::mutate(
      cor = NA_real_,
      lower = NA_real_,
      upper = NA_real_,
      t = NA_real_,
      df = NA_real_,
      S = NA_real_,
      z = NA_real_,
      p = NA_real_,
      n_pair = NA_real_,
      n_na = NA_real_
    )

  interval <- c(NA_real_, NA_real_)

  for(i in 1:ncol(dat)){
    for(j in 1:ncol(dat)){
      if(j > i){
        index_valid <- (dat[,c(i,j)] %>% is.na() %>% rowSums()) == 0
        mat <- dat[,c(i,j)] %>%
          tidyr::drop_na() %>% # Pair-wise exclusion
          as.matrix()
        if(k==1){
          z <- control[index_valid,1]
        } else if(k >= 2){
          z <- control[index_valid,]
        }

        out[out$row == i & out$col == j,]$n_pair <- nrow(mat)
        out[out$row == i & out$col == j,]$n_na <- nrow(dat) - nrow(mat)

        n <- out[out$row == i & out$col == j,]$n_pair

        # Frequentist

        if(cor){
          if(!is.null(control)){
            list_cortest <- ppcor::pcor.test(
              x = mat[,1], y = mat[,2], z = z,
              method = method
            )

            t <- list_cortest$statistic

            if(ci == "freq" & method == "pearson"){
              if(alternative == "less"){
                p <- pt(t, n - k - 2, 0)
              } else{
                if(alternative == "greater"){
                  p <- 1 - pt(t, n - k - 2, 0)
                } else{
                p <- (1 - pt(abs(t), n - k - 2, 0))*2
                }
              }
              out[out$row == i & out$col == j,]$p <- p
            }

          } else{
            list_cortest <- cor.test(
              x = mat[,1], y = mat[,2],
              alternative = alternative, method = method
            )

            out[out$row == i & out$col == j,]$p <- list_cortest$p.value
          }

          out[out$row == i & out$col == j,]$cor <- list_cortest$estimate


          if(method == "pearson"){
            out[out$row == i & out$col == j,]$t <- list_cortest$statistic
            out[out$row == i & out$col == j,]$df <- n - k - 2

            if(ci == "freq"){
              r <- out[out$row == i & out$col == j,]$cor
              z <- 0.5 * log((1 + r) / (1 - r))
              se <- 1 / sqrt(n - k - 3)

              if(alternative == "less"){
                z_crit <- qnorm(1 - (1 - conf.level))
                z_upper <- z + z_crit * se
                r_lower <- -1
                r_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
              } else if(alternative == "greater"){
                z_crit <- qnorm(1 - (1 - conf.level))
                z_lower <- z - z_crit * se
                r_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
                r_upper <- 1
              } else{
                z_crit <- qnorm(1 - (1 - conf.level)/2)
                z_lower <- z - z_crit * se
                z_upper <- z + z_crit * se
                r_lower <- (exp(2 * z_lower) - 1) / (exp(2 * z_lower) + 1)
                r_upper <- (exp(2 * z_upper) - 1) / (exp(2 * z_upper) + 1)
              }

              interval <- c(lower = r_lower, upper = r_upper)
            }
          }

          if(method == "spearman"){
            if(is.null(control)){
              out[out$row == i & out$col == j,]$S <- list_cortest$statistic
            } else{
              out[out$row == i & out$col == j,]$t <- list_cortest$statistic
            }

          }

          if(method == "kendall"){
            if(is.null(control)){
              out[out$row == i & out$col == j,]$z <- list_cortest$statistic
            } else{
              out[out$row == i & out$col == j,]$t <- list_cortest$statistic
            }
          }
        }

        # Bayesian estimation

        ## Under development...


        out[out$row == i & out$col == j,]$lower <- interval[1]
        out[out$row == i & out$col == j,]$upper <- interval[2]

        # Bayes factor

        ## Under development...

      }
    }
  }

  out <- out %>%
    select_if(~ !any(is.na(.))) %>%
    left_join(d.varnames, by = join_by(x$row == y$num)) %>%
    dplyr::rename(var_row = "varname") %>%
    left_join(d.varnames, by = join_by(x$col == y$num)) %>%
    dplyr::rename(var_col = "varname") %>%
    dplyr::select("var_row", "var_col", everything())

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
