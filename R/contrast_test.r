#' Perform Contrast Test
#'
#' This function performs a contrast test on a given dataset to compare the means of multiple variables using weighted contrasts.
#' It calculates several statistics, including the t-value, F-value, p-value, confidence intervals, and effect sizes (Cohen's f and partial eta squared).
#'
#' @param dat A data frame or tibble containing the data to be analyzed. Each column represents a variable. For unbalanced data, please fill missing values with NA.
#' @param weight A numeric vector of weights to apply to each variable in the contrast. The sum of the weights must be zero.
#' @param paired A logical indicating whether the data is paired (TRUE) or unpaired (FALSE). Default is FALSE.
#' @param alternative A character string specifying the type of test. Possible values are "two.sided" (default), "less", or "greater".
#' @param conf.level A numeric value indicating the confidence level for the confidence intervals. Default is 0.95.
#' @param verbose Logical. If `TRUE`, prints additional messages.
#'
#' @return A data frame containing the following columns:
#' \describe{
#'   \item{weights}{The weights applied to each variable in the contrast.}
#'   \item{estimate}{The weighted contrast estimate.}
#'   \item{lower}{The lower bound of the confidence interval.}
#'   \item{upper}{The upper bound of the confidence interval.}
#'   \item{t}{The t-value for the contrast.}
#'   \item{F}{The F-value for the contrast.}
#'   \item{df_error}{The degrees of freedom for the error.}
#'   \item{SS_contrast}{The sum of squares for the contrast component.}
#'   \item{SS_error}{The sum of squares for the error.}
#'   \item{SS_effect}{The sum of squares for the effect.}
#'   \item{SS_total}{The toal sum of squares for the data.}
#'   \item{p}{The p-value for the contrast test.}
#'   \item{eta2}{The eta squared effect size.}
#'   \item{peta2}{The partial eta squared effect size.}
#'   \item{cohens_f}{The Cohen's f effect size.}
#'   \item{n_total}{The total number of observations in the unpaired data.}
#'   \item{n_total_na}{The number of missing values in the unpaired data.}
#'   \item{n_pair}{The number of non-missing pairs in the paired data.}
#'   \item{n_pair_na}{The number of missing pairs in the paired data.}
#' }
#'
#' @details
#' This function performs a contrast analysis on a given dataset using the provided weights. It computes statistics such as the t-value, F-value, confidence intervals, and effect sizes (Cohen's f and partial eta squared).
#' If the data is paired, pair-wise exclusion is applied. The function supports two-tailed and one-tailed tests depending on the value of the `alternative` parameter.
#'
#' @seealso
#' [`trend_test()`] for trend analysis using contrast tests.
#'
#' @examples
#' # Example of linear trend analysis with unpaired data
#' dat <- data.frame(A = rnorm(100), B = rnorm(100), C = rnorm(100))
#' weight <- c(1, -1, 0)
#' contrast_test(dat, weight)
#'
#' # Example with paired data
#' dat <- data.frame(A = rnorm(50), B = rnorm(50), C = rnorm(50))
#' weight <- c(1, -2, 1)
#' contrast_test(dat, weight, paired = TRUE)
#'
#' @import dplyr
#' @import tidyr
#' @export

contrast_test <- function(
    dat,
    weight = NULL, paired = FALSE,
    alternative = c("two.sided", "less", "greater"),
    conf.level = 0.95, verbose = TRUE
){

  alternative <- alternative[1]
  nvar <- ncol(dat)
  alpha <- 1 - conf.level

  if(abs(sum(weight)) > 0.00001){
    stop("The sum of the weights must be zero!")
  }

  if(length(weight) != nvar){
    stop("The number of weights does not match the number of variables!")
  }

  # Data frame for results
  out <- data.frame(
    weights = paste0(weight, collapse = ","),
    estimate = NA_real_,
    lower = NA_real_,
    upper = NA_real_,
    t = NA_real_,
    F = NA_real_,
    df_error = NA_real_,
    SS_contrast = NA_real_,
    SS_error = NA_real_,
    SS_total = NA_real_,
    SS_effect = NA_real_,
    p = NA_real_,
    peta2 = NA_real_,
    cohens_f = NA_real_,
    n_total = NA_real_,
    n_total_na = NA_real_,
    n_pair = NA_real_,
    n_pair_na = NA_real_

  )

  # NA omission

  if(paired){
    dat2 <- dat %>%
      drop_na() # Pair-wise exclusion
    out$n_pair <- nrow(dat2)
    out$n_pair_na <- nrow(dat) - nrow(dat2)
    dat <- dat2
  } else{
    out$n_total_na <- sum(is.na(dat))
    out$n_total <- sum(!is.na(dat))
  }

  # Making tidy data

  dat_long <- dat %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    tidyr::pivot_longer(-id, names_to = "variable", values_to = "value") %>%
    tidyr::drop_na() %>%
    dplyr::group_by(variable) %>%
    dplyr::mutate(mean_group = mean(value)) %>%
    dplyr::ungroup()

  # Calculation of errors

  if(paired){
    dat_long <- dat_long %>%
      dplyr::group_by(id) %>%
      dplyr::mutate(mean_id = mean(value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(error = value - mean_group - mean_id + mean(value))
  } else{
    dat_long <- dat_long %>%
      dplyr::mutate(error = value - mean_group)
  }

  # Calculation of df

  if(paired){
    df <- (out$n_pair - 1)*(nvar - 1)
  } else{
    df <- out$n_total - nvar
  }

  # Calculation of SS and MS of errors

  SS_e <- sum(dat_long$error^2)
  MS_e <- SS_e / df
  S <- sqrt(MS_e)

  # Calculation of SS_total and SS_effect
  SS_total <- sum((dat_long$value - mean(dat_long$value))^2)
  SS_effect <- SS_total - SS_e

  # Summary statistics
  smry <- dat_long %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean = mean(value, na.rm = T)
    ) %>%
    dplyr::mutate(weight = weight)

  # Calculation of SS and MS of contrast

  C <- sum(smry$mean * smry$weight)
  SS_C <- C^2 / sum(smry$weight^2 / smry$n)
  MS_C <- SS_C / 1

  # t-test

  SE_C <- S * sqrt(sum(smry$weight^2 / smry$n))
  t <- C/SE_C

  if(alternative == "two.sided"){
    p <- 2 * (1 - pt(abs(t), df, 0))

    t_crit <- qt(1 - alpha / 2, df)
    lower <- C - t_crit * SE_C
    upper <- C + t_crit * SE_C

  } else if(alternative == "less"){
    p <- pt(t, df, 0)

    t_crit <- qt(1 - alpha, df)
    lower <- -Inf
    upper <- C + t_crit * SE_C

  } else if(alternative == "greater"){
    p <- 1 - pt(t, df, 0)

    t_crit <- qt(1 - alpha, df)
    lower <- C - t_crit * SE_C
    upper <- Inf

  }

  out$estimate <- C
  out$t <- t
  out$df_error <- df
  out$p <- p
  out$lower <- lower
  out$upper <- upper

  # Calculation of F statistics

  out$F <- MS_C/MS_e
  out$eta2 <- SS_C / (SS_total)
  out$peta2 <- SS_C / (SS_C + SS_e)
  out$cohens_f <- sqrt(SS_C / SS_e)

  out$SS_contrast <- SS_C
  out$SS_error <- SS_e
  out$SS_total <- SS_total
  out$SS_effect <- SS_effect

  # out$p_F <- 1-pf(out$F, 1, df) # for check

  out <- out %>%
    dplyr::select_if(~ !any(is.na(.)))

  if(verbose){
    cat("\n-----------------------------------------------\n")
    if(paired){
      cat("Contrast analysis for paired samples")
    } else{
      cat("Contrast analysis for independent samples")
    }
    cat("\n-----------------------------------------------\n")
    cat(paste0("Mean = (", paste0(format(smry$mean, digit = 4, nsmall = 2), collapse = ", "), ")\n"))
    if(paired){
      cat(paste0("N = ", nrow(dat), "\n"))
    } else{
      cat(paste0("n = (", paste0(smry$n, collapse = ", "), ")\n"))
    }
    cat(paste0("Weight = (", paste0(round(smry$weight,4), collapse = ", "), ")\n"))
    cat("-----------------------------------------------\n")
    cat(paste0("Contrast = ", format(C, digit = 4, nsmall = 2), "\n"))
    cat(paste0("CI = [", format(out$lower, digit = 4, nsmall = 2), ", ", format(out$upper, digit = 4, nsmall = 2), "]\n"))
    cat("-----------------------------------------------\n")
    cat(paste0("t = ", format(out$t, digit = 4, nsmall = 2), ", df = ", out$df_error, ", p = ", format(out$p, nsmall = 3), "\n(F = ", format(out$F, digit = 4, nsmall = 2), ")\n"))
    cat("-----------------------------------------------\n")
    cat(paste0("SS_contrast = ", format(out$SS_contrast, digit = 4, nsmall = 2), ", SS_error = ", format(out$SS_error, digit = 4, nsmall = 2), "\n"))
    cat(paste0("SS_total = ", format(out$SS_total, digit = 4, nsmall = 2), ", SS_effect = ", format(out$SS_effect, digit = 4, nsmall = 2), "\n"))
    cat("-----------------------------------------------\n")
    cat(paste0("eta squared = ", format(out$eta2, digit = 4, nsmall = 2), "\n"))
    cat(paste0("partial eta squared = ", format(out$peta2, digit = 4, nsmall = 2), ", Cohen's f = ", format(out$cohens_f, digit = 4, nsmall = 2), "\n"))
    cat("-----------------------------------------------\n")
    cat("Note:\nThe CI represents the frequentist confidence interval.")
    cat("\n(Confidence level = ",conf.level,")\n", sep = "")

  }

  return(invisible(out))
}
