#' Perform Frequentist and Bayesian t-tests with Effect Size Estimation
#'
#' This function performs frequentist and Bayesian t-tests, with options for effect size estimation,
#' confidence intervals, Bayesian credible intervals, probability of direction (pd), and Bayes factors.
#'
#' @param x A numeric vector representing the first sample.
#' @param y An optional numeric vector representing the second sample. If `NULL`, a one-sample t-test is performed (default: `NULL`).
#' @param var.label A character vector of length two specifying labels for `x` and `y`.
#' @param paired Logical. If `TRUE`, a paired t-test is performed (default: `FALSE`).
#' @param var.equal Logical. If `TRUE`, the two-sample t-test assumes equal variances (default: `FALSE`).
#' @param mu A numeric value specifying the null hypothesis mean difference.
#' @param ci Character. Specifies the type of confidence or credible interval: `"freq"` (frequentist confidence interval),
#' `"bayes_central"` (Bayesian central credible interval), or `"bayes_hdi"` (highest density interval based on the posterior distribution) (default: `"freq"`).
#' @param alternative Character. Specifies the alternative hypothesis for the frequentist test: `"two.sided"`, `"less"`, or `"greater"` (default: `"two.sided"`).
#' @param conf.level Numeric. The confidence level for frequentist intervals or credibility level for Bayesian intervals (default: `0.95`).
#' @param alpha Numeric. Significance level. Defaults to 0.05.
#' @param pd Logical. If `TRUE`, computes the probability of direction (pd) based on posterior distributions (default: `FALSE`).
#' @param bf Logical. If `TRUE`, computes Bayes factors for the presence of a difference versus the null hypothesis (default: `FALSE`).
#' @param cor Logical. If `TRUE`, computes Pearson correlation for paired samples.
#' @param mean_x_EAP Logical. If `TRUE`, computes the expected a posteriori (EAP) estimate of the mean of `x` (default: `FALSE`).
#' @param mean_x_MAP Logical. If `TRUE`, computes the maximum a posteriori (MAP) estimate of the mean of `x` (default: `FALSE`).
#' @param mean_x_MED Logical. If `TRUE`, computes the median of the posterior distribution (MED) for the mean of `x` (default: `FALSE`).
#' @param diff_EAP Logical. If `TRUE`, computes the expected a posteriori (EAP) estimate of the mean difference (default: `FALSE`).
#' @param diff_MAP Logical. If `TRUE`, computes the maximum a posteriori (MAP) estimate of the mean difference (default: `FALSE`).
#' @param diff_MED Logical. If `TRUE`, computes the median of the posterior distribution (MED) for the mean difference (default: `FALSE`).
#' @param cohens_d Logical. If `TRUE`, computes Cohen's d for independent samples.
#' @param cohens_d_EAP Logical. If `TRUE`, computes the expected a posteriori (EAP) estimate of Cohen's d (default: `FALSE`).
#' @param cohens_d_MAP Logical. If `TRUE`, computes the maximum a posteriori (MAP) estimate of Cohen's d (default: `FALSE`).
#' @param cohens_d_MED Logical. If `TRUE`, computes the median of the posterior distribution (MED) for Cohen's d (default: `FALSE`).
#' @param cohens_dz Logical. If `TRUE`, computes Cohen's dz for paired samples.
#' @param cohens_dz_EAP Logical. If `TRUE`, computes the expected a posteriori (EAP) estimate of Cohen's dz (default: `FALSE`).
#' @param cohens_dz_MAP Logical. If `TRUE`, computes the maximum a posteriori (MAP) estimate of Cohen's dz (default: `FALSE`).
#' @param cohens_dz_MED Logical. If `TRUE`, computes the median of the posterior distribution (MED) for Cohen's dz (default: `FALSE`).
#' @param rscale_est Numeric or character. Specifies the Cauchy prior scale for Bayesian estimation of the posterior distribution.
#' Options: `"ultrawide"`, `"wide"`, `"medium"`, or a positive real number (default: `Inf`). Passed to `BayesFactor::ttestBF()`.
#' @param rscale_bf Numeric or character. Specifies the Cauchy prior scale for Bayes factor calculation.
#' Options: `"ultrawide"`, `"wide"`, `"medium"`, or a positive real number (default: `"medium"`). Passed to `BayesFactor::ttestBF()`.
#' @param iterations Integer. Number of MCMC iterations for Bayesian estimation.
#' @param map_density_n Integer. Number of bins for MAP density estimation.
#' @param verbose Logical. If `TRUE`, prints additional messages.
#' @param detailed Logical. Whether to return detailed results (\code{TRUE}) or
#'   minimal output (\code{FALSE}, default).
#' @param fullbayes Logical. Whether to show only Bayesian results (\code{TRUE}) or both frequentist and Bayesian results (\code{FALSE}, default).
#'
#' @return A data frame containing test statistics, effect sizes, confidence intervals, and Bayesian estimates.
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(30,0, 1)
#' y <- x + rnorm(30,0.5, 1)
#'
#' # Welch's t-test with effect size estimation
#' t_test_all(x, y)
#'
#' # Welch's t-test with Bayesian independent t-test (Bayes factor and pd)
#' # and 95% highest density intervals
#' t_test_all(x, y, diff_MAP = TRUE, cohens_d_MAP = TRUE,
#'            bf = TRUE, pd = TRUE, ci = "bayes_hdi", rscale_bf = "medium")
#'
#' # Paired t-test with effect size estimation
#' t_test_all(x, y, paired = TRUE)
#'
#' # Paired t-test with Bayesian paired t-test (Bayes factor and pd)
#' # and 95% central credible intervals
#' t_test_all(x, y, paired = TRUE, diff_MAP = TRUE, cohens_dz_MAP = TRUE,
#'            bf = TRUE, pd = TRUE, ci = "bayes_central", rscale_bf = "medium")
#'
#' @importFrom effectsize cohens_d repeated_measures_d
#' @importFrom BayesFactor ttestBF
#' @importFrom dplyr mutate transmute select_if if_else %>%
#' @importFrom tidyr drop_na
#' @export

t_test_all <- function(
    x, y = NULL, var.label = c("x", "y"), paired = F, var.equal = FALSE, mu = 0,
    ci = c("freq","bayes_central",  "bayes_hdi"),
    alternative = c("two.sided", "less", "greater"),
    conf.level = 0.95, alpha = 0.05,
    pd = FALSE, bf = FALSE, cor = TRUE,
    mean_x_EAP = FALSE, mean_x_MAP = FALSE, mean_x_MED = FALSE,
    diff_EAP = FALSE, diff_MAP = FALSE, diff_MED = FALSE,
    cohens_d = NULL, cohens_d_EAP = FALSE, cohens_d_MAP = FALSE, cohens_d_MED = FALSE,
    cohens_dz = TRUE, cohens_dz_EAP = FALSE, cohens_dz_MAP = FALSE, cohens_dz_MED = FALSE,
    rscale_est = Inf, rscale_bf = "medium",
    iterations = 10000, map_density_n = 512, verbose = TRUE, detailed = FALSE, fullbayes = FALSE
){

  # initialization

  alternative <- alternative[1]
  ci <- ci[1]

  if(fullbayes){
    if(ci == "freq") ci <- "bayes_hdi"
    if(!pd & !bf){
      bf <- TRUE
      pd <- TRUE
    }
  }

  if(is.null(y)){
    paired <- FALSE
    var.equal <- FALSE
    cor <- FALSE
    var.label[2] <- NA
  } else if(paired){
    var.equal <- FALSE
  } else{
    cor <- FALSE
  }

  if(is.null(cohens_d)){
    if(!is.null(y) & paired == T){
      cohens_d <- F
    } else{
      cohens_d <- T
    }
  }

  # Data frame for results
  out <- data.frame(
    x = var.label[1],
    y = var.label[2],
    mean_x = NA_real_,
    mean_x_EAP = NA_real_,
    mean_x_MAP = NA_real_,
    mean_x_MED = NA_real_,
    mean_x_lower = NA_real_,
    mean_x_upper = NA_real_,
    mean_y = NA_real_,
    diff = NA_real_,
    diff_EAP = NA_real_,
    diff_MAP = NA_real_,
    diff_MED = NA_real_,
    diff_lower = NA_real_,
    diff_upper = NA_real_,
    t = NA_real_,
    df = NA_real_,
    p = NA_real_,
    alpha = alpha,
    sig = NA_character_,
    cor_xy = NA_real_,
    cohens_d = NA_real_,
    cohens_d_EAP = NA_real_,
    cohens_d_MAP = NA_real_,
    cohens_d_MED = NA_real_,
    cohens_d_lower = NA_real_,
    cohens_d_upper = NA_real_,
    cohens_dz = NA_real_,
    cohens_dz_EAP = NA_real_,
    cohens_dz_MAP = NA_real_,
    cohens_dz_MED = NA_real_,
    cohens_dz_lower = NA_real_,
    cohens_dz_upper = NA_real_,
    pd = NA_real_,
    BF10 = NA_real_,
    log10_BF10 = NA_real_,
    favor = NA_character_,
    evidence = NA_character_,
    n_x = NA_real_,
    n_y =  NA_real_,
    n_pair = NA_real_,
    n_na_x = NA_real_,
    n_na_y = NA_real_,
    n_na = NA_real_
  )

  # NA omission

  if(paired){
    tmp <- length(x)
    d2 <- data.frame(x = x, y = y) %>%
      tidyr::drop_na() # Pair-wise exclusion
    x <- d2$x
    y <- d2$y
    out$n_pair <- nrow(d2)
    out$n_na <- tmp - nrow(d2)
  } else if(is.null(y)){
    out$n_na_x <- sum(is.na(x))
    x <- x[!is.na(x)]
    out$n_x <- length(x)
  } else {
    out$n_na_x <- sum(is.na(x))
    out$n_na_y <- sum(is.na(y))
    x <- x[!is.na(x)]
    y <- y[!is.na(y)]
    out$n_x <- length(x)
    out$n_y <- length(y)
  }

  # Frequentist

  ttest <- t.test(
    x = x, y = y,
    alternative = alternative,
    mu = mu,
    paired = paired,
    var.equal = var.equal,
    conf.level = conf.level
  )

  out$mean_x <- mean(x)

  if(!is.null(y)){
    out$mean_y <- mean(y)
    out$diff <- mean(x) - mean(y)

    if(paired & cor){
      out$cor_xy <- cor(x,y)
    }
  }

  out$t <- ttest$statistic
  out$df <- ttest$parameter
  out$p <- ttest$p.value

  if(ci == "freq"){
    if(is.null(y)){
      out$mean_x_lower <- ttest$conf.int[1]
      out$mean_x_upper <- ttest$conf.int[2]
    } else{
      out$diff_lower <- ttest$conf.int[1]
      out$diff_upper <- ttest$conf.int[2]
    }
  }

  # Frequentist effect size

  if(cohens_d & !is.null(y)){
    d_est <- effectsize::cohens_d(
      x,y,
      mu = mu, paired = F, ci = conf.level, alternative = alternative,
      adjust = F
    )

    out$cohens_d <- d_est$Cohens_d

    if(ci == "freq"){
      out$cohens_d_lower <- d_est$CI_low
      out$cohens_d_upper <- d_est$CI_high
    }
  }

  if(cohens_d & is.null(y)){
    d_est <- effectsize::cohens_d(
      x,
      mu = mu, ci = conf.level, alternative = alternative,
      adjust = F
    )

    out$cohens_d <- d_est$Cohens_d

    if(ci == "freq"){
      out$cohens_d_lower <- d_est$CI_low
      out$cohens_d_upper <- d_est$CI_high
    }
  }

  if(cohens_dz & !is.null(y) & paired){
    dz_est <- effectsize::repeated_measures_d(
      x, y,
      method = "z",
      mu = mu, ci = conf.level, alternative = alternative,
      adjust = F
    )

    out$cohens_dz <- dz_est$d_z

    if(ci == "freq"){
      out$cohens_dz_lower <- dz_est$CI_low
      out$cohens_dz_upper <- dz_est$CI_high
    }
  }

  # Bayesian Estimation

  mcmcsample <- NULL

  if(
    (!is.null(y) & diff_EAP)|(!is.null(y) & diff_MAP)|(!is.null(y) & diff_MED)|
    cohens_d_EAP|cohens_d_MAP|cohens_d_MED|
    (is.null(y) & mean_x_EAP)|(is.null(y) & mean_x_MAP)|(is.null(y) & mean_x_MED)|
    (paired & cohens_dz_EAP)|(paired & cohens_dz_MAP)|(paired & cohens_dz_MED)|
    (ci %in% c("bayes_central", "bayes_hdi"))|pd
  ) {
    suppressMessages(
      mcmcsample <- BayesFactor::ttestBF(
        x = x, y = y, mu = mu, paired = paired,
        posterior = T, rscale = rscale_est, iterations = iterations,
        progress = F
      )
    )

    if(is.null(y)){
      if(mean_x_EAP) out$mean_x_EAP <- mean(mcmcsample[,"mu"])
      if(mean_x_MAP) out$mean_x_MAP <- mode_stat(mcmcsample[,"mu"], n = map_density_n)
      if(mean_x_MED) out$mean_x_MED <- median(mcmcsample[,"mu"])
      if(cohens_d_EAP) out$cohens_d_EAP <- mean(mcmcsample[,"delta"])
      if(cohens_d_MAP) out$cohens_d_MAP <- mode_stat(mcmcsample[,"delta"], n = map_density_n)
      if(cohens_d_MED) out$cohens_d_MED <- median(mcmcsample[,"delta"])

      if(ci == "bayes_central"){
        out$mean_x_lower <- quantile(mcmcsample[,"mu"], (1 - conf.level)/2)
        out$mean_x_upper <- quantile(mcmcsample[,"mu"], (1 - conf.level)/2 + conf.level)
        if(cohens_d | cohens_d_EAP | cohens_d_MAP | cohens_d_MED){
          out$cohens_d_lower <- quantile(mcmcsample[,"delta"], (1 - conf.level)/2)
          out$cohens_d_upper <- quantile(mcmcsample[,"delta"], (1 - conf.level)/2 + conf.level)
        }
      }

      if(ci == "bayes_hdi"){
        tmp <- hdi(mcmcsample[,"mu"], prob = conf.level)
        out$mean_x_lower <- tmp[1]
        out$mean_x_upper <- tmp[2]
        if(cohens_d | cohens_d_EAP | cohens_d_MAP | cohens_d_MED){
          tmp <- hdi(mcmcsample[,"delta"], prob = conf.level)
          out$cohens_d_lower <- tmp[1]
          out$cohens_d_upper <- tmp[2]
        }
      }

    } else if(paired){
      mcmcsample_diff <- sqrt(mcmcsample[,"sig2"]) * mcmcsample[,"delta"]
      if(diff_EAP) out$diff_EAP <- mean(mcmcsample_diff)
      if(diff_MAP) out$diff_MAP <- mode_stat(mcmcsample_diff, n = map_density_n)
      if(diff_MED) out$diff_MED <- median(mcmcsample_diff)
      if(cohens_dz_EAP) out$cohens_dz_EAP <- mean(mcmcsample[,"delta"])
      if(cohens_dz_MAP) out$cohens_dz_MAP <- mode_stat(mcmcsample[,"delta"], n = map_density_n)
      if(cohens_dz_MED) out$cohens_dz_MED <- median(mcmcsample[,"delta"])

      if(cohens_d_EAP|cohens_d_MAP|cohens_d_MED|
         (ci %in% c("bayes_central", "bayes_hdi") & cohens_d)){
        suppressMessages(
          mcmcsample_between <- BayesFactor::ttestBF(
            x = x, y = y, mu = mu, paired = F,
            posterior = T, rscale = rscale_est, iterations = 10000,
            progress = F
          )
        )

        if(cohens_d_EAP) out$cohens_d_EAP <- mean(mcmcsample_between[,"delta"])
        if(cohens_d_MAP) out$cohens_d_MAP <- mode_stat(mcmcsample_between[,"delta"], n = map_density_n)
        if(cohens_d_MED) out$cohens_d_MED <- median(mcmcsample_between[,"delta"])
      }

      if(ci == "bayes_central"){
        out$diff_lower <- quantile(mcmcsample_diff, (1 - conf.level)/2)
        out$diff_upper <- quantile(mcmcsample_diff, (1 - conf.level)/2 + conf.level)
        if(cohens_d | cohens_d_EAP | cohens_d_MAP | cohens_d_MED){
          out$cohens_d_lower <- quantile(mcmcsample_between[,"delta"], (1 - conf.level)/2)
          out$cohens_d_upper <- quantile(mcmcsample_between[,"delta"], (1 - conf.level)/2 + conf.level)
        }
        if(cohens_dz | cohens_dz_EAP | cohens_dz_MAP | cohens_dz_MED){
          out$cohens_dz_lower <- quantile(mcmcsample[,"delta"], (1 - conf.level)/2)
          out$cohens_dz_upper <- quantile(mcmcsample[,"delta"], (1 - conf.level)/2 + conf.level)
        }
      }

      if(ci == "bayes_hdi"){
        tmp <- hdi(mcmcsample_diff, prob = conf.level)
        out$diff_lower <- tmp[1]
        out$diff_upper <- tmp[2]
        if(cohens_d | cohens_d_EAP | cohens_d_MAP | cohens_d_MED){
          tmp <- hdi(mcmcsample_between[,"delta"], prob = conf.level)
          out$cohens_d_lower <- tmp[1]
          out$cohens_d_upper <- tmp[2]
        }
        if(cohens_dz | cohens_dz_EAP | cohens_dz_MAP | cohens_dz_MED){
          tmp <- hdi(mcmcsample[,"delta"], prob = conf.level)
          out$cohens_dz_lower <- tmp[1]
          out$cohens_dz_upper <- tmp[2]
        }
      }

    } else{
      if(diff_EAP) out$diff_EAP <- mean(mcmcsample[,"beta (x - y)"])
      if(diff_MAP) out$diff_MAP <- mode_stat(mcmcsample[,"beta (x - y)"], n = map_density_n)
      if(diff_MED) out$diff_MED <- median(mcmcsample[,"beta (x - y)"])
      if(cohens_d_EAP) out$cohens_d_EAP <- mean(mcmcsample[,"delta"])
      if(cohens_d_MAP) out$cohens_d_MAP <- mode_stat(mcmcsample[,"delta"], n = map_density_n)
      if(cohens_d_MED) out$cohens_d_MED <- median(mcmcsample[,"delta"])

      if(ci == "bayes_central"){
        out$diff_lower <- quantile(mcmcsample[,"beta (x - y)"], (1 - conf.level)/2)
        out$diff_upper <- quantile(mcmcsample[,"beta (x - y)"], (1 - conf.level)/2 + conf.level)
        if(cohens_d | cohens_d_EAP | cohens_d_MAP | cohens_d_MED){
          out$cohens_d_lower <- quantile(mcmcsample[,"delta"], (1 - conf.level)/2)
          out$cohens_d_upper <- quantile(mcmcsample[,"delta"], (1 - conf.level)/2 + conf.level)
        }
      }

      if(ci == "bayes_hdi"){
        tmp <- hdi(mcmcsample[,"beta (x - y)"], prob = conf.level)
        out$diff_lower <- tmp[1]
        out$diff_upper <- tmp[2]
        if(cohens_d | cohens_d_EAP | cohens_d_MAP | cohens_d_MED){
          tmp <- hdi(mcmcsample[,"delta"], prob = conf.level)
          out$cohens_d_lower <- tmp[1]
          out$cohens_d_upper <- tmp[2]
        }
      }
    }

    if(pd){
      out$pd <- pdir(mcmcsample[,"delta"], na.rm = T)
    }

  }

  # Bayes factor

  if(bf){
    suppressMessages(
      bf_est <- BayesFactor::ttestBF(
        x = x, y = y, mu = mu, paired = paired,
        rscale = rscale_bf
      )
    )

    out$BF10 <- exp(bf_est@bayesFactor$bf)
    out$log10_BF10 <- log10(out$BF10)
    out <- out %>%
      dplyr::mutate(
        favor = dplyr::if_else(BF10 > 1, "alt.", "null"),
        evidence = "anecdotal",
        evidence = dplyr::if_else(abs(log10_BF10) > log(3, 10), "moderate", evidence),
        evidence = dplyr::if_else(abs(log10_BF10) > log(10, 10), "strong", evidence),
        evidence = dplyr::if_else(abs(log10_BF10) > log(30, 10), "very strong", evidence),
        evidence = dplyr::if_else(abs(log10_BF10) > log(100, 10), "extreme", evidence)
      )

  }

  if (verbose) {
    cat("\n-----------------------------------------------\n")
    if(is.null(y)){
      cat("One-sample t-test")
    } else if(paired){
      cat("Paired t-test")
    } else if(var.equal){
      cat("Two-sample t-test")
    } else{
      cat("Two-sample t-test\n(Welch's method applied in the frequentist analysis)",
          sep = "")
    }

    cat("\n-----------------------------------------------\n")

    char_rel <- "!="
    if(alternative == "less") char_rel <- "<"
    if(alternative == "greater") char_rel <- ">"

    if(is.null(y)){
      cat(
        "H0: mu_x = ",mu,", ",
        "H1: mu_x ",char_rel," ",mu,"\n",
        sep = ""
      )
    }

    if(!is.null(y)){
      cat(
        "H0: mu_x - mu_y = ",mu,", ",
        "H1: mu_x - mu_y ",char_rel," ",mu, "\n",
        sep = ""
      )
    }

    if(is.null(y)){
      cat(
        "\nN = ", out$n_x, "\n",
        sep = ""
      )
      if(out$n_na_x > 0) cat("(Note: NAs were removed.)\n")
    } else if(paired){
      cat(
        "\nN = ", out$n_pair, " (pairs)\n",
        sep = ""
      )
      if(out$n_na > 0) cat("(Note: NAs were removed using pairwise deletion.)\n")
    } else{
      cat(
        "\nn_x = ", out$n_x, ", ",
        "n_y = ", out$n_y, "\n",
        sep = ""
      )
      if(out$n_na_x > 0|out$n_na_y > 0) cat("(Note: NAs were removed.)\n")
    }

    if(!fullbayes){
      cat(
        "\nt = ", out$t, ", ",
        "df = ", out$df, ", ",
        "p = ", out$p,
        sep = ""
      )
    }

    if(!is.null(out$cor_xy)){
      cat(
        "\nr_(x,y) = ", out$cor_xy,
        sep = ""
      )
    }

    if(pd){
      cat(
        "\npd = ", out$pd, sep = ""
      )
    }

    if(bf){
      rscale_show <- rscale_bf
      if(rscale_show == "medium") rscale_show <- sqrt(2)/2
      if(rscale_show == "wide") rscale_show <- 1
      if(rscale_show == "ultrawide") rscale_show <- sqrt(2)

      cat(
        "\n\nBF_10 = ", out$BF10, " in favor of H",as.numeric(out$favor == "alt."), " (", out$evidence, ")",
        "\nrscale = ",rscale_show, sep = ""
      )

      cat(
        "\nNote: BF is sensitive to the rscale parameter of the prior."
      )
    }

    # Mean of x

    cat("\n-----------------------------------------------")

    cat(
      "\nMean of x = ", out$mean_x, "",
      sep = ""
    )

    if(is.null(y)){

      if(mean_x_EAP){
        cat(
          "\nEAP of mu_x = ", out$mean_x_EAP, "",
          sep = ""
        )
      }

      if(mean_x_MAP){
        cat(
          "\nMAP of mu_x = ", out$mean_x_MAP, "",
          sep = ""
        )
      }

      if(mean_x_MED){
        cat(
          "\nMED of mu_x = ", out$mean_x_MED, "",
          sep = ""
        )
      }

      cat(
        "\nCI = [", out$mean_x_lower,", ",out$mean_x_upper,"]",
        sep = ""
      )

    } else{
      cat(
        "\nMean of y = ", out$mean_y, "",
        sep = ""
      )
    }

    # Mean of diff

    if(!is.null(y)){
      cat("\n-----------------------------------------------")

      cat(
        "\nDiff (x-y) = ", out$diff, "",
        sep = ""
      )

      if(diff_EAP){
        cat(
          "\nEAP of diff = ", out$diff_EAP, "",
          sep = ""
        )
      }

      if(diff_MAP){
        cat(
          "\nMAP of diff = ", out$diff_MAP, "",
          sep = ""
        )
      }

      if(diff_MED){
        cat(
          "\nMED of diff = ", out$diff_MED, "",
          sep = ""
        )
      }

      cat(
        "\nCI = [", out$diff_lower,", ",out$diff_upper,"]",
        sep = ""
      )

    }

    # Cohen's d

    if(cohens_d | cohens_d_EAP | cohens_d_MAP | cohens_d_MED){
      cat("\n-----------------------------------------------")
    }

    if(cohens_d){
      cat(
        "\nCohen's d = ", out$cohens_d, "",
        sep = ""
      )
    }

    if(cohens_d_EAP){
      cat(
        "\nEAP of delta = ", out$cohens_d_EAP, "",
        sep = ""
      )
    }

    if(cohens_d_MAP){
      cat(
        "\nMAP of delta = ", out$cohens_d_MAP, "",
        sep = ""
      )
    }

    if(cohens_d_MED){
      cat(
        "\nMED of delta = ", out$cohens_d_MED, "",
        sep = ""
      )
    }

    if(cohens_d|cohens_d_EAP|cohens_d_MAP|cohens_d_MED){
      cat(
        "\nCI = [", out$cohens_d_lower,", ",out$cohens_d_upper,"]",
        sep = ""
      )
    }

    # Cohen's dz

    if(paired & !is.null(y) & (cohens_dz | cohens_dz_EAP | cohens_dz_MAP | cohens_dz_MED)){
      cat("\n-----------------------------------------------")
    }

    if(paired & !is.null(y)){
      if(cohens_dz & paired & !is.null(y)){
        cat(
          "\nCohen's d_z = ", out$cohens_dz, "",
          sep = ""
        )
      }

      if(cohens_dz_EAP & paired & !is.null(y)){
        cat(
          "\nEAP of delta_z = ", out$cohens_dz_EAP, "",
          sep = ""
        )
      }

      if(cohens_dz_MAP & paired & !is.null(y)){
        cat(
          "\nMAP of delta_z = ", out$cohens_dz_MAP, "",
          sep = ""
        )
      }

      if(cohens_dz_MED & paired & !is.null(y)){
        cat(
          "\nMED of delta_z = ", out$cohens_dz_MED, "",
          sep = ""
        )
      }

      cat(
        "\nCI = [", out$cohens_dz_lower,", ",out$cohens_dz_upper,"]",
        sep = ""
      )
    }


    cat("\n-----------------------------------------------\n")

    if(ci == "freq"){
      cat("Note:\nThe CI represents the frequentist confidence interval.")
      cat("\n(Confidence level = ",conf.level,")\n", sep = "")
    }

    if(ci == "bayes_central"){
      cat("Note:\nThe CI represents the Bayesian central posterior interval.")
      cat("\n(Probability = ",conf.level,")\n", sep = "")
    }

    if(ci == "bayes_hdi"){
      cat("Note:\nThe CI represents the Bayesian posterior highest density interval.")
      cat("\n(Probability = ",conf.level,")\n", sep = "")
    }

    if(!is.null(mcmcsample)){
      cat("Note:\nThe posterior was approximated using ",iterations, " samples,\ngiven prior rscale = ",rscale_est,".\n",
          sep = "")
    }
  }

  out$sig <- dplyr::if_else(out$p < alpha, "*", "ns")

  if(fullbayes){
    out <- out %>%
      dplyr::mutate(
        t = NA_real_, df = NA_real_, p = NA_real_, alpha = NA_real_, sig = NA_character_
      )
  }

  if(!detailed){
    out <- out %>%
      dplyr::transmute(
        diff = diff,
        t = t %>% round(2),
        df = df,
        p = p %>% round(3),
        alpha = alpha,
        sig = sig,
        cohens_d = cohens_d %>% round(3),
        cohens_dz = cohens_dz %>% round(3),
        pd = pd %>% round(3),
        BF10 = BF10 %>% round(3),
        log10_BF10 = log10_BF10,
        favor = favor,
        evidence = evidence
      )
  }

  out <- out %>%
    dplyr::select_if(~ !any(is.na(.)))

  return(invisible(out))
  }
