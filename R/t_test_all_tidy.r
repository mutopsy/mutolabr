#' Perform Frequentist and Bayesian t-tests with Effect Size Estimation from Tidy Data
#'
#' A wrapper around \code{\link{t_test_all}} that accepts tidy-format data frames
#' with exactly three columns (participant ID, independent variable, dependent variable)
#' or two columns only for one-sample design (participant ID, dependent variable).
#' The function automatically reshapes the data (long \eqn{\rightarrow} wide for paired designs),
#' checks for consistency, and then calls \code{t_test_all()} with the extracted vectors.
#'
#' @param dataset A data frame with exactly three columns, in this order:
#'   \enumerate{
#'     \item Participant ID
#'     \item Independent variable (must have exactly two levels)
#'     \item Dependent variable (numeric)
#'   }
#' @param paired Logical. If \code{TRUE}, a paired t-test is performed (default \code{FALSE}).
#' @param var.equal Logical. If \code{TRUE}, the two-sample t-test assumes equal variances (default \code{FALSE}).
#' @param onesample Logical. If \code{TRUE}, a one-sample t-test is performed (default \code{FALSE}).
#' @param mu Numeric. Null hypothesis value for the mean difference (default \code{0}).
#' @param ci Character vector specifying interval type(s): \code{"freq"}, \code{"bayes_central"},
#'   or \code{"bayes_hdi"}. Default is \code{c("freq","bayes_central","bayes_hdi")}. Passed to \code{t_test_all}.
#' @param alternative Character. Alternative hypothesis: \code{"two.sided"}, \code{"less"}, or \code{"greater"}.
#'   Default is \code{"two.sided"}. Passed to \code{t_test_all}.
#' @param conf.level Numeric. Confidence/credibility level (default \code{0.95}). Passed to \code{t_test_all}.
#' @param alpha Numeric. Significance level. Defaults to 0.05.
#' @param pd,bf,cor Logical flags. Whether to compute probability of direction (pd), Bayes factors, or correlation
#'   (for paired samples). Passed to \code{t_test_all}.
#' @param mean_x_EAP,mean_x_MAP,mean_x_MED Logical. Report posterior summaries for group means.
#'   Passed to \code{t_test_all}.
#' @param diff_EAP,diff_MAP,diff_MED Logical. Report posterior summaries for the mean difference.
#'   Passed to \code{t_test_all}.
#' @param cohens_d Logical or character. Type/request for Cohen's d in independent samples. Passed to \code{t_test_all}.
#' @param cohens_d_EAP,cohens_d_MAP,cohens_d_MED Logical. Posterior summaries for Cohen's d. Passed to \code{t_test_all}.
#' @param cohens_dz Logical. Request Cohen's dz for paired samples. Passed to \code{t_test_all}.
#' @param cohens_dz_EAP,cohens_dz_MAP,cohens_dz_MED Logical. Posterior summaries for Cohen's dz. Passed to \code{t_test_all}.
#' @param rscale_est,rscale_bf Numeric or character. Cauchy prior scales for estimation and Bayes factors
#'   (e.g., \code{"ultrawide"}, \code{"wide"}, \code{"medium"}, or a positive number).
#'   Defaults are \code{Inf} and \code{"medium"}, respectively. Passed to \code{t_test_all}.
#' @param iterations,map_density_n Integer. MCMC iterations and grid size for MAP density. Passed to \code{t_test_all}.
#' @param verbose Logical. If \code{TRUE}, print additional messages (default \code{FALSE}).
#' @param show_table Logical. If \code{TRUE}, print table with design (default \code{TRUE}).
#' @param detailed Logical. Whether to return detailed results (\code{TRUE}) or
#'   minimal output (\code{FALSE}, default).
#' @param fullbayes Logical. Whether to show only Bayesian results (\code{TRUE}) or both frequentist and Bayesian results (\code{FALSE}, default).
#'
#' @return The object returned by \code{\link{t_test_all}} (test statistics, effect sizes,
#'   confidence/credible intervals, and Bayesian estimates).
#'
#' @details
#' This function enforces the following:
#' \itemize{
#'   \item \code{dataset} must have exactly three columns in the order: ID, independent variable, dependent variable.
#'   \item The independent variable must have exactly two levels.
#'   \item The dependent variable must be numeric.
#'   \item If \code{paired = TRUE}, each participant must contribute exactly one observation in each condition.
#' }
#' If any requirement is violated, an informative error is raised.
#'
#' @examples
#' set.seed(610)
#' dat <- data.frame(
#'   id   = rep(1:10, each = 2),
#'   cond = rep(c("A","B"), times = 10),
#'   y    = rnorm(20)
#' )
#' # Independent-samples t-test
#' t_test_all_tidy(dat, paired = FALSE)
#' # Paired-samples t-test
#' t_test_all_tidy(dat, paired = TRUE)
#'
#' @seealso \code{\link{t_test_all}}
#'
#' @importFrom dplyr group_by summarise filter rename pull
#' @importFrom tidyr pivot_wider
#' @export

t_test_all_tidy <- function(
    dataset, paired = F, var.equal = FALSE, onesample = FALSE, mu = 0,
    ci = c("freq","bayes_central",  "bayes_hdi"),
    alternative = c("two.sided", "less", "greater"),
    conf.level = 0.95, alpha = 0.05,
    pd = FALSE, bf = FALSE, cor = TRUE,
    mean_x_EAP = FALSE, mean_x_MAP = FALSE, mean_x_MED = FALSE,
    diff_EAP = FALSE, diff_MAP = FALSE, diff_MED = FALSE,
    cohens_d = NULL, cohens_d_EAP = FALSE, cohens_d_MAP = FALSE, cohens_d_MED = FALSE,
    cohens_dz = TRUE, cohens_dz_EAP = FALSE, cohens_dz_MAP = FALSE, cohens_dz_MED = FALSE,
    rscale_est = Inf, rscale_bf = "medium",
    iterations = 10000, map_density_n = 512, verbose = FALSE, show_table = TRUE,
    detailed = FALSE, fullbayes = FALSE
){

  # initialization

  ## Calculate numbers of factors and levels

  ncol <- ncol(dataset)

  if(!onesample){
    # Two-sample or paired t-test --------------------------------------

    if(ncol != 3){
      stop("The dataset must contain exactly three columns for two-sample or paired t-tests, in this exact order: participant ID, independent variable, and dependent variable.")
    }

    if(!is.numeric(dataset[[3]])){
      stop("The third column (dependent variable) must be numeric.")
    }

    dataset[[1]] <- as.factor(dataset[[1]])
    dataset[[2]] <- as.factor(dataset[[2]]) %>% droplevels()

    levels_label <- dataset[[2]] %>% na.omit() %>% unique()
    nlevel <- nlevels(dataset[[2]])

    if(nlevel != 2){
      stop("The second column (independent variable) must have exactly two levels.")
    }

    ## Transform data

    if(paired){
      check <- dataset %>%
        dplyr::rename(obs = 1) %>%
        dplyr::group_by(obs) %>%
        dplyr::summarise(n = n()) %>%
        dplyr::filter(n != 2)

      if(nrow(check) != 0){
        stop("Each participant must have exactly one observation in each condition for a paired t-test.")
      }

      d2 <- dataset %>%
        tidyr::pivot_wider(names_from = 2, values_from = 3)

      x <- d2 %>% dplyr::pull(2)
      y <- d2 %>% dplyr::pull(3)

    } else{
      x <- dataset %>%
        dplyr::rename(group_mutolabr = 2) %>%
        dplyr::filter(group_mutolabr == levels_label[1]) %>%
        dplyr::pull(3)

      y <- dataset %>%
        dplyr::rename(group_mutolabr = 2) %>%
        dplyr::filter(group_mutolabr == levels_label[2]) %>%
        dplyr::pull(3)
    }

    ## Pass transformed data to t_test_all()

    var.label <- levels_label

    out <- t_test_all(
      x = x, y = y, var.label = var.label, paired = paired, var.equal = var.equal, mu = mu,
      ci = ci,  alternative = alternative,
      conf.level = conf.level, alpha = alpha,
      pd = pd, bf = bf, cor = cor,
      mean_x_EAP = mean_x_EAP, mean_x_MAP = mean_x_MAP, mean_x_MED = mean_x_MED,
      diff_EAP = diff_EAP, diff_MAP = diff_MAP, diff_MED = diff_MED,
      cohens_d = cohens_d, cohens_d_EAP = cohens_d_EAP, cohens_d_MAP = cohens_d_MAP, cohens_d_MED = cohens_d_MED,
      cohens_dz = cohens_dz, cohens_dz_EAP = cohens_dz_EAP, cohens_dz_MAP = cohens_dz_MAP, cohens_dz_MED = cohens_dz_MED,
      rscale_est = rscale_est, rscale_bf = rscale_bf,
      iterations = iterations, map_density_n = map_density_n, verbose = verbose,
      detailed = detailed, fullbayes = fullbayes
    )

    if(show_table){

      if(paired){
        cat("design: paired")
      } else if(var.equal){
        cat("design: two samples (equal variance)")
      } else{
        cat("design: two samples (unequal variance)")
      }
      print(out)

    }

  } else{
    # one-sample t-test --------------------------------------

    if(paired){
      warning("Because 'onesample' was true, 'paired' was ignored.")
      paired <- FALSE
    }

    if(!ncol %in% c(2,3)){
      stop("The dataset must contain two or three columns for one-sample t-tests, in this exact order: participant ID, (independent variable with one level), and dependent variable.")
    }

    if(ncol == 3){
      if(!is.numeric(dataset[[3]])){
        stop("The third column (dependent variable) must be numeric.")
      }

      dataset[[1]] <- as.factor(dataset[[1]])
      dataset[[2]] <- as.factor(dataset[[2]]) %>% droplevels()

      levels_label <- dataset[[2]] %>% na.omit() %>% unique()
      nlevel <- nlevels(dataset[[2]])

      if(nlevel != 1){
        stop("The second column (independent variable) must have exactly one level.")
      }
      x <- dataset[[3]]
    } else if(ncol == 2){
      x <- dataset[[2]]
    }

    ## Pass transformed data to t_test_all()

    var.label <- NULL

    out <- t_test_all(
      x = x, y = NULL, var.label = var.label, var.equal = var.equal, mu = mu,
      ci = ci,  alternative = alternative,
      conf.level = conf.level, alpha = alpha,
      pd = pd, bf = bf, cor = cor,
      mean_x_EAP = mean_x_EAP, mean_x_MAP = mean_x_MAP, mean_x_MED = mean_x_MED,
      diff_EAP = diff_EAP, diff_MAP = diff_MAP, diff_MED = diff_MED,
      cohens_d = cohens_d, cohens_d_EAP = cohens_d_EAP, cohens_d_MAP = cohens_d_MAP, cohens_d_MED = cohens_d_MED,
      cohens_dz = cohens_dz, cohens_dz_EAP = cohens_dz_EAP, cohens_dz_MAP = cohens_dz_MAP, cohens_dz_MED = cohens_dz_MED,
      rscale_est = rscale_est, rscale_bf = rscale_bf,
      iterations = iterations, map_density_n = map_density_n, verbose = verbose,
      detailed = detailed, fullbayes = fullbayes
    )
    if(show_table){
      cat("design: one sample")
      print(out)
    }
  }

  return(invisible(out))
  }

