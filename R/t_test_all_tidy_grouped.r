#' Run multiple t-tests by groups on tidy data
#'
#' A wrapper around \code{\link{t_test_all_tidy}} that performs t-tests
#' separately for each combination of grouping variables in a tidy dataset.
#' Optionally, it provides Holm's step-down family-wise error control by
#' returning per-hypothesis critical alpha thresholds.
#'
#' @param dataset A tidy data frame containing participant ID, one or more
#'   grouping variables, an independent variable with exactly two levels,
#'   and a numeric dependent variable (in that order).
#' @param n_group_col Integer. The number of grouping columns (default = 1).
#' @param holm Logical. If \code{TRUE} and the output contains a \code{p}
#'   column, an \code{alpha} column is (re)computed using Holm's method via
#'   \code{\link{p_to_holmalpha}} with \code{sig.level = alpha}. This affects
#'   only the per-hypothesis critical alpha thresholds, not the p-values.
#'   Default is \code{FALSE}.
#' @param alpha Numeric in (0, 1). Family-wise significance level used for
#'   Holm's method when \code{holm = TRUE}. Default is \code{0.05}.
#' @inheritParams t_test_all_tidy
#'
#' @details
#' The function validates the dataset layout, groups by the specified
#' grouping variables, and applies \code{t_test_all_tidy()} within each
#' group. When \code{holm = TRUE}, it replaces/creates an \code{alpha}
#' column containing Holm step-down critical alpha values corresponding to
#' the reported \code{p} values (per group), controlling the family-wise
#' error rate at \code{alpha}. P-values themselves are not adjusted.
#'
#' @return A data frame with t-test results for each group, matching the
#'   structure of \code{t_test_all_tidy()} and, if requested, including
#'   Holm-based critical alpha thresholds in \code{alpha}.
#'
#' @examples
#' # Example with one grouping variable:
#' head(data_fixt_mixed)
#' t_test_all_tidy_grouped(data_fict_mixed, paired = TRUE, n_group_col = 1)
#'
#' @seealso \code{\link{t_test_all_tidy}}, \code{\link{t_test_all}}
#' @importFrom dplyr group_by ungroup group_modify across all_of %>%
#' @importFrom stats na.omit
#' @export

t_test_all_tidy_grouped <- function(
    dataset, n_group_col = 1, paired = F, var.equal = FALSE, mu = 0,
    ci = c("freq","bayes_central",  "bayes_hdi"),
    alternative = c("two.sided", "less", "greater"),
    conf.level = 0.95, alpha = 0.05, holm = FALSE,
    pd = FALSE, bf = FALSE, cor = TRUE,
    mean_x_EAP = FALSE, mean_x_MAP = FALSE, mean_x_MED = FALSE,
    diff_EAP = FALSE, diff_MAP = FALSE, diff_MED = FALSE,
    cohens_d = NULL, cohens_d_EAP = FALSE, cohens_d_MAP = FALSE, cohens_d_MED = FALSE,
    cohens_dz = TRUE, cohens_dz_EAP = FALSE, cohens_dz_MAP = FALSE, cohens_dz_MED = FALSE,
    rscale_est = Inf, rscale_bf = "medium",
    iterations = 10000, map_density_n = 512,
    detailed = FALSE, fullbayes = FALSE
){

  # initialization

  ## Calculate numbers of factors and levels

  ncol <- ncol(dataset)

  if (!is.numeric(n_group_col) || length(n_group_col) != 1L || n_group_col < 0 || n_group_col %% 1 != 0) {
    stop("'n_group_col' must be a non-negative integer.")
  }

  if(ncol != (3 + n_group_col)){
    stop("The dataset must contain exactly three columns plus numbers of group colmuns, in this exact order: participant ID, ... (group columns), independent variable, and dependent variable.")
  }

  if(!is.numeric(dataset[[3 + n_group_col]])){
    stop("The dependent variable must be numeric.")
  }

  dataset[[1]] <- as.factor(dataset[[1]])
  dataset[[2 + n_group_col]] <- as.factor(dataset[[2 + n_group_col]]) %>% droplevels()

  levels_label <- dataset[[2 + n_group_col]] %>% na.omit() %>% unique()
  nlevel <- nlevels(dataset[[2 + n_group_col]])

  if(nlevel != 2){
    stop("The independent variable must have exactly two levels.")
  }

  ## Pass to t_test_all_tidy

  if(n_group_col >= 1){
    group_cols <- colnames(dataset)[2:(n_group_col+1)]

    out <- dataset %>%
      group_by(across(all_of(group_cols))) %>%
      group_modify(
        ~ t_test_all_tidy(
          dataset = .x, paired = paired, var.equal = var.equal,
          ci = ci,  alternative = alternative,
          conf.level = conf.level, alpha = alpha,
          pd = pd, bf = bf, cor = cor,
          mean_x_EAP = mean_x_EAP, mean_x_MAP = mean_x_MAP, mean_x_MED = mean_x_MED,
          diff_EAP = diff_EAP, diff_MAP = diff_MAP, diff_MED = diff_MED,
          cohens_d = cohens_d, cohens_d_EAP = cohens_d_EAP, cohens_d_MAP = cohens_d_MAP, cohens_d_MED = cohens_d_MED,
          cohens_dz = cohens_dz, cohens_dz_EAP = cohens_dz_EAP, cohens_dz_MAP = cohens_dz_MAP, cohens_dz_MED = cohens_dz_MED,
          rscale_est = rscale_est, rscale_bf = rscale_bf,
          iterations = iterations, map_density_n = map_density_n, verbose = FALSE,
          detailed = detailed, fullbayes = fullbayes
        )
      ) %>%
      ungroup()

    if(holm && "alpha" %in% colnames(out)){
      out$alpha <- p_to_holmalpha(out$p, sig.level = alpha)
    }

  } else{
    out <- t_test_all_tidy(
        dataset = dataset, paired = paired, var.equal = var.equal,
        ci = ci,  alternative = alternative,
        conf.level = conf.level, alpha = alpha,
        pd = pd, bf = bf, cor = cor,
        mean_x_EAP = mean_x_EAP, mean_x_MAP = mean_x_MAP, mean_x_MED = mean_x_MED,
        diff_EAP = diff_EAP, diff_MAP = diff_MAP, diff_MED = diff_MED,
        cohens_d = cohens_d, cohens_d_EAP = cohens_d_EAP, cohens_d_MAP = cohens_d_MAP, cohens_d_MED = cohens_d_MED,
        cohens_dz = cohens_dz, cohens_dz_EAP = cohens_dz_EAP, cohens_dz_MAP = cohens_dz_MAP, cohens_dz_MED = cohens_dz_MED,
        rscale_est = rscale_est, rscale_bf = rscale_bf,
        iterations = iterations, map_density_n = map_density_n, verbose = FALSE,
        detailed = detailed, fullbayes = fullbayes
      )
  }

  return(out)
}
