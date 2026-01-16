#' Planned contrast test for tidy-format data using explicit sum-of-squares decomposition
#'
#' This function performs a planned contrast test on tidy-format data
#' (participant × condition × response × contrast weight),
#' by explicitly constructing sum-of-squares components.
#'
#' The implementation is intentionally written in a step-by-step
#' decomposition style (grand mean, condition effects, residuals,
#' contrast component, error component) so that students can directly
#' inspect how a 1-degree-of-freedom contrast emerges as a regression
#' on contrast weights and how its F statistic is constructed.
#'
#' Although equivalent numerical results can be obtained by fitting
#' a linear model, this function is designed for educational purposes
#' to make the internal structure of contrast tests transparent.
#'
#' @param dataset A data frame or tibble with exactly four columns,
#'   in the following order:
#'   (1) participant ID,
#'   (2) independent variable (condition),
#'   (3) dependent variable (response),
#'   (4) contrast weights.
#'
#' @param paired Logical. If \code{TRUE}, a within-participants
#'   (repeated-measures) contrast is performed.
#'   If \code{FALSE}, an independent-groups contrast is performed.
#'
#' @param alpha Significance level used for the \code{sig} column.
#'
#' @param inc_allvar Logical. If \code{TRUE}, return intermediate
#'   sums of squares and mean squares.
#'   If \code{FALSE}, return only commonly reported statistics.
#'
#' @param do_round Logical. If \code{TRUE}, round output values
#'   for compact display.
#'
#' @param autocorrect_unbalance Logical.
#'   If \code{TRUE} and \code{paired = FALSE}, contrast weights
#'   are automatically converted from condition-level coefficients
#'   to observation-level coefficients when group sizes are unbalanced,
#'   so that the tested contrast corresponds to the standard
#'   planned contrast on condition means.
#'
#' @details
#' The function assumes that contrast weights are constant within each
#' condition. If their sum is not zero, weights are automatically centered.
#'
#' Internally, the contrast is treated as a 1-degree-of-freedom regression
#' on contrast weights, and the F statistic is constructed from explicit
#' sum-of-squares decomposition rather than by calling \code{lm()}.
#'
#' In unbalanced designs, results may differ slightly from textbook
#' planned contrasts based on condition means, because the contrast
#' is implemented as a 1-df regression on observation-level contrast weights.
#'
#' This design is intended to support teaching of contrast tests
#' and ANOVA sum-of-squares structure.
#'
#' @return A data frame containing at least:
#' \itemize{
#'   \item \code{effect} Contrast label.
#'   \item \code{df1}, \code{df2} Numerator and denominator degrees of freedom.
#'   \item \code{F} F statistic.
#'   \item \code{p} p-value.
#'   \item \code{sig} Significance code (\code{"*"} or \code{"ns"}).
#'   \item \code{eta2} Eta-squared.
#'   \item \code{peta2} Partial eta-squared.
#'   \item \code{cohens_f} Cohen's f.
#' }
#' If \code{inc_allvar = TRUE}, sum-of-squares and mean squares are also returned.
#'
#' @examples
#' # Example with independent groups
#' df <- data.frame(
#'   id = rep(1:30, each = 1),
#'   cond = rep(c("A","B","C"), each = 10),
#'   y = rnorm(30),
#'   w = rep(c(-1, 0, 1), each = 10)
#' )
#'
#' contrast_test_tidy(df)
#'
#' @importFrom dplyr rename mutate group_by ungroup summarise left_join select n_distinct if_else
#' @importFrom tidyr complete
#' @importFrom stats cov var pf
#'
#' @export

contrast_test_tidy <- function(
    dataset, paired = FALSE, alpha = 0.05, inc_allvar = FALSE, do_round = TRUE,
    autocorrect_unbalance = TRUE
){

  # Initial Input check

  stopifnot(is.data.frame(dataset)||tibble::is_tibble(dataset))

  if(ncol(dataset) != 4){
    stop("The dataset must contain exactly four columns in this exact order: participant ID, independent variable, dependent variable, and weight.")
  }

  stopifnot(length(alpha) == 1, is.finite(alpha), alpha > 0, alpha < 1)
  stopifnot(is.logical(paired), length(paired)==1)

  # Decomposition
  dc <- dataset %>%
    dplyr::rename(s = 1, x = 2, y = 3, c = 4) %>%
    dplyr::mutate(m = mean(y)) %>%
    dplyr::group_by(x) %>%
    dplyr::mutate(a = mean(y) - m) %>%
    dplyr::ungroup()

  # Check contrast weights

  stopifnot(all(is.finite(dc$c)))

  chk <- dc %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(
      cval = dplyr::first(c),
      n = dplyr::n(),
      k = dplyr::n_distinct(c),
      .groups = "drop"
    )

  if(any(chk$k != 1))
    stop("Contrast weights must be constant within each condition.")

  # Convert condition coefficients -> observation coefficients
  if(autocorrect_unbalance && !paired && dplyr::n_distinct(chk$n) > 1){
    dc <- dc %>%
      dplyr::left_join(chk %>% dplyr::select(x, n), by="x") %>%
      dplyr::mutate(c = c / n) %>%
      dplyr::select(-n)
  }

  stopifnot(all(is.finite(dc$c)))

  # Data Check

  if(abs(mean(dc$c)) > 1e-5){
    dc$c <- dc$c - mean(dc$c)
    warning("Weights were centered because the sum of them was not zero.")
  }

  if(length(unique(dc$x)) <= 1) {
    stop("The second column (independent variable) must have two or more levels.")
  }

  if(paired){
    check <- dc %>%
      dplyr::group_by(s,x) %>%
      dplyr::summarise(n = n(), .groups = "drop") %>%
      tidyr::complete(s, x, fill = list(n = 0)) %>%
      dplyr::mutate(inapp = as.numeric(n != 1))
    if(sum(check$inapp) != 0){
      stop("Each participant must have exactly one observation per condition when paired = TRUE.")
    }
  }

  # Calculate contrast components

  dc_agg <- dc %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(
      y = mean(y),
      c = c[1],
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ac = stats::cov(c,y)/stats::var(c)*c,
      ac = ac - mean(ac)
    )

  # Joint

  dc2 <- dc %>%
    dplyr::left_join(
      dc_agg %>% dplyr::select(x, ac),
      by = "x"
    ) %>%
    dplyr::mutate(
      e = y - m - a,
      ared = a - ac
    )

  # Numerator statistics

  SS_total <- sum((dc2$y-mean(dc2$y))^2)
  SS_Ac <- sum(dc2$ac^2)
  nu_Ac <- 1

  # Calculation of Denominator statistics

  if(!paired){
    # Between-participants error
    SS_E <- sum(dc2$e^2)
    nu_E <- nrow(dc2) - length(unique(dc$x))

  } else if(paired){
    # Within-participants error
    dc3 <- dc2 %>%
      dplyr::group_by(s) %>%
      dplyr::mutate(
        e_s = mean(y) - m,
        e_sa = e - e_s,
        e_sac = stats::cov(e_sa, c)/stats::var(c)*c
      )

    SS_E <- sum(dc3$e_sac^2)
    nu_E <- length(unique(dc$s)) - 1
  }

  if(SS_E == 0) stop("Denominator SS is zero.")

  # Results

  out <- data.frame(effect = "contrast") %>%
    dplyr::mutate(
      df1 = nu_Ac, df2 = nu_E,
      SS1 = SS_Ac, SS2 = SS_E,
      MS1 = SS1 / df1, MS2 = SS2 / df2,
      Fv = MS1/MS2,
      p = pf(Fv, df1 = df1, df2 = df2, lower.tail = FALSE),
      sig = dplyr::if_else(p < alpha, "*", "ns"),
      eta2 = SS_Ac/SS_total,
      peta2 = SS_Ac/(SS_Ac + SS_E),
      cohens_f = sqrt(SS_Ac/SS_E)
    )

  # Do round

  if(do_round){
    out <- out %>%
      dplyr::mutate(
        df1 = df1 %>% round(1), df2 = df2 %>% round(1),
        SS1 = SS1 %>% round(2), SS2 = SS2 %>% round(2),
        MS1 = MS1 %>% round(2), MS2 = MS2 %>% round(2),
        Fv = Fv %>% round(2), p = p %>% round(3),
        eta2 = eta2 %>% round(3),
        peta2 = peta2 %>% round(3),
        cohens_f = cohens_f %>% round(3)
      )
  }

  # Omit unnecessary variables

  if(!inc_allvar){
    out <- out %>%
      dplyr::select(
        effect, df1, df2, Fv, p, sig, eta2, peta2, cohens_f
      )
  }

  out <- out %>% dplyr::rename(F = Fv)

  return(out)

}
