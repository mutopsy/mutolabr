#' A wrapper for anovakun (version 4.8.9) with improved usability and formatting
#'
#' This function serves as a wrapper around \code{\link{anovakun}} (version 4.8.9),
#' streamlining common usage patterns in mutolab and formatting the results for easier interpretation.
#'
#' While the core computation is delegated to the original \code{anovakun} function, this wrapper
#' automatically infers the number of factor levels, simplifies output, and adds formatting options such as rounding.
#'
#' For full details of the original implementation of anovakun, refer to:
#' \url{https://riseki.cloudfree.jp/?ANOVA%E5%90%9B}
#'
#' @param dataset A data frame containing the input data in long (tidy) format.
#' @param design A character string specifying the experimental design (e.g., "As", "ABs", "sA", "sAB", "AsB", etc.).
#' @param ... Additional arguments passed to \code{\link{anovakun}}. The number of levels for each factor
#'   is automatically calculated from \code{dataset} and \code{design}, and does not need to be specified manually.
#' @param alpha Significance level. Defaults to 0.05.
#' @param inc_allvar Logical. If \code{TRUE}, all columns will be included in the output. Defaults to \code{FALSE}.
#' @param do_round Logical. If \code{TRUE}, numeric values in the output will be rounded for readability. Defaults to \code{TRUE}.
#'
#' @return A data frame containing the results of ANOVA, including F-statistics, p-values, effect sizes,
#' and sphericity indices (if applicable).
#'
#' @examples
#' data_snakemr %>%
#'   anovakun_tidy("sABC")
#'
#' @seealso \code{\link{anovakun}} for the original function wrapped by this helper.
#'
#' @export

anovakun_tidy <- function(
    dataset,
    design,
    alpha = 0.05,
    inc_allvar = FALSE,
    do_round = TRUE
    ){

  ncol <- ncol(dataset)

  nfct <- dataset[,2:(ncol-1)] %>%
    dplyr::summarise(across(everything(), ~ length(unique(.)))) %>%
    unlist() %>%
    as.numeric()

  text_nfct <- nfct %>% paste(collapse = "*")

  res <- do.call(
    anovakun,
    c(
      list(dataset, design),
      as.list(nfct),
      list(long = TRUE, nopost = TRUE, eta = TRUE, peta = TRUE, geta = TRUE, tech = TRUE, cm = TRUE)
    )
  )

  sph_df <- res$`SPHERICITY INDICES`[[2]]

  inc_between <- as.numeric(substr(design, 1, 1) != "s")
  inc_within <- as.numeric(substr(design, nchar(design), nchar(design)) != "s")

  if(!is.null(sph_df)){
    sph <- sph_df %>%
      dplyr::select(effect = Effect, epsilon_CM = CM)
  } else{
    sph <- data.frame(effect = NA, epsilon_CM = NA)
  }

  atbl <- res$`ANOVA TABLE`[[2]] %>%
    dplyr::rename(
      effect = source.col,
      SS1= ss.col,
      df1 = df.col,
      MS1 = ms.col,
      F = f.col,
      p = p.col,
      sig = sig.col,
      eta2 = "eta^2",
      peta2 = "p.eta^2",
      geta2 = "G.eta^2"
    ) %>%
    dplyr::filter(effect != "Total", effect != "s") %>%
    dplyr::mutate(
      is_error = if_else(substr(effect,1,3) == "s x", 1, 0),
      is_error = if_else(effect == "Error", 1, is_error),
      SS2 = if_else(is_error == 1, SS1, NA_real_),
      df2 = if_else(is_error == 1, df1, NA_real_),
      MS2 = if_else(is_error == 1, MS1, NA_real_)
    ) %>%
    dplyr::mutate(
      df2 = rev(
        tidyr::fill(tibble(x = rev(df2)), x, .direction = "down")$x
      ),
      SS2 = rev(
        tidyr::fill(tibble(x = rev(SS2)), x, .direction = "down")$x
      ),
      MS2 = rev(
        tidyr::fill(tibble(x = rev(MS2)), x, .direction = "down")$x
      ),
    ) %>%
    dplyr::filter(is_error == 0) %>%
    dplyr::select(-is_error) %>%
    dplyr::mutate(
      cohens_f = sqrt(SS1/SS2),
      sig = if_else(p < alpha, "*", "ns")
    ) %>%
    dplyr::select(
      effect, df1, df2, SS1, SS2, MS1, MS2, F, p, everything()
    ) %>%
    dplyr::left_join(sph, by = "effect") %>%
    dplyr::mutate(epsilon_CM = if_else(is.na(epsilon_CM), 1, epsilon_CM))

  if(do_round){
    atbl <- atbl %>%
      dplyr::mutate(
        df1 = df1 %>% round(2),
        df2 = df2 %>% round(2),
        SS1 = SS1 %>% round(2),
        SS2 = SS2 %>% round(2),
        MS1 = MS1 %>% round(2),
        MS2 = MS2 %>% round(2),
        F = F %>% round(2),
        p = p %>% round(3),
        eta2 = eta2 %>% round(3),
        peta2 = peta2 %>% round(3),
        geta2 = geta2 %>% round(3),
        cohens_f = cohens_f %>% round(3),
        epsilon_CM = epsilon_CM %>% round(3)
      )
  }

  if(!inc_allvar){
    atbl <- atbl %>%
      dplyr::select(effect, df1, df2, F, p, sig, peta2, cohens_f, epsilon_CM)

  }

  if(!inc_within){
    atbl <- atbl %>% dplyr::select(-epsilon_CM)
  }

  if(inc_between & inc_within){
    text_design <- "mixed-design"
  } else if(inc_between){
    text_design <- "between-participants"
  } else if(inc_within){
    text_design <- "within-participants"
  } else{
    text_design <- "unknown"
  }

  cat(
    paste(
      "A", text_nfct, text_design, "ANOVA was performed.\n",
      sep = " "
    )
  )

  return(atbl)
}


if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "Effect", "CM", "source.col", "ss.col", "df.col", "ms.col",
    "f.col", "p.col", "sig.col", "effect", "is_error", "SS1", "df1",
    "MS1", "df2", "x", "SS2", "MS2", "p", "epsilon_CM", "eta2",
    "peta2", "geta2", "sig"
  ))
}

