#' Perform Trend Analysis Using Contrast Test
#'
#' This function conducts a trend analysis by applying contrast tests to a given dataset.
#'
#' @param dat A data frame or matrix containing the data. Columns represent different levels of the independent variable.
#' @param order An integer vector specifying the trend orders to test. Defaults to `1:3`.
#' @param paired Logical. If `TRUE`, a paired contrast test is performed. Defaults to `FALSE`.
#' @param alternative A character string specifying the alternative hypothesis. One of `"two.sided"`, `"less"`, or `"greater"`. Defaults to `"two.sided"`.
#' @param conf.level Confidence level for the test. Defaults to `0.95`.
#' @param verbose Logical. If `TRUE`, displays messages. Defaults to `TRUE`.
#'
#' @details
#' The function constructs contrast vectors for specified trend orders using polynomial contrasts (`contr.poly`).
#' Then, `contrast_test()` is applied to test each contrast.
#' For more details on the testing procedure, refer to the documentation of [`contrast_test()`].
#'
#' @return
#' A data frame containing the results of the contrast tests, with an additional column indicating the trend order.
#'
#' @seealso
#' [`contrast_test()`] for details on the contrast testing procedure.
#'
#' @examples
#' # Example dataset with 5 conditions
#' dat <- matrix(rnorm(50), nrow = 10, ncol = 5)
#'
#' # Perform trend analysis for 1st and 2nd order trends
#' trend_test(dat, order = 1:2)
#'
#' @importFrom dplyr bind_rows mutate
#' @import tidyr
#' @export

trend_test <- function(
    dat,
    order = 1:3, paired = FALSE,
    alternative = c("two.sided", "less", "greater"),
    conf.level = 0.95, verbose = TRUE
){

  if(is.matrix(dat)) dat <- as.data.frame(dat)

  alternative <- alternative[1]
  nvar <- ncol(dat)

  if(is.null(order)) order <- 1:(nvar-1)

  if(nvar <= 3){
    order <- order[order <= (nvar-1)]
  }

  if(max(order) > (nvar-1)){
    stop("One or more elements in 'order' is too large!")
  }

  i<-1

  weight <- round(t(contr.poly(nvar)),5)
  rownames(weight) <- NULL

  out <- list()
  tol <- 0.005

  for(i in 1:length(order)){
    w <- weight[order[i],]
    w[abs(w)<tol] <- 0
    w <- w / min(abs(w[w!=0]))

    denom <- 0
    for (d in 1:500) {
      if (max(abs(w * d - round(w * d))) < tol){
        denom <- d
        w <- round(w*denom,0)
        break
      }
    }

    if(denom == 0){
      w <- w - mean(w)
    }

    out[[i]] <- contrast_test(
      dat = dat, weight = w, paired = paired, alternative = alternative,
      conf.level = conf.level, verbose = FALSE
    )
  }

  out <- out %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(order = order, .before = 1)

  return(out)
}
