#' A Default ggplot2 Theme
#'
#' Returns a simplified ggplot2 theme based on \code{theme_bw()}, with grid lines removed,
#' black axis text, and no legend title. This theme is useful for producing cleaner, publication-ready plots.
#'
#' @return A \code{ggplot2} theme object.
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   theme_gg0()
#' @export

theme_gg0 <- function(){
  out <- theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(color = "black"),
      legend.title = element_blank()
  )
  return(out)
}
