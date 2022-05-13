#' ggtheme_sCRM 
#'
#' @description A simple, light ggplot2 theme to display plots in the sCRM app
#'
#' @return A named list in the format of [ggplot2::theme()]
#'
#' @import ggplot2
#' 
#' @noRd
theme_scrm <- function(){
  
  theme_bw() + 
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11)
      )
  
}