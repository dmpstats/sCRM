#' info_dropdown 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
info_dropdown <- function(inputId, placement, trigger = "mouseenter", 
                          md_path){
  
  shinyWidgets::dropMenu(
    tag = actionLink(
      inputId = inputId,
      label = "",
      icon = icon("info-circle", class = "fa-xl")
    ),
    placement = placement,
    trigger = trigger,
    arrow = TRUE,
    theme = "light",
    options = shinyWidgets::dropMenuOptions(
      padding = 0
      #stiky = TRUE
    ),
    div(
      style = "width: 450px; font-size: 13px;",
      includeMarkdown(md_path)
    ) 
  )
}