#' intro_guide 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' 
#' @import cicerone
#'
#' @noRd
intro_guide <- function(){
  
  Cicerone$
    new()$ 
    step(
      el = "sidebarmenu",
      title = "Yo here",
      description = "This is where you chose the CRM mode"
    )
}