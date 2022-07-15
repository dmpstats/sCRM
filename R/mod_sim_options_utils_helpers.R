#' Assign icons to status of inputs
#' 
#' @param valid_status Boolean, is the input valid?
#'
#' @noRd
status_icon <- function(valid_status, level = 0){
  
  if(valid_status == TRUE){
    if(level == 0){
      icon("check-circle",  style ="color: green", class = "fa-lg")
    }else{
      icon("check", style ="color: green;", class = "fa-lg")
    }
    
  }else{
    if(level == 0){
      icon("exclamation-triangle", style = "color: #d40404;", class = "fa-lg")  
      #icon("exclamation-circle", style = "color: #d40404;", class = "fa-lg")  
    }else{
      tags$a(
        icon(
          "times", 
          # class = "fa-lg fa-bounce", 
          # style = "color: #d40404;  --fa-bounce-rebound: 0; --fa-bounce-height: -4px",
          class = "fa-lg fa-beat-fade",
          style = "color: #d40404; --fa-beat-fade-opacity: 0.7; --fa-beat-fade-scale: 1.15;"
        ), 
        href = "#"
      )
    }
  }
  
}