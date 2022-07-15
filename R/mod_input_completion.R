#' input_completion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_input_completion_ui <- function(id, pb_title = ""){
  
  ns <- NS(id)
  
  tagList(
    
    # Completeness progress bar  -----------------------------------------------
    fluidRow(
      col_3(
        div(
          shinyWidgets::progressBar(
            id = ns("pbcmpltn"),
            value = 0,
            size = "xs",
            status = "success",
            striped = FALSE,
            title = pb_title,
          ),
          class = "pb-inputcompletion"
        ),
        div(
          style = "font-size: 13px; font-style: italic", #float: right",
          textOutput(outputId = ns("cmpltstatus"))
        )
      )
    ),
    
    HTML("<hr style='height:1px;border:none;color: #b1b3b8; background-color:#b1b3b8; margin-top: 8px; margin-bottom: 13px' />"),
    
  )
}


    
#' input_completion Server Functions
#'
#' @import shinyvalidate
#'
#' @noRd 
mod_input_completion_server <- function(id, iv){
 
  moduleServer( id, function(input, output, session){

    ns <- session$ns
    
    # Completeness status for input parameters tracked by a given inputValidator object ------
    observe({
      
      n_pars <- length(iv$fields())
      
      faulty_pars <- drop_nulls(iv$validate())
      n_faulty <- length(faulty_pars)
      completion_pctg <- (n_pars - n_faulty)/n_pars * 100
      
      # Empty inputs have an empty string as message
      n_missing <- purrr::map(faulty_pars, ~.$message) |>
        purrr::keep(~ .x == "") |>
        length()
      
      n_errors <- n_faulty - n_missing
      
      # Update feedback text title
      if(n_errors > 0 & n_missing == 0){
        feedback <- glue::glue("{n_errors} error(s)")
      }else if(n_errors == 0 & n_missing > 0){
        feedback <- glue::glue("{n_missing} input(s) missing")
      }else if(n_errors > 0 & n_missing > 0){
        feedback <- glue::glue("{n_errors} error(s) + {n_missing} input(s) missing")
      }else{
        feedback <- glue::glue("All good!")  
      }
      
      # Update PB colour
      if (completion_pctg < 100) {
        status <- "danger"
      } else{
        status <- "success"
      }
      
      
      # Update PB
      shinyWidgets::updateProgressBar(
        id = "pbcmpltn",
        value = completion_pctg,
        status = status
      )
      
      output$cmpltstatus <- renderText({ feedback })
      
    })
    
 
  })
}
    
## To be copied in the UI
# mod_input_completion_ui("input_completion_ui_1")
    
## To be copied in the server
# mod_input_completion_server("input_completion_ui_1")
