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
            title = pb_title, #"Demo Windfarm Parameters", #"Completion Status"
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
      
      # Update PB title
      if(n_errors > 0 & n_missing == 0){
        title <- glue::glue("Completion Status: {n_errors} error(s)")
      }else if(n_errors == 0 & n_missing > 0){
        title <- glue::glue("Completion Status: {n_missing} input(s) missing")
      }else if(n_errors > 0 & n_missing > 0){
        title <- glue::glue("Completion Status: {n_errors} error(s) + {n_missing} input(s) missing")
      }else{
        title <- glue::glue("Completion Status: All good!")  
      }
      
      # Update PB colour
      # if (completion_pctg < 70) {
      #   status <- "danger"
      # } else if (completion_pctg >= 70 & completion_pctg < 80) {
      #   status <- "warning"
      # } else if(completion_pctg >= 80 & completion_pctg < 99){
      #   status <- "info"
      # } else{
      #   status <- "success"
      # }
      
      if (completion_pctg < 100) {
        status <- "danger"
      } else{
        status <- "success"
      }
      
      
      # Update PB
      shinyWidgets::updateProgressBar(
        id = "pbcmpltn",
        value = completion_pctg,
        status = status,
        #title = title
      )
      
      
      output$cmpltstatus <- renderText({
        stringr::str_replace(title, "Completion Status:", "") 
      })
      
      
    })
    
 
  })
}
    
## To be copied in the UI
# mod_input_completion_ui("input_completion_ui_1")
    
## To be copied in the server
# mod_input_completion_server("input_completion_ui_1")
