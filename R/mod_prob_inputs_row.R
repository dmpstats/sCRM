#' prob_inputs_row UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_prob_inputs_row_ui <- function(id, par_label, dflt_mean, dflt_sd){
  ns <- NS(id)
  
  tagList(
    tags$tr(
      tags$th(
        style = "text-align: left;",
        as.character(par_label)
      ),
      tags$td(
        tagAppendAttributes(
          numericInput(
            ns("mean"), 
            label = NULL, 
            value = dflt_mean),
          style = "margin-bottom: 0px"
        )
      ),
      tags$td(
        tagAppendAttributes(
          numericInput(
            inputId = ns("sd"), 
            label = NULL, 
            value = dflt_sd),
          style = "margin-bottom: 0px"
        )
      ),
      tags$td(
        shinyWidgets::dropMenu(maxWidth = "350px",
          actionButton(ns("plotbtn"), "", icon = icon("chart-area")),
          plotOutput(ns("dplot"), width = "300px", height = "200px"),
          br(),
          verbatimTextOutput(ns("qtls")),
          placement = "right")
      )
    )
  )
}
    
#' prob_inputs_row Server Functions
#'
#' @noRd 
mod_prob_inputs_row_server <- function(id, par_label, par_dist, par_name, band_mode){
  
  stopifnot(!is.reactive(par_label))
  stopifnot(!is.reactive(par_dist))
  stopifnot(!is.reactive(par_name))
  stopifnot(is.reactive(band_mode))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$plotbtn,{
      
      if(par_dist == "tnorm") {
        
        output$dplot <- renderPlot({
          tnorm_dplots(mean = input$mean, 
                       sd = input$sd, 
                       lower = 0,
                       xlab = par_label)
        })
      
        output$qtls <- renderPrint({
          tnorm_qtl_tbl(mean = input$mean,
                        sd = input$sd,
                        lower = 0,
                        varTag = par_name)
        })
         
      }else if(par_dist == "beta"){
        
        print("TO DO")
        
      }
    })
    
    observe({
      shinyjs::toggleState(id = "sd", condition = !band_mode())
      shinyjs::toggleState(id = "plotbtn", condition = !band_mode())
      shinyjs::toggle(id = "dplot", condition = !band_mode())
    })
    
  })
}
    
## To be copied in the UI
# mod_prob_inputs_row_ui("prob_inputs_row_ui_1")
    
## To be copied in the server
# mod_prob_inputs_row_server("prob_inputs_row_ui_1")
