#' spp_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spp_results_ui <- function(id, out_opt, band_mode){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      tagAppendAttributes(
        shinyWidgets::radioGroupButtons(
          inputId = ns("plt_tbl_switch"),
          label = "", 
          choices = c(
            `<i class='fa fa-chart-area'></i>` = "plot", 
            `<i class='fa fa-table'></i>` = "table"
          )
          #status = "primary"
          #justified = TRUE
        ), 
        style = "float:right; margin-right: -18px; margin-top: -33px;"
        #style = "margin-top: -30px;"
      ),
    ),
    
    fluidRow(
      conditionalPanel(
        condition = "input.plt_tbl_switch == 'plot'", 
        ns = ns,
        plotOutput(
          outputId = ns("collision_plot"), 
          height = ifelse(out_opt == "months" & band_mode == FALSE, 800, 450)
        )
      ),
      
      conditionalPanel(
        condition = "input.plt_tbl_switch == 'table'", 
        ns = ns,
        uiOutput(ns("collision_tbl"))
      )
    )
    
  )
}
    
#' spp_results Server Functions
#'
#' @noRd 
mod_spp_results_server <- function(id, coll_plot, coll_summ_ft){
  

  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # initialize waiter
    w <- waiter::Waiter$new(
      id = ns("collision_plot"),
      html = waiter::spin_loaders(15, color = "#434C5E") #f37403")
    )
    
    # render plot
    output$collision_plot <- renderPlot({
      w$show()
      coll_plot
    })
    
    
    # render table
    output$collision_tbl <- renderUI({
      coll_summ_ft %>%
        flextable::htmltools_value(ft.htmlscroll = FALSE)
    })
 
  })
}
 



   
## To be copied in the UI
# mod_spp_results_ui("spp_results_ui_1")
    
## To be copied in the server
# mod_spp_results_server("spp_results_ui_1")
