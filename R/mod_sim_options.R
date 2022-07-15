#' sim_options UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sim_options_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    p(strong("Inputs Status")),
    reactable::reactableOutput(
      outputId = ns("inputs-status"),
      width = "100%"
    ),
    
    rep_br(2),
    
    numericInput(
      inputId = ns("seed"),
      label = "Random Seed", 
      value = NULL
    ),
    
    br(),
    
    sliderInput(
      inputId = ns("niter"),
      label = "Number of Iterations",
      min = 1000,
      max = 5000,
      step = 100,
      sep = "",
      value = 1000),

    
    rep_br(1),
    shinyjs::disabled(
      shinyWidgets::actionBttn( 
        inputId = ns("run-scrm"), 
        label = div(
          #icon("circle-play", verify_fa = FALSE, class = "fa-lg"),
          icon("play", class = "fa-lg"), 
          p("Run CRM", style = "font-size: 13px; margin-bottom: 0px")
        ),
        #icon = icon("circle-play", verify_fa = FALSE, class = "fa-lg"),
        color = "success",
        style = "simple",
        #style = "fill",
        size = "lg",
        block = TRUE
      )
    )
  )
}
 




   
#' sim_options Server Functions
#' 
#' @import shinyvalidate
#' 
#' @noRd 
mod_sim_options_server <- function(id, iv_status, app_session, band_mode){
  
  stopifnot(is.reactive(iv_status))
  stopifnot(is.reactive(band_mode))
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    
    # Initialize input validation for current module
    iv <- InputValidator$new()
    
    ## Random seed validation ------
    iv$add_rule("seed", sv_optional())
    iv$add_rule("seed", sv_numeric())
    iv$add_rule("seed", sv_gt(0))
    
    iv$enable()
    
    
    # Render table with input validation summary ------------------------------- 
    output$'inputs-status' <- reactable::renderReactable({

      req(iv_status())
      
      #browser()
      
      if(nrow(iv_status()) == 1){
        
        iv_status() %>%
          reactable::reactable(
            theme = reactable::reactableTheme(borderColor = "black"),
            style = list(
              fontSize = "12px"
            ),
            columns = list(
              #section =  reactable::colDef(
              #   #name = "Input Section", 
              #   minWidth = 90
              # ),
              input_valid = reactable::colDef(
                #name = "", 
                minWidth = 40, 
                align = "center",
                cell = function(value, index) status_icon(value, level = 0)
              )
            ),
            outlined = TRUE, 
            # hide column headers via css
            class = "hidden-column-headers",
          )
        
      }else{
        
        iv_status_nested <- iv_status() %>%
          dplyr::group_by(scenario) %>%
          # omit species selection status if valid, for simplicity
          #dplyr::filter(input_valid == FALSE section == "Species selection" && input_valid == FALSE) %>%
          # Assess validation for all input sections of a wf scenario
          dplyr::mutate(scenario_status = all(input_valid == TRUE)) %>%
          # nest by scenario
          tidyr::nest(
            section_status = c(section, input_valid, menu_item_id, tbox_id, tb_pnl_id)
          )
        
        
        iv_status_nested %>%
          dplyr::select(scenario, scenario_status) %>%
          reactable::reactable(
            outlined = TRUE, 
            highlight = TRUE,
            theme = reactable::reactableTheme(borderColor = "black"),
            columns = list(
              scenario = reactable::colDef(
                #name = "Scenario", 
                minWidth = 90, 
                style = "font-weight: bold"),
              scenario_status = reactable::colDef(
                #name = "Status", 
                minWidth = 40, 
                align = "center",
                headerStyle = "display: none",
                cell = function(value) status_icon(value, level = 0)
              )
            ), 
            style = list(
              fontSize = "12px"
            ), 
            
            details = function(index){
              
              iv_status_nested$section_status[[index]] %>%
                reactable::reactable(
                  theme = reactable::reactableTheme(backgroundColor = "rgb(244, 249, 255)"),
                  columns = list(
                    menu_item_id = reactable::colDef(show = FALSE),
                    tbox_id = reactable::colDef(show = FALSE),
                    tb_pnl_id =reactable::colDef(show = FALSE),
                    section =  reactable::colDef(
                      #name = "Input Section", 
                      minWidth = 90
                    ),
                    input_valid = reactable::colDef(
                      #name = "", 
                      minWidth = 40, 
                      align = "center",
                      cell = function(value, index) status_icon(value, level = 1)
                    )
                  ),
                  outlined = TRUE,
                  highlight = TRUE,
                  onClick = htmlwidgets::JS(
                    # click action to point to panel with non-valid inputs
                    paste0(
                      "function(rowInfo, column) {
                // Only handle click events on the 'input_valid' column
                  if (column.id !== 'input_valid') {
                    return 
                  }
                
               // Send the click event to Shiny, which will be available in input$go_to_panel
               if (window.Shiny) {
                  Shiny.setInputValue('", ns('go_to_panel'), "', { rowdata: rowInfo.row}, { priority: 'event' })
               }
               }")
                  )
                )
            },
            # hide column headers via css
            class = "hidden-column-headers",
            defaultExpanded = TRUE
          )
      }
      
    })

    
    
    # Logic to open input section clicked by the user in input status table  -----
    observeEvent(input$go_to_panel, {

      # 1. Go to the (sidebar) menu item
      shinydashboard::updateTabItems(
        session = app_session,
        inputId = "sidebarmenu",
        selected = input$go_to_panel$rowdata$menu_item_id
      )

      # 2. Go to the tabPabel in the tabBox
      updateTabsetPanel(
        session = app_session,
        inputId = input$go_to_panel$rowdata$tbox_id,
        selected = input$go_to_panel$rowdata$tb_pnl_id
      )

    })
    
    
    
    # Toggle state of simulation run button, given status of input validation ----
    observe({
      if(nrow(iv_status()) > 0){
        shinyjs::toggleState("run-scrm", condition = all(iv_status()$input_valid))  
      }
    })
    
    
    
    # Toggle widgets for random seed and number of iteration, given band_mode ---
    observe({
      shinyjs::toggleState("seed", condition = !band_mode())
      shinyjs::toggleState("niter", condition = !band_mode()) 
    })

    
    # Module Output ------------------------------------------------------------
    list(
      go_sim = reactive(input$"run-scrm"),
      rseed = reactive(input$seed),
      niter = reactive(input$niter)
    )
    

 
  })
}
    
## To be copied in the UI
# mod_sim_options_ui("sim_options_ui_1")
    
## To be copied in the server
# mod_sim_options_server("sim_options_ui_1")
