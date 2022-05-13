#' spp_in_wf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spp_in_wf_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      shinydashboard::tabBox(
        id = ns("tbx-spp"),
        width = 12,
        title = uiOutput(ns("tbx-title-add-spp-btns"))
      )
    )
  )
}
 


   
#' spp_in_wf Server Functions
#'
#' @noRd 
mod_spp_in_wf_server <- function(id, band_mode, wf_label){
  
  stopifnot(is.reactive(band_mode))
  stopifnot(!is.reactive(wf_label))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # global variables
    prev_slct_spp <- c()
    
    # Reactives: Species
    rv_spp <- reactiveValues(
      added_spp = NULL
    )
    

    # Dynamic UI: Species Features -----------------------------------------------
    
    # Logic to pair-up with UI generation of species tabs: Get a first-time
    # selected species, returning empty character if currently selected species
    # have been previously selected
    observe({
      sltc_spp <- input$'slct-spp'
      rv_spp$added_spp <- sltc_spp[!sltc_spp %in% prev_slct_spp]
      prev_slct_spp <<- c(prev_slct_spp, rv_spp$added_spp)
    })
    
    # # --- Store selected species
    # slct_spp_ids <- reactive({
    #   req(input$'slct-spp')
    #   tibble::tibble(
    #     spp_label = input$'slct-spp',
    #     spp_id = label2id(spp_label)
    #   )
    # })
    
    # observe({
    #   cat("\nAdded species:\n")
    #   print(rv_spp$added_spp)
    #   cat("\nCurrently selected species\n")
    #   print(input$'slct-spp')
    # })
    # 
    
    # shinyWidgets::dropdown did not opened when this module was used
    # via shiny::insertUI nor shinyWidgets::verticalTab. **Eventually** managed to solve the issue
    # by rendering it on the server side... weird!
    output$"tbx-title-add-spp-btns" <- renderUI({
      add_spp_btns(
        slctize_id = ns("slct-spp"),
        btn_add_id = ns("btn-add-spp"),
        drpdwn_btn_id = ns("btn-upld-spps"),
        dwnl_btn_id = ns("dt-spp-inputs-tmpl"),
        file_input_id = ns("flinput-spp-inputs"),
        dpdn_close_id = ns("close-drpdwn-spp")
        )
    })
    
    
    # Append tabpanel for added species
    observeEvent(rv_spp$added_spp, {
      
      req(rv_spp$added_spp)
      
      spp_label <- rv_spp$added_spp
      spp_id <- label2id(spp_label)
      
      # Dynamically append tabPanel for a new species
      appendTab(
        inputId = "tbx-spp",
        select = TRUE,
        tabPanel(
          value = paste0("tbp-spp-", spp_id),
          title = tagList(
            strong(spp_label), 
            # Species panel remove button
            shinyWidgets::circleButton(
              inputId = ns(paste0("btn-rmv-spp-", spp_id)),
              size = "xs",
              status = "danger",
              icon = icon("remove", verify_fa = FALSE),
              class = "btn-rmv-tabPanel"
            )
          ),
          # TabPanel content
          wellPanel(
            style = "padding-top: 10px",
            mod_pnl_spp_ui(
              id = ns(paste0('pnl-spp-', spp_id)),
              spp_label = spp_label,
              band_mode = isolate(band_mode())
            )
          )
        )
      )
      
      # module for selected species' main tabPanel - Server side
      mod_pnl_spp_server(
        id = paste0('pnl-spp-', spp_id),
        spp_label = spp_label,
        band_mode = band_mode)
      
    })
    
    # Setting up the removing of spp tabPanels when the "remove" button is clicked
    # Can be a bit confusing, as this is a "promised" action, i.e.
    # the closing reaction is set at creation of the removing button
    observeEvent(rv_spp$added_spp, {

      spp_id <- label2id(rv_spp$added_spp)
      
      # reactivity on clicking of the spp removing button
      shinyjs::onclick(
        id = paste0("btn-rmv-spp-", spp_id),
        expr = {
          # remove tab
          removeTab(inputId = "tbx-spp", target = paste0("tbp-spp-", spp_id))
          
          # update selected spps in selectize widget
          upd_slct_spp <- drop_from_sltz(
            rmv_id = spp_id,
            c_slctd_labs = input$'slct-spp',
            inputId = "slct-spp")
          
          # Update previously selected species (essential to keep track of spps
          # that are deselected)
          prev_slct_spp <<- c(upd_slct_spp)
        })
    })
    
    # Remove tabPanels for species deselected from associated selectize widget
    observeEvent(input$'slct-spp', {
      
      sltc_spp <- input$'slct-spp'
      rmv_spp_label <- setdiff(prev_slct_spp, sltc_spp)
      
      if(length(rmv_spp_label) > 0 ){
        # get remaining selected species
        rmv_spp_id <- label2id(rmv_spp_label)
        # remove tabPanel
        removeTab(inputId = "tbx-spp", target = paste0("tbp-spp-", rmv_spp_id)) 
        prev_slct_spp <<- c(sltc_spp)
      }
    })

  })
}

## To be copied in the UI
# mod_spp_in_wf_ui("spp_in_wf_ui_1")
    
## To be copied in the server
# mod_spp_in_wf_server("spp_in_wf_ui_1")
