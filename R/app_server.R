#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
 
  x <- environment()
  x$y <- 0
  
  # Reactives: windfarm scenarios
  rv_wf <- reactiveValues(
    # remove buttons
    tbp_scn_rm_btn = NULL,
    counter = 1L
    )
  
  
  # Dynamic UI -----------------------------------------------------------------
  
  # show landing modal
  observeEvent("", {
    showModal(modalDialog(
      easyClose = TRUE,
      includeMarkdown( app_sys("inst/app/www/landing_intro_text.md") )
      # footer = tagList(
      #   actionButton(inputId = "intro", label = "INTRODUCTION TOUR",
      #                icon = icon("info-circle"))
      #   
      #)
    ))
  })
  
  # Add windfarm scenario tabPanel
  observeEvent(input$'btn-add-wf', {
    
    rv_wf$counter <- rv_wf$counter + 1L
    wf_id <- rv_wf$counter

    # Dynamically append tabPanel for a new scenario
    appendTab(
      inputId = "tbx-wf",
      select = TRUE,
      tabPanel(
        value = paste0("tbp-wf-", wf_id),
        title = tagList(
          strong(paste("Scenario", wf_id)), 
          shinyWidgets::circleButton(
            inputId = paste0("btn-rmv-wf-", wf_id),
            size = "xs",
            status = "danger",
            icon = icon("remove", verify_fa = FALSE), #icon("minus")
            class = "css-btn-rmv-tabPanel"
          )
        ),
        wellPanel(
          h4(strong(sprintf("Wind farm scenario %s", wf_id))),
          # TabPanel content
          fluidRow(
            # module for added wf main tabPanel content - UI side
            mod_pnl_wf_ui(
              id = paste('pnl-wf', wf_id, sep = "-")
            )
          )
        )
      )
    )
    
    # module for added wf main panel content - server side
    mod_pnl_wf_server(
      paste('pnl-wf', wf_id, sep = "-"),
      band_mode = reactive(input$'swtc-band-mode')
    )
    
  })
  
  # Panel content for first WF scenario
  mod_pnl_wf_server(
    paste('pnl-wf-1'), 
    band_mode = reactive(input$'swtc-band-mode'))

  
  # Observer for removing tabPanels
  observeEvent(input$'js-btn-clicked', {
    
    #browser()
    
    # extract input id of clicked button
    clicked_btn_id <- stringr::str_split(input$'js-btn-clicked', "_")[[1]][1]
    
    if(clicked_btn_id == ""){
      message("\nUnidentified button\n")
    }else{
      
      # list with button tags
      btn_tags <- unlist(stringr::str_split(clicked_btn_id, "-"))
      
      action <- btn_tags[2]
      
      if(action == "rmv"){
        # current ui input ids (excluding those "nullified")
        valid_inputs_ids <- names(
          Filter(Negate(is.null), reactiveValuesToList(input))
        )
        # remove tabPanel
        remove_tbp(btn_tags, valid_inputs_ids)
      }
    }
    
  })
  
  
  # observeEvent(input$remove, {
  #   removeTab(inputId = "tabs", target = "Foo")
  # })
  
}
