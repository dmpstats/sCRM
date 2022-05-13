#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  
  #prev_slct_spp <- prev_actv_wf <- c()
  prev_actv_wf <- c()
  
  # Reactives: windfarm scenarios
  rv_wf <- reactiveValues(
    # remove buttons
    #tbp_scn_rm_btn = NULL,
    counter = 1L,
    added_wf = NULL
  )
  
  # # Reactives: Species
  # rv_spp <- reactiveValues(
  #   added_spp = NULL
  # )
  
  
  
  # ----- Dynamic UI: Landing Modal --------------------------------------------
  
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
    
  

  # ----- Dynamic UI: Wind farm Scenarios --------------------------------------
  
  # Tack just added and previously added wfs
  observeEvent(input$'active-wfs', {
    
    active_spp <- input$'active-wfs'
    rv_wf$added_wf <- active_spp[!active_spp %in% prev_actv_wf]
    prev_actv_wf <<- c(prev_actv_wf, rv_wf$added_wf)
    
  })
  
  # observe({
  #   cat("\nAdded windfarm:\n")
  #   print(rv_wf$added_wf)
  #   cat("\nCurrently active windfarms\n")
  #   print(input$'active-wfs')
  # })
  # 
  
  # Append tabPanels for added windfarms in windfarm section
  observeEvent(rv_wf$added_wf, {
  
    req(rv_wf$added_wf)
    
    wf_id <- label2id(rv_wf$added_wf)
    
    # flag a demo wf
    is_demo <- stringr::str_detect(wf_id, "(D|d)emo")
    
    appendTab(
      inputId = "tbx-wf",
      select = TRUE,
      tabPanel(
        value = paste0("tbp-wf-", wf_id),
        title = tagList(
          strong(rv_wf$added_wf), 
          shinyWidgets::circleButton(
            inputId = paste0("btn-rmv-wf-", wf_id),
            size = "xs",
            status = "danger",
            icon = icon("remove", verify_fa = FALSE), #icon("minus")
            class = "btn-rmv-tabPanel"
          )
        ),
        # TabPanel content
        wellPanel(
          style = "padding-top: 10px",
          #fluidRow(
            #col_12(
              #h4(strong(sprintf("%s parameters", rv_wf$added_wf))),
              # module for added wf main tabPanel content - UI side
              mod_pnl_wf_ui(
                id = paste0('pnl-wf-', wf_id),
                band_mode = input$'swtc-band-mode',
                is_demo = is_demo
              )
            #)
          #)
        )
      )
    )
    
    # module for added wf main panel content - server side
    mod_pnl_wf_server(
      paste0('pnl-wf-', wf_id),
      band_mode = reactive(input$'swtc-band-mode'),
      is_demo = is_demo
    )
    
    # # reset scenario label to an empty field
    # updateTextInput(inputId = 'wf-label', value = NA)
    
  })
  
  
  # # Panel content for default WF scenario
  # mod_pnl_wf_server(
  #   id = 'pnl-wf-1', 
  #   band_mode = reactive(input$'swtc-band-mode'))


  
  # Render menuSubItem on sidebar for species within active windfarms
  output$subItems_spps_in_wf <- renderMenu({
    
    req(input$'active-wfs')
    
    wf_menuSubItems <- input$'active-wfs' %>% 
      purrr::map(function(x){
        shinydashboard::menuSubItem(
          text = x, 
          tabName = paste0("sbsm-sppinwf-", label2id(x)), 
          icon = icon("angle-right", verify_fa = FALSE)
          )
      })
    
    shinydashboard::sidebarMenu(
      .list = wf_menuSubItems
      )
  })

  
  # Generate tabItem for species in a given added windfarm
  observeEvent(rv_wf$added_wf, {
    
    req(rv_wf$added_wf)
    
    wf_id <- label2id(rv_wf$added_wf)
    
    insertUI(
      selector = "#tabItemsEnvelope",
      where = "beforeEnd",
      multiple = TRUE,
      immediate = TRUE,  
      ui = shinydashboard::tabItem(
        tabName = paste0("sbsm-sppinwf-", wf_id),
        # paste0("Here goes the module for species in wf ", wf_id)
        # module for spp in wf
        mod_spp_in_wf_ui(id = paste0("pnl-sppinwf-", wf_id))
      )
    ) 
    
    # module for spp in default wf - server side
    mod_spp_in_wf_server(
      id = paste0("pnl-sppinwf-", wf_id), 
      wf_label = rv_wf$added_wf,
      band_mode = reactive(input$'swtc-band-mode')
      )
    
  })
  
  
  
  # close dropdown menu with wf addition widget
  observeEvent(input$'drpdwn-close', {
    session$sendCustomMessage("close_drpdwn-add-wf", "")
  })
  
  
  
  # Dynamic UI: remove tabpanels  ----------------------------------------------
  
  # Observer for removing tabPanels via the action button
  observeEvent(input$'js-btn-clicked', {
    
    # extract input id of clicked button
    clicked_btn_id <- stringr::str_split(input$'js-btn-clicked', "\\|")[[1]][1]
    
    if(clicked_btn_id == ""){
      message("\nUnidentified button\n")
    }else{
      
      # list with button tags
      btn_tags <- unlist(stringr::str_split(clicked_btn_id, "-"))
      
      action <- btn_tags[2]
      section <- btn_tags[3]
      section_id <- btn_tags[4]
      
      if(action == "rmv"){
        # current ui input ids (excluding those "nullified")
        valid_inputs_ids <- names(
          Filter(Negate(is.null), reactiveValuesToList(input))
        )
        # remove tabPanel
        remove_tbp(btn_tags, valid_inputs_ids)
        
        # For species panels, update list of selected species in associated selectize
        if(section == "spp"){
          
          upd_slct_spp <- drop_from_sltz(
            rmv_id = section_id, 
            c_slctd_labs = input$'slct-spp', 
            inputId = "slct-spp")

          prev_slct_spp <<- c(upd_slct_spp)
        }
        
        # For windfarm panels, update list of active wfs in associated selectize
        if(section == "wf"){
          
          upd_actv_wf <- drop_from_sltz(
            session = session,
            rmv_id = section_id, 
            c_slctd_labs = input$'active-wfs', 
            inputId = "active-wfs", 
            drop_from_choices = TRUE)
          
          # reset previous selected species
          prev_actv_wf <<- c(upd_actv_wf)
        }
        
      }
    }
  })

  
  # Remove tabPanels for winfarms deselected from associated selectize widget
  observeEvent(input$'active-wfs', {
    
    actv_wf <- input$'active-wfs'
    rmv_wf_label <- setdiff(prev_actv_wf, actv_wf)
    
    if(length(rmv_wf_label) > 0 ){
      # get remaining selected species
      rmv_wf_id <- label2id(rmv_wf_label)
      # remove tabPanel
      removeTab(inputId = "tbx-wf", target = paste0("tbp-wf-", rmv_wf_id)) 
      prev_actv_wf <<- c(actv_wf)
    }
  })
  
  
}
