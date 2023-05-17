#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#'     
#'     
#' @import shiny
#' @import shinyvalidate
#' @import zeallot
#'
#' @noRd
app_server <- function( input, output, session ) {
  
  # Application server logic

  # Variable initialization ----------------------------------------------------
  
  ## Reactives ----------------------------------------
  
  # Global reactive to act as an internal database to store all the inputs for the sCRM
  # simulation
  scrm_inputs <- reactiveValues()
  
  wf_inputs <- reactiveValues()
  spps_in_wf_dt <- reactiveValues()
  #actv_spps <- reactiveValues()
  #spps_in_wf_inputs <- reactiveValues()
  
  # windfarm scenarios
  rv_wf <- reactiveValues(added_wf = NULL)

  
  ## local globals -------------------------------------
  prev_actv_wf <- c()

  ## App loader ----------------------------------------
  hostess <- Hostess$new("app_loader", infinite = TRUE)
  hostess$start()
  
  #g <- waiter::Garcon$new(image = "myImage", opacity = 0.2, filter = "blur")
  #g$set(15)
    
  ## Waiter loading panel for wf tabs  --------------
  w <- waiter::Waiter$new(
    id = "waiter-content", 
    #html = waiter::spin_pulsar(),
    html = div(
      class = "waiter-input-panel", 
      img(src = "www/wf_loading.gif", height = 120)
    ),
    fadeout = TRUE
  )

   
  # Input Validation -----------------------------------------------------------

  ## Initialize input validator variable
  iv <- InputValidator$new()

  # add rule to chech if at least one wf is specified
  iv$add_rule("active-wfs", sv_required(message = ""))


  ## Raise message when no windfarm scenario is specified -------
  observe({
    shinyjs::toggle(
      id = "no_wf_fdbck",
      condition = is.null(input$'active-wfs')
    )
  })

  ## Start displaying error feedback in UI  ---------------------
  iv$enable()


  ## Store iv state regarding with windfarm(s) having been specified
  observe({
    scrm_inputs$valid <- iv$is_valid()
  })

   
  # ----- Dynamic UI: Landing Modal --------------------------------------------

  observeEvent("", {
    showModal(
      tagAppendAttributes(
      modalDialog(
        easyClose = TRUE,
        includeMarkdown( app_sys("app/markdown/landing_intro_text.md") ), 
        size = "l",
        # footer = tagList(
        #   shinyWidgets::actionBttn(
        #     inputId = "guidedtour", 
        #     label = "Guided Tour", color = "warning", style = "simple", size = "md",
        #     icon = icon("info-circle")
        #     )
        # )
      ), style = "font-size: 14px; border-radius: 6px !important"
      )
    )
  })
  
  
  # observeEvent(input$guidedtour, {
  #   removeModal()
  # })


  

  # ----- Dynamic UI: Wind farm Scenarios --------------------------------------

  ## Tack just added and previously added wfs -----------
  observeEvent(input$'active-wfs', {
    
    active_spp <- input$'active-wfs'
    rv_wf$added_wf <- active_spp[!active_spp %in% prev_actv_wf]
    prev_actv_wf <<- c(prev_actv_wf, rv_wf$added_wf)

  })


  ## Append tabPanels for added windfarms in windfarm section   -----------
  observeEvent(rv_wf$added_wf, {

    req(rv_wf$added_wf)
    
    if(rv_wf$added_wf != "Demo Windfarm"){
      w$show()  
    }

    wf_label <- rv_wf$added_wf
    wf_id <- label2id(wf_label)
    wf_tp_id <- paste0("tbp-wf-", wf_id)

    # flag-up a demo wf
    is_demo <- stringr::str_detect(wf_id, "(D|d)emo")

    appendTab(
      inputId = "tbx-wf",
      select = TRUE,
      tabPanel(
        value = wf_tp_id,
        title = tagList(
          strong(wf_label),
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
          # module for added wf main tabPanel content - UI side
          mod_pnl_wf_ui(
            id = paste0('pnl-wf-', wf_id),
            band_mode = input$'swtc-band-mode',
            is_demo = is_demo,
            wf_label = wf_label
          )
        )
      )
    )

    # # module for added wf main panel content - server side
    # mod_pnl_wf_server(
    #   paste0('pnl-wf-', wf_id),
    #   band_mode = reactive(input$'swtc-band-mode'),
    #   is_demo = is_demo,
    #   scrm_inputs = scrm_inputs,
    #   wf_id = wf_id,
    #   wf_tp_id = wf_tp_id
    # )
    # 
    # scrm_inputs$wf_scens[[wf_id]]$wf_label <- wf_label

    # module for added wf main panel content - server side
    wf_inputs[[wf_id]] <- mod_pnl_wf_server(
      paste0('pnl-wf-', wf_id),
      band_mode = reactive(input$'swtc-band-mode'),
      is_demo = is_demo,
      wf_id = wf_id,
      wf_tp_id = wf_tp_id
    )

    # Store wf label
    scrm_inputs$wf_scens[[wf_id]]$wf_label <- wf_label
    
    w$hide()

  })
  
  
  ## Iterate over wf inputs reactively to store inputs -------------------------
  observe({
    # browser()
    purrr::iwalk(rvtl(wf_inputs), function(x, y){
      scrm_inputs$wf_scens[[y]]$wf_inputs <- x()
    })
  })


  #g$set(50)
  
  
  # Render menuSubItem on sidebar to enclose species in active windfarms ----------
  output$subItems_spps_in_wf <- renderMenu({

    req(input$'active-wfs')
    
    wf_menuSubItems <- input$'active-wfs' %>%
      purrr::map(function(x){
        shinydashboard::menuSubItem(
          text = glue::glue("Species at {x}"),
          #text = x,
          #text = span(x, shinydashboardPlus::dashboardBadge("1", color = "blue")),
          #text = paste0(x, " (1)"),
          tabName = paste0("sbsm-sppinwf-", label2id(x)),
          #icon = icon("at", verify_fa = FALSE)
          )
      })
    
    shinydashboard::sidebarMenu(
      .list = wf_menuSubItems
      )
  })

  
  #g$set(75)
  
  
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

        # module for spp in wf - ui side
        mod_spp_in_wf_ui(
          id = paste0("pnl-sppinwf-", wf_id),
          wf_label = rv_wf$added_wf
        )
      )
    )

    # module for spp in wf - server side
    spps_in_wf_dt[[wf_id]] <- mod_spp_in_wf_server(
      id = paste0("pnl-sppinwf-", wf_id),
      wf_label = rv_wf$added_wf,
      band_mode = reactive(input$'swtc-band-mode'),
      wf_id = wf_id,
      wf_oper = reactive(wf_inputs[[wf_id]]()$oper)
      #scrm_inputs = scrm_inputs
    )

    # add menu id to list with model inputs
    scrm_inputs$wf_scens[[wf_id]]$spp_wf_menu_id <- paste0("sbsm-sppinwf-", wf_id)
    
  })
  
  
  # reactively iterate over wfs to store data (including inputs) of species in each wf
  observe({
    purrr::iwalk(rvtl(spps_in_wf_dt), function(x, y){
      #browser()
      scrm_inputs$wf_scens[[y]]$spp_in_wf <- x()$spps_inputs
      scrm_inputs$wf_scens[[y]]$valid <- x()$any_spp_selected
      scrm_inputs$wf_scens[[y]]$active_spps <- x()$active_spps
    })
  })

  
  
  # close dropdown menu comprising wf addition widget
  observeEvent(input$'drpdwn-close', {
    session$sendCustomMessage("close_drpdwn-add-wf", "")
  })



  # Dynamic UI: remove wf tabpanels  ----------------------------------------------

  # Setting up the removing of wf tabPanels when the removing "x" button is clicked
  # Can be a bit confusing, as this is a "promised" action, i.e.
  # the closing reaction is set at creation of the removing button
  observeEvent(rv_wf$added_wf, {

    wf_id <- label2id(rv_wf$added_wf)

    # reactivity on clicking of the "x" of the wf separator 
    shinyjs::onclick(
      id = paste0("btn-rmv-wf-", wf_id),
      expr = {
        # remove tab
        removeTab(inputId = "tbx-wf", target = paste0("tbp-wf-", wf_id) )

        # update selected wfs in selectize widget
        upd_actv_wf <- drop_from_sltz(
          session = session,
          rmv_id = wf_id,
          c_slctd_labs = input$'active-wfs',
          inputId = "active-wfs",
          drop_from_choices = TRUE)

        # Update previously selected wfs
        prev_actv_wf <<- c(upd_actv_wf)
      })
  })
  



  # Removing via the selectize widget in dropdown button
  observeEvent(input$'active-wfs', ignoreNULL = FALSE, {
    
    actv_wf <- input$'active-wfs'
    rmv_wf_label <- setdiff(prev_actv_wf, actv_wf)

    if(length(rmv_wf_label) > 0 ){
      # get remaining selected winfarms
      rmv_wf_id <- label2id(rmv_wf_label)
      # remove tabPanel
      removeTab(inputId = "tbx-wf", target = paste0("tbp-wf-", rmv_wf_id))
      prev_actv_wf <<- c(actv_wf)
    }
  })



  # Store currently active windfarms
  observeEvent(input$'active-wfs', {
    scrm_inputs$active_scens <- label2id(input$'active-wfs')
  })


  #g$set(85)
  
  
  # Simulation and Outputs Sub-module  ------------------------------------------
  mod_pnl_sim_server(
    id = "pnl-sim",
    scrm_inputs = scrm_inputs,
    band_mode = reactive(input$'swtc-band-mode'), 
    app_session = session
  )
  
  
  #g$set(100)
  
  # close app loading screen elements
  hostess$close()
  waiter_hide()
  
  # Customized Shiny disconnected screen
  sever::sever(
    html = sever::sever_default(
      title = "Gah!", 
      subtitle = "Your session ended", 
      button = "Reconnect", 
      button_class = "success"
    ), 
    bg_color = "#000", 
    opacity = 0.9
  )

}




# # Removing via the action button
# observeEvent(input$'js-btn-clicked', {
# 
#   # extract input id of clicked button
#   clicked_btn_id <- stringr::str_split(input$'js-btn-clicked', "\\|")[[1]][1]
# 
#   if(clicked_btn_id == ""){
#     message("\nUnidentified button\n")
#   }else{
# 
#     # list with button tags
#     btn_tags <- unlist(stringr::str_split(clicked_btn_id, "-"))
# 
#     action <- btn_tags[2]
#     section <- btn_tags[3]
#     section_id <- btn_tags[4]
# 
#     if(action == "rmv"){
#       # current ui input ids (excluding those "nullified")
#       valid_inputs_ids <- names(
#         Filter(Negate(is.null), reactiveValuesToList(input))
#       )
#       # remove tabPanel
#       remove_tbp(btn_tags, valid_inputs_ids)
# 
#       # For windfarm panels, update list of active wfs in associated selectize
#       if(section == "wf"){
# 
#         upd_actv_wf <- drop_from_sltz(
#           session = session,
#           rmv_id = section_id,
#           c_slctd_labs = input$'active-wfs',
#           inputId = "active-wfs",
#           drop_from_choices = TRUE)
# 
#         # reset previous selected windfarm
#         prev_actv_wf <<- c(upd_actv_wf)
#       }
# 
#     }
#   }
# })