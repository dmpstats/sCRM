#' spp_in_wf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spp_in_wf_ui <- function(id, wf_label, band_mode = FALSE){
  ns <- NS(id)
  
  tagList(
    
    div(
      # force minimum height of DIV otherwise overlayed waiter is too small
      style = "min-height: 100vh;",
      fluidRow(
        
        shinydashboard::tabBox(
          id = ns("tbx-spps"),
          width = 12,
          title = uiOutput(ns("tbx-title-add-spp-btns")),
          
          # first rendering of demo species tab, which needs to be UI-side given
          # it involves module dependent on {rhandsometable}
          tabPanel(
            value = ns(init_spp_tp_id),
            title = tagList(
              strong(init_spp_label),
              # Species panel remove button
              shinyWidgets::circleButton(
                inputId = ns(paste0("btn-rmv-spp-", init_spp_id)),
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
                id = ns(paste0('pnl-spp-', init_spp_id)),
                spp_label = init_spp_label,
                wf_label = wf_label,
                band_mode = band_mode
              )
            )
          )
          
        )
      ),
      
      shinyjs::hidden(
        div(
          id = ns("no_spp_fdbck"),
          class = "centered-textblock",
          p(
            glue::glue("No species specified for {wf_label}. To proceed, please 
                   select at least one species."),
            style = "color: #dd4b39; font-size: 16px;"
          )
        )
      )
    )
  )
}
 


   
#' spp_in_wf Server Functions
#' 
#' @import waiter
#' @import shinyvalidate
#'
#' @noRd 
mod_spp_in_wf_server <- function(id, band_mode, wf_label, wf_id, wf_oper){
  
  stopifnot(is.reactive(band_mode))
  stopifnot(is.reactive(wf_oper))
  stopifnot(!is.reactive(wf_label))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # global variables
    prev_slct_spp <- c()
    
    # Reactives: Species
    rv_spp <- reactiveValues(
      added_spp = NULL
    )
    
    spp_inputs <- reactiveValues()
    spps_in_wf_inputs <- reactiveValues() 
    
    
    w <- waiter::Waiter$new(
      id = "waiter-content", 
      html = div(
        class = "waiter-input-panel",
        img(src = "www/wf_loading.gif", height = 120),
      ),
      #html = waiter::spin_pulsar(),
      fadeout = TRUE
    )
    
    
    # Input Validation ---------------------------------------------------------
    
    ## Initialize input validator variable
    iv <- InputValidator$new()
    
    # rule for checking if at least one species was selected for the current
    # windfarm.
    iv$add_rule("slct-spp", sv_required(message = ""))
    
    
    ## Raise message if no species is specified --------------------------------
    observeEvent(input$'slct-spp', {
      
      shinyjs::toggle(
        id = "no_spp_fdbck", 
        condition = is.null(input$'slct-spp'), 
        animType = "fade", time = 1
      )
    },
    ignoreInit = TRUE, 
    ignoreNULL = FALSE
    )
    
    
    ## Start displaying error feedback in UI  ---------------------
    iv$enable()


    # Dynamic UI: Add Species in Windfarm  --------------------------------------
    
    # Logic to pair-up with UI generation of species tabs: Get a first-time
    # selected species, returning empty character if currently selected species
    # have been previously selected
    observe({
      sltc_spp <- input$'slct-spp'
      rv_spp$added_spp <- sltc_spp[!sltc_spp %in% prev_slct_spp]
      prev_slct_spp <<- c(prev_slct_spp, rv_spp$added_spp)
    })
    
    
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
        dpdn_close_id = ns("close-drpdwn-spp"),
        wf_label = wf_label
      )
    })
    
    
    
    ## Append tabpanel for added species  ------------------------------------
    observeEvent(rv_spp$added_spp, {
      
      req(rv_spp$added_spp)
      
      w$show()
      
      spp_label <- rv_spp$added_spp
      spp_id <- label2id(spp_label)
      spp_tp_id <- ns(paste0("tbp-", spp_id))
      
      if(spp_label != init_spp_label){
        
        # Dynamically append tabPanel for a new species
        appendTab(
          inputId = "tbx-spps",
          select = TRUE,
          tab = tabPanel(
            value = spp_tp_id,
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
                wf_label = wf_label,
                band_mode = isolate(band_mode())
              )
            )
          )
        )
      }
      
      # module for selected species' main tabPanel - Server side
      spp_inputs[[spp_id]] <- mod_pnl_spp_server(
        id = paste0('pnl-spp-', spp_id),
        spp_id = spp_id,
        tbx_id = ns("tbx-spps"),
        spp_tp_id = spp_tp_id,
        spp_label = spp_label,
        wf_label = wf_label,
        band_mode = band_mode,
        wf_oper = wf_oper
      ) 
      
      
      w$hide()
    })
    
    
    ## Reactively iterate over species to track changes in species inputs ------
    observe({
      purrr::iwalk(rvtl(spp_inputs), function(x, y){
        spps_in_wf_inputs[[y]] <- x()
      })
    })
    
    
    # Dynamic UI: Drop Species ------------------------------------------------
    
    # Setting up the removing of spp tabPanels when the removing "x" button is clicked
    # Can be a bit confusing, as this is a "promised" action, i.e.
    # the closing reaction is set at creation of the removing button
    observeEvent(rv_spp$added_spp, {

      spp_id <- label2id(rv_spp$added_spp)
      
      # reactivity on clicking of the spp removing button
      shinyjs::onclick(
        id = paste0("btn-rmv-spp-", spp_id),
        expr = {
          # remove tab
          removeTab(inputId = "tbx-spps", target = ns(paste0("tbp-", spp_id)) )
          
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
    
    
    # Remove tabPanel for species deselected from associated selectize widget
    observeEvent(input$'slct-spp', ignoreNULL = FALSE, {
      
      sltc_spp <- input$'slct-spp'
      rmv_spp_label <- setdiff(prev_slct_spp, sltc_spp)
      
      if(length(rmv_spp_label) > 0 ){
        # get remaining selected species
        rmv_spp_id <- label2id(rmv_spp_label)
        # remove tabPanel
        removeTab(inputId = "tbx-spps", target = ns(paste0("tbp-", rmv_spp_id)) )
        prev_slct_spp <<- c(sltc_spp)
      }
    })

    
    # Data prep for Module output ---------------------------------------------
    spps_in_wf_dt <- reactive({
      list(
        any_spp_selected = iv$is_valid(),
        active_spps = label2id(input$'slct-spp'),
        spps_inputs = rvtl(spps_in_wf_inputs)
      )
    })


    
    # Module output -----------------------------------------------------------
    spps_in_wf_dt
    
  })
}

## To be copied in the UI
# mod_spp_in_wf_ui("spp_in_wf_ui_1")
    
## To be copied in the server
# mod_spp_in_wf_server("spp_in_wf_ui_1")
