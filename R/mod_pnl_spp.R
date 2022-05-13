#' pnl_spp UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#' @import glue
#'
#' @importFrom shiny NS tagList 
mod_pnl_spp_ui <- function(id, spp_label, band_mode){
  
  ns <- NS(id)
  
  #browser()
  
  # Get default parameter values for current species
  par_dflts <- spp_dflts %>%
    dplyr::filter(spp_id == label2id(spp_label)) %>%
    dplyr::select(body_lt, wing_span, fl_speed, nct_act, avoid_bsc, avoid_ext, fl_type, 
           upwind_fl, prop_crh) %>%
    tidyr::pivot_longer(cols = everything(), names_to = "par_name", values_to = "data") %>%
    tidyr::unnest(data, keep_empty = TRUE) %>%
    dplyr::full_join(spp_probdist_pars, ., by = "par_name")
  
  if("dflt_mean" %not_in% names(par_dflts)) par_dflts$dflt_mean <- NA
  if("dflt" %not_in% names(par_dflts)) par_dflts$dflt <- NA
  
  bio_pars <- par_dflts %>% 
    dplyr::filter(par_name %in% c("body_lt", "wing_span", "fl_speed", "nct_act", "avoid_bsc", "avoid_ext"))
  
  chr_pars <- par_dflts %>%  
    dplyr::filter(par_name %in% c("prop_crh"))
  
  
  tagList(
    #br(),
    
    # Trick with hidden textInput to allow shinyvalidate on rhandsontable
    shinyjs::hidden(
      textInput(inputId = ns("hotseasons_ok"), label = "", value = "yes")
    ),
    
    # Input completion status tracker
    mod_input_completion_ui(id = ns("inputstate")),

    fluidRow(
      style = 'overflow-y:scroll; height:72vh !important; overflow-x: hidden;',
      
      # Misc Features ------------------------------------------------------------
      fluidRow(
        col_5(
          ## Biometric Features --------------------------------
          shinydashboardPlus::box(
            title = "Biometric Features",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            mod_prob_inputs_ui(
              id = ns("biopars"),
              pars_lkp = bio_pars, 
              band_mode = band_mode
            ),
            br()
          ),
          
          ## In-flight features ---------------------------------
          shinydashboardPlus::box(
            title = "In-flight Features",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              style = "padding-left: 10px",
              col_7(
                # Flight type
                shinyWidgets::radioGroupButtons(
                  width = "90%",
                  inputId = ns("fltype"),
                  label = "Flight Type",
                  choices = c("Flapping", "Gliding"), 
                  size = "normal", 
                  #status = "primary",
                  selected = dplyr::filter(
                    par_dflts, par_name == "fl_type") %>% 
                    dplyr::pull(dflt),
                  individual = TRUE,
                  justified = TRUE,
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-circle",
                                 style = "color: #2D78C3"),
                    no = tags$i(class = "fa fa-circle-o",
                                style = "color: #2D78C3")
                  )
                )
              ),
              col_5(
                # Prop upwind flights
                div(
                  numericInput(
                    width = "80%",
                    inputId = ns("upwind"),
                    label = "Upwind flights (pp)",
                    step = 0.01,
                    value = dplyr::filter(
                      par_dflts, par_name == "upwind_fl") %>% 
                      dplyr::pull(dflt_mean), #0.5,
                    min = 0, 
                    max = 1),
                  style = "padding-top: 10px;"
                )
              )
            ),
            
            HTML("<hr style='height:2px;border:none;color:#edf0f5;background-color:#edf0f5;' />"),
            
            mod_prob_inputs_ui(
              id = ns("colrisk"), 
              pars_lkp = chr_pars, 
              band_mode = band_mode
            ),
            br()
          )
        ),
        
        ## FHD ---------------------------------------------------------------
        col_7(
          shinydashboardPlus::box(
            title = "Flight Height Distribution",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            mod_fhd_inputs_ui(id = ns("fhd")),
            
            rep_br(2)
          )
        )
      ),
      
      # Monthly Densities --------------------------------------------------------
      shinydashboardPlus::box(
        title = "Monthly In-flight Density",
        width = 9,
        status = "primary",
        solidHeader = TRUE,
        
        mod_bird_dens_ui(
          id = ns("dens"), 
          spp_label = spp_label)
      ),
      
      
      # Seasonal Periods --------------------------------------------------------
      shinydashboardPlus::box(
        title = "Seasonal Periods",
        width = 3,
        status = "primary",
        solidHeader = TRUE,
        br(),
        
        tagAppendAttributes(
          shinyWidgets::materialSwitch(
            inputId = ns("swtchseasons"),
            label = strong("Outputs by seasonal period(s)?"),
            value = FALSE,
            status = "success",
            inline = FALSE
          ),
          style = "padding-left: 9px"
        ),
        hr(),
        div(
          id = ns("seasons_pnl"),
          h5(strong(id = ns("seasons_label"), "Seasonal Definitions", style = "padding-left: 9px")),
          helpText(
            "Add/remove table rows by right-clicking on a cell",
            style = "font-size: 12px; padding-left: 8px"
          ),
          div(
            class = "hot-monthly", 
            rhandsontable::rHandsontableOutput(
              outputId = ns("hotseasons"),
              width = "100%")
          ),
          div(
            class = "hot-feedback", 
            textOutput(ns("seasons_iv_fbck"))
          )
        )
      )
    )
  )
}



#' pnl_spp Server Functions
#'
#' @import rhandsontable
#' @import zeallot
#' @import shinyvalidate
#' 
#' @noRd 
mod_pnl_spp_server <- function(id, spp_label, band_mode){
  
  stopifnot(!is.reactive(spp_label))
  stopifnot(is.reactive(band_mode))
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    # Seasonal periods start-up table -----------------------------------------
    
    seasons_df <- dplyr::filter(spp_dflts, spp_id == label2id(spp_label)) %>% 
      dplyr::pull(seasons) %>% 
      purrr::pluck(1) 
    
    if(is.null(seasons_df)){
      seasons_df <- data.frame(
        period_name = as.character(NA),
        start_month = as.character(NA),
        end_month = as.character(NA)
      )
    }
    

    # Sub-modules' server side -------------------------------------------------
    c(biopars_iv) %<-% mod_prob_inputs_server(
      id = "biopars",
      pars_lkp = dplyr::filter(
        spp_probdist_pars, 
        par_name %in% c("body_lt", "wing_span", "fl_speed", "nct_act", "avoid_bsc", "avoid_ext")
        ),
      band_mode = band_mode,
      plot_fill = spp_colour
    )
  
    c(fhd_iv) %<-% mod_fhd_inputs_server(
      id = "fhd", 
      spp_label = spp_label, 
      band_mode = band_mode
    )
    
    
    c(crh_iv) %<-% mod_prob_inputs_server(
      id = "colrisk",
      pars_lkp =  dplyr::filter(spp_probdist_pars, par_name %in% c("prop_crh")),
      band_mode = band_mode,
      plot_fill = spp_colour
    )
    
    
    c(dens_type, dens_dt, dens_iv) %<-% mod_bird_dens_server(
      id = "dens", 
      band_mode = band_mode, 
      spp_label = spp_label
    )
    
    
    # Input validation -----------------------------------------------------
    
    ## InputValidator rules -----
    
    # Initialize input validator variable
    iv <- InputValidator$new()
    
    # Rules for local module inputs
    iv$add_rule("fltype", sv_required(message = ""))
    iv$add_rule("upwind", sv_required(message = ""))
    iv$add_rule("upwind", sv_between(0, 1, message_fmt = "Must be between {left} and {right}"))
    
    ## Conditional validation  -----
    seasons_iv <- InputValidator$new()
    seasons_iv$condition(~ input$swtchseasons == TRUE)
    
    seasons_iv$add_rule("hotseasons_ok", sv_required(message = ""))
    seasons_iv$add_rule("hotseasons_ok", ~ if(. != "yes") . )
    
    # Append conditional validators
    iv$add_validator(seasons_iv)
    
    # Append validators from sub-modules
    iv$add_validator(biopars_iv)
    iv$add_validator(crh_iv)
    iv$add_validator(dens_iv)
    iv$add_validator(fhd_iv)
    
    ## Upwind flights: Highlight widget background if input missing ----
    observeEvent(input$upwind, {
      
      if(!is.na(input$upwind)){
        shinyjs::js$backgroundCol(ns("upwind"),"white")
      }else{
        shinyjs::js$backgroundCol(ns("upwind"), '#fff1f1')
      }
    })
    
    
    ## Seasonal periods data validation ---------
    observe({
      
      req(input$hotseasons)
      seas_df <- hot_to_r(input$hotseasons) 
      #browser()
      
      if(dens_type() == "tnorm"){
        avlb_mths <- tidyr::drop_na(dens_dt()) %>%
          pull(month)  
      }else if(not_null(dens_dt())){
        avlb_mths <- tidyr::drop_na(dens_dt()) %>%
          names()
      }else{
        avlb_mths <- NULL
      }
      
      if(all(is.na(seas_df))){   # Check for empty table
        
        shinyjs::js$fontCol(ns("seasons_label"), '#dd4b39')
        updateTextInput(inputId = "hotseasons_ok", value = "")
        
      }else if(any(is.na(seas_df))){  # Check for any missing value
        
        msg <- "All cells must be populated"
        output$seasons_iv_fbck <- renderText({msg})
        shinyjs::js$fontCol(ns("seasons_label"), '#dd4b39')
        updateTextInput(inputId = "hotseasons_ok", value = msg)
        
      }else if(seas_uncovered(seas_df, avlb_mths)){  # check if period is covered by monthly density data
        msg <- "Defined period(s) comprise month(s) with no density data"
        output$seasons_iv_fbck <- renderText({msg})
        shinyjs::js$fontCol(ns("seasons_label"), '#dd4b39')
        updateTextInput(inputId = "hotseasons_ok", value = msg)
        
      }else{  # All good
        
        output$seasons_iv_fbck <- renderText({invisible()})
        shinyjs::js$fontCol(ns("seasons_label"),"#333")
        updateTextInput(inputId = "hotseasons_ok", value = "yes")
        
      }
    })
    
    
    ## Input completion tracker sub-module --------
    mod_input_completion_server(id = "inputstate",  iv = iv )
    
    
    ## Start validation feedback in the UI ----
    iv$enable()
    
    
   # Input table for seasonal definitions --------------------------------------
    output$hotseasons <- renderRHandsontable({
      
      seasons_df %>%
        rhandsontable(
          height = 200,
          #stretchH = "all",
          rowHeaders = NULL,
          colHeaders = c(
            "Period\nName",
            "Starting\nMonth", 
            "Ending\nMonth"),
          overflow = "visible"
        ) %>%
        hot_context_menu(allowColEdit = FALSE) %>%
        hot_cols(colWidths = 98) %>%
        hot_col(
          col = 1, type = "text",
          renderer = "
             function (instance, td, row, col, prop, value, cellProperties) {
               Handsontable.renderers.TextRenderer.apply(this, arguments);
               if (value == null || value.length === 0) {
                   td.style.background = '#fff1f1';
               }
          td.style.fontWeight = 'bold';
              }"
        ) %>%
        hot_col(
          col = c(2, 3), type = "dropdown", 
          valign = " htMiddle",
          source = month.name, strict = TRUE,
          renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
              Handsontable.renderers.DropdownRenderer.apply(this, arguments);
              if (!value || value === '') {
                td.style.background = '#fff1f1';
               }
             }"
          )
    })
    
    
    # Dynamic UI: show/hide seasonal definitions panel -------------------------
    observe({
      shinyjs::toggle(
        id = "seasons_pnl", 
        condition = input$swtchseasons, 
        anim = TRUE, 
        animType = "slide") 
    })
    
    
  })
}
    
## To be copied in the UI
# mod_pnl_spp_ui("pnl_spp_ui_1")
    
## To be copied in the server
# mod_pnl_spp_server("pnl_spp_ui_1")
