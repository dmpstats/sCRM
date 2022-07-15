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
mod_pnl_spp_ui <- function(id, spp_label, wf_label, band_mode){
  
  ns <- NS(id)
  
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
    dplyr::filter(par_name %in% c("body_lt", "wing_span"))
  
  inflight_pars <- par_dflts %>%  
    dplyr::filter(par_name %in% c("fl_speed", "nct_act", "avoid_bsc", "avoid_ext", "prop_crh"))
  
  
  tagList(

    # Input completion status tracker ------------------------------------------
    mod_input_completion_ui(
      id = ns("inputstate"), 
      pb_title = glue::glue("{spp_label} at {wf_label}")
      ),

    
    fluidRow(
      style = 'overflow-y:scroll; height:75vh !important; overflow-x: hidden;',
      
      # Misc Features ----------------------------------------------------------
      fluidRow(
        col_5(
          ## Body dimensions --------------------------------
          shinydashboardPlus::box(
            title = "Body Dimensions",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            dropdownMenu = info_dropdown(
              inputId = ns("biominfo"),
              placement = "bottom-end",
              md_path =  "inst/app/markdown/info_buttons_docs/spp_body_dimensions.md"
            ),
            
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
            
            dropdownMenu = info_dropdown(
              inputId = ns("inflightinfo"),
              placement = "bottom-end",
              md_path =  "inst/app/markdown/info_buttons_docs/spp_inflight_features.md"
            ),
            
            fluidRow(
              style = "padding-left: 10px",
              col_6(
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
              col_6(
                # Prop upwind flights
                div(
                  numericInput(
                    width = "80%",
                    inputId = ns("upwind"),
                    label = "Proportion of upwind flights",
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
              id = ns("inflightpars"), 
              pars_lkp = inflight_pars, 
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
            
            dropdownMenu = info_dropdown(
              inputId = ns("fhdinfo"),
              placement = "bottom-end",
              md_path =  "inst/app/markdown/info_buttons_docs/spp_fhd.md"
            ),
            
            mod_fhd_inputs_ui(id = ns("fhd")),
            
            rep_br(2)
          )
        )
      ),
      
      # Monthly Densities ------------------------------------------------------
      shinydashboardPlus::box(
        title = "Monthly In-flight Density",
        width = 9,
        status = "primary",
        solidHeader = TRUE,
        
        dropdownMenu = info_dropdown(
          inputId = ns("densinfo"),
          placement = "bottom-end",
          md_path =  "inst/app/markdown/info_buttons_docs/spp_density.md"
        ),
        
        mod_bird_dens_ui(
          id = ns("dens"), 
          spp_label = spp_label)
      ),
  
      # Output Aggregation level -----------------------------------------------
      shinydashboardPlus::box(
        title = "Output Options",
        width = 3,
        status = "primary",
        solidHeader = TRUE,
        
        dropdownMenu = info_dropdown(
          inputId = ns("outoptinfo"),
          placement = "bottom-end",
          md_path =  "inst/app/markdown/info_buttons_docs/spp_output_options.md"
        ),
        
        mod_output_spec_ui(id = ns("outspec"))
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
mod_pnl_spp_server <- function(id, spp_id, tbx_id, spp_tp_id, spp_label, wf_label, 
                               band_mode, wf_oper){
  
  stopifnot(is.reactive(band_mode))
  stopifnot(is.reactive(wf_oper))
  stopifnot(!is.reactive(spp_label))
  stopifnot(!is.reactive(spp_id))
  stopifnot(!is.reactive(wf_label))
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    

    # Sub-modules' server side -------------------------------------------------
    c(biopars_iv, c(body_lt, wing_span)) %<-%  
      mod_prob_inputs_server(
        id = "biopars",
        pars_lkp = dplyr::filter(
          spp_probdist_pars, 
          par_name %in% c("body_lt", "wing_span")
        ),
        band_mode = band_mode,
        plot_fill = spp_colour
      )
    
    c(fhd_iv, fhd) %<-% 
      mod_fhd_inputs_server(
        id = "fhd", 
        spp_label = spp_label, 
        band_mode = band_mode
      )
    
    
    c(inflightpars_iv, c(fl_speed, nct_act, avoid_bsc, avoid_ext, colrisk)) %<-% 
      mod_prob_inputs_server(
        id = "inflightpars",
        pars_lkp =  dplyr::filter(
          spp_probdist_pars, 
          par_name %in% c("fl_speed", "nct_act", "avoid_bsc", "avoid_ext", "prop_crh")
        ),
        band_mode = band_mode,
        plot_fill = spp_colour
      )
    
    
    c(dens_iv, dens) %<-% 
      mod_bird_dens_server(
        id = "dens", 
        band_mode = band_mode, 
        spp_label = spp_label
      )
    
    c(out_opt_iv, out_opt, seas_dt) %<-% mod_output_spec_server(
      id = "outspec", 
      spp_label = spp_label,
      wf_label = wf_label,
      wf_oper = wf_oper,
      dens = dens
      )
    
    
    # Input validation -----------------------------------------------------
    
    ## InputValidator rules ------------
    
    # Initialize input validator variable
    iv <- InputValidator$new()
    
    # Rules for local module inputs
    iv$add_rule("fltype", sv_required(message = ""))
    iv$add_rule("upwind", sv_required(message = ""))
    iv$add_rule("upwind", sv_between(0, 1, message_fmt = "Must be between {left} and {right}"))

    ## Append validators from sub-modules ---
    iv$add_validator(biopars_iv)
    iv$add_validator(inflightpars_iv)
    iv$add_validator(dens_iv)
    iv$add_validator(fhd_iv)
    iv$add_validator(out_opt_iv)
    
    
    
    ## Upwind flights: Highlight widget background if input missing ---------
    observeEvent(input$upwind, {
      
      if(!is.na(input$upwind)){
        shinyjs::js$backgroundCol(ns("upwind"),"white")
      }else{
        shinyjs::js$backgroundCol(ns("upwind"), '#fff1f1')
      }
    })
    
    
    ## Input completion tracker sub-module --------
    mod_input_completion_server(id = "inputstate",  iv = iv )
    
    
    ## Start validation feedback in the UI --------
    iv$enable()
    
    
  
    # Data processing for Module output ---------------------------------------
    spp_inputs <- reactive({
      
      list(
        
        valid = iv$is_valid(),
        
        spp_label = spp_label,
        
        tbx_id = tbx_id,

        spp_tp_id = spp_tp_id,
        
        biom_ftrs = list(
          body_lt = body_lt(),
          wing_span = wing_span()
        ),
        
        fhd = fhd(),
        
        inflight_ftrs = list(
          fltype = input$fltype,
          upwind = input$upwind,
          colrisk = colrisk(),
          fl_speed = fl_speed(),
          nct_act = nct_act(),
          avoid_bsc = avoid_bsc(),
          avoid_ext = avoid_ext()
        ),
        
        bird_dens = dens(),
        
        # seasons = list(
        #   #opt = input$swtchseasons,
        #   opt = input$output_aggr,
        #   dt = hot_to_r(input$hotseasons)
        # )
        
        out_specs = list(
          out_opt = out_opt(),
          seas_dt = seas_dt()
        )
      )
    })
    
    # Module output ----------------------------------------------------------
    spp_inputs
    
  })
}
    
## To be copied in the UI
# mod_pnl_spp_ui("pnl_spp_ui_1")
    
## To be copied in the server
# mod_pnl_spp_server("pnl_spp_ui_1")
