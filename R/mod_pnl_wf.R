#' pnl_wf UI Function
#'
#' @description Shiny Module for the main panel comprising wind farm inputs
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @importFrom dplyr filter
mod_pnl_wf_ui <- function(id, band_mode, is_demo, wf_label){
  
  ns <- NS(id)
  
  if(!is_demo){
    wf_pars_lookup$dflt_mean <- NA
    wf_pars_lookup$dflt_sd <- NA
  }
  
  tagList(
    
    # Trick with hidden textInput to allow shinyvalidate on rhandsontable
    shinyjs::hidden(
      textInput(inputId = ns("hotWdSpdRln_ok"), label = "", value = "yes")
    ),
    
    # Input completion status tracker
    mod_input_completion_ui(
      id = ns("inputstate"), 
      pb_title = glue::glue("{wf_label} Parameters")
      ),
    
    fluidRow(
      style = 'overflow-y:scroll; height:75vh !important; overflow-x: hidden;',
      
      fluidRow(
        # Wind farm Features ----------------------------------------------------
        col_2(
          style = "padding-right: 0px",
          shinydashboardPlus::box(
            title =  "Windfarm Features",
            width = 12,
            status = "primary",
            solidHeader = TRUE,

            dropdownMenu = info_dropdown(
              inputId = ns("wfftinfo"),
              placement = "bottom-start",
              md_path =  "inst/app/markdown/info_buttons_docs/wf_features.md"
            ),
            
            wf_feat_inputs(ns = ns, inputs_width = "90%", is_demo)
          )
        ),
        
        # Turbine Features ----------------------------------------------------
        col_10(
          shinydashboardPlus::box(
            title = "Turbine Features",
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            
            dropdownMenu = info_dropdown(
              inputId = ns("trbnftinfo"),
              placement = "bottom-end", 
              md_path = "inst/app/markdown/info_buttons_docs/trbn_features.md"
            ),
            
            fluidRow(
              col_3(
                # shinydashboard::box preferable as it doesn't insert space for title
                # which is not being used here
                shinydashboard::box(
                  width = 12,
                  status = "primary",
                  trbn_cfg_inputs(ns = ns, inputs_width = "80%", is_demo)
                )
              ),
              col_9(
                shinydashboardPlus::box(
                  title = "Blade Pitch and Rotation Speed",
                  width = 12,
                  status = "primary",
                  
                  dropdownMenu = info_dropdown(
                    inputId = ns("pitchrotinfo"),
                    placement = "bottom-end", 
                    md_path = "inst/app/markdown/info_buttons_docs/pitch_rotationspeed.md"
                  ),
                  
                  #p("Choose how to incorporate uncertainty in blade pitch and rotation speed"),
                  shinyWidgets::radioGroupButtons(
                    inputId = ns("rtn_pitch_opt"),
                    individual = TRUE,
                    label = "Input type",
                    #width = "55%",
                    choices = c("Trunc. Normal Distribution" = "probDist",
                                "Wind speed Relationship" = "windSpeedReltn"),
                    checkIcon = list(yes = tags$i(class = "fa fa-circle",
                                                  style = "color: #2D78C3"),
                                     no = tags$i(class = "fa fa-circle-o",
                                                 style = "color: #2D78C3"))),
                  #br(),
                  hr(),
                  
                  conditionalPanel(
                    condition = "input.rtn_pitch_opt == 'probDist'", 
                    ns = ns,
                    fluidRow(
                      col_8(
                        # insert panel with blade feature inputs
                        mod_prob_inputs_ui(
                          id = ns("blade"), 
                          pars_lkp = wf_pars_lookup %>% 
                            filter(par_name %in% c("bld_pitch", "rtn_speed")),
                          band_mode = band_mode
                        ),
                        br()
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.rtn_pitch_opt == 'windSpeedReltn'", 
                    ns = ns,
                    fluidRow(
                      col_8(
                        mod_prob_inputs_ui(
                          id = ns("windspeed"), 
                          pars_lkp = wf_pars_lookup %>% 
                            filter(par_name == "wind_spd"),
                          band_mode = band_mode
                        )
                      )
                    ),
                    hr(),
                    p(id = ns("WdSpdRln_label"),
                      strong("Rotation speed & blade pitch Vs Wind speed"),
                      style = "padding-bottom: 8px"
                    ),
                    fluidRow(
                      #br(),
                      col_7(
                        div(
                          class = "hot-monthly",
                          rhandsontable::rHandsontableOutput(
                            outputId = ns("hotWindSpeedRltn"),
                            width = "100%",
                            height = "300px")
                        ),
                        div(
                          class = "hot-feedback", 
                          textOutput(ns("WdSpdRtn_iv_fbck"))
                        )
                      ),
                      col_2(
                        shinyWidgets::dropMenu(
                          theme = "light-border",
                          actionButton(ns("wndvsrotptch-plotbtn"), "", icon = icon("chart-area")), 
                          placement = "right-start",
                          plotOutput(ns("wndvsrot_plot"),  width = "270px", height = "180px"),
                          br(),
                          plotOutput(ns("wndvsptch_plot"),  width = "270px", height = "180px")
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),
      # Turbine Monthly Operation  ----------------------------------------------------
      shinydashboardPlus::box(
        title = "Monthly Operation",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        dropdownMenu = info_dropdown(
          inputId = ns("trbnoperinfo"),
          placement = "bottom-end", 
          md_path = "inst/app/markdown/info_buttons_docs/trbn_operation.md"
        ),
        
        # Turbine monthly operation module
        mod_trbn_oper_ui(id = ns("trbnoper"))
      )
    )
  )
}



#' pnl_wf Server Functions
#' 
#' @param id string, the unique id of the module
#' @param band_mode a `reactive()` logical, specifying whether the scrm
#'   simulation should be deterministic (`TRUE`) or stochastic (`FALSE`)
#' @param is_demo logical, defining if generating input panel for a demo
#'   windfarm, for demonstration purposes
#' @param wf_id string, the id of the farm
#' @param scrm_inputs a `reactiveValues()` list, to store all input values
#'   related with windfarm features. (Note: based on the “stratégie du petit r”
#'   for sharing data between modules).
#' 
#'
#' @import rhandsontable 
#' @import shinyvalidate
#' @import zeallot
#' 
#' @noRd
mod_pnl_wf_server <- function(id, band_mode, is_demo, wf_id,  wf_tp_id){
  
  stopifnot(is.reactive(band_mode))
  stopifnot(!is.reactive(is_demo))
  stopifnot(!is.reactive(wf_id))

  # start-up data for rotation and pitch vs windspeed relationship. NAs for non-demo wfs
  if(is_demo){
    rtn_ptch_wnd_startup <- stochLAB::wndspd_rtn_ptch_example
  }else{
    rtn_ptch_wnd_startup <- stochLAB::wndspd_rtn_ptch_example %>%
      dplyr::mutate(
        rtn_speed = NA,
        bld_pitch = NA
      )
  }
  
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    # Initialize reactive valuas ---------------------------------------------
    wf_inputs <- reactiveValues()
    
    
    # Sub-modules' server side ---------------------------------------------
    
    c(blade_prob_iv, c(bld_pitch, rtn_speed) ) %<-% 
      mod_prob_inputs_server(
        id = "blade", 
        pars_lkp = wf_pars_lookup %>% 
          filter(par_name %in% c("bld_pitch", "rtn_speed")),
        band_mode = band_mode,
        plot_fill = wf_colour
      )
    
    c(wind_spd_iv, c(wind_spd)) %<-% 
      mod_prob_inputs_server(
        id = "windspeed", 
        pars_lkp = wf_pars_lookup %>% 
          filter(par_name == "wind_spd"),
        band_mode = band_mode, 
        plot_fill = wf_colour
      )
    
    c(trbdwnt, wndavlb, trbn_oper_iv) %<-% 
      mod_trbn_oper_server(
        id = "trbnoper", 
        band_mode = band_mode, 
        is_demo = is_demo
      )
    
    
    # Input validation -----------------------------------------------------
    
    # Initialize input validator variable
    iv <- InputValidator$new()
    
    ## Non-conditional inputs ------
    iv$add_rule("lat", sv_required(message = ""))
    iv$add_rule("lat", sv_between(-90, 90))

    purrr::walk(
      .x = c("wfwidth", "tdloffset", "rtradius", "airgap", "bladewth"), 
      .f = function(x){
        iv$add_rule(x, sv_required(message = ""))
        iv$add_rule(x, sv_numeric())
        iv$add_rule(x, sv_gte(0, message_fmt = "Must be non-negative"))
      })
    
    purrr::walk(
      .x = c("nturb", "nblades"), 
      .f = function(x){
        iv$add_rule(x, sv_required(message = ""))
        iv$add_rule(x, sv_integer())
        iv$add_rule(x, sv_gte(0, message_fmt = "Must be non-negative"))
      })
    
    # Append validators from monthly operation sub-module
    iv$add_validator(trbn_oper_iv)
    
    
    ## Conditional inputs  -------

    ### Rotation speed and pitch as relationship with windspeed -----
    rtnptch_wnd_iv <- InputValidator$new()
    rtnptch_wnd_iv$condition(~ input$rtn_pitch_opt == 'windSpeedReltn')
    
    # Append validator from windspeed probdist sub-module
    rtnptch_wnd_iv$add_validator(wind_spd_iv)
    
    # Validation of hotable with blade pitch and rotation VS. windspeed
    rtnptch_wnd_iv$add_rule("hotWdSpdRln_ok", sv_required(message = ""))
    rtnptch_wnd_iv$add_rule("hotWdSpdRln_ok", ~ if(. != "yes") . )


    ### Rotation speed and pitch as prob distributions  -------
    rtnptch_dst_iv <- InputValidator$new()
    rtnptch_dst_iv$condition(~ input$rtn_pitch_opt == 'probDist')
    
    # Append validators from blade speed and pitch probdist sub-module
    rtnptch_dst_iv$add_validator(blade_prob_iv)

    # Append conditional validators to main validator as children
    iv$add_validator(rtnptch_wnd_iv)
    iv$add_validator(rtnptch_dst_iv)
    

    
    ## Rotation and pitch vs wind data validation -----
    observeEvent(input$hotWindSpeedRltn, {
      
      df <- hot_to_r(input$hotWindSpeedRltn)
      
      if(all(is.na(df))){ # check for empty table
        
        updateTextInput(inputId = "hotWdSpdRln_ok", value = "")
        shinyjs::js$fontCol(ns("WdSpdRln_label"), '#dd4b39')
        output$WdSpdRtn_iv_fbck <- ({invisible()})
        
      }else if(any(df < 0, na.rm = TRUE)){ # check for negative values
        
        msg <- "All values must be positive"
        output$WdSpdRtn_iv_fbck <- renderText({msg})
        updateTextInput(inputId = "hotWdSpdRln_ok", value = msg)
        shinyjs::js$fontCol(ns("WdSpdRln_label"), '#dd4b39')
        
      }else if(empty_cols(df)){ # check for empty columns
        
        msg <- "Empty column(s) are not allowed"
        output$WdSpdRtn_iv_fbck <- renderText({msg})
        updateTextInput(inputId = "hotWdSpdRln_ok", value = msg)
        shinyjs::js$fontCol(ns("WdSpdRln_label"), '#dd4b39')
        
      }else{
        
        # All good
        output$WdSpdRtn_iv_fbck <- ({invisible()})
        updateTextInput(inputId = "hotWdSpdRln_ok", value = "yes")
        shinyjs::js$fontCol(ns("WdSpdRln_label"),"#333")
      }
    })
    
    ## Highlight local widgets' background if input is missing
    observe({
      par_ids <- c("nturb", "lat", "wfwidth", "tdloffset", "nblades", "rtradius",
                   "airgap", "bladewth")
      
      purrr::walk(par_ids, function(x){
        val <- input[[x]]
        if(not_null(val)){
          if(!is.na(val)){
            shinyjs::js$backgroundCol(ns(x),"white")
          }else{
            shinyjs::js$backgroundCol(ns(x), '#fff1f1')
          }
        }
      })
    })
    
    
    ## Input completion tracker sub-module -------------------------------
    mod_input_completion_server(id = "inputstate", iv = iv)
    
    
    ## Start displaying error feedback in UI  -------------------
    iv$enable()
    
    
    
    # Ui rendering for pitch and rotation speed data, based on band_mode -------------
    observeEvent(band_mode(), {
      
      if(band_mode()){
        
        shinyWidgets::updateRadioGroupButtons( 
          session = session, 
          inputId = "rtn_pitch_opt",  
          choices = c("Point Estimates" = "probDist"), 
          selected = "probDist",
          checkIcon = list(
            yes = tags$i(class = "fa fa-circle",
                         style = "color: #2D78C3"),
            no = tags$i(class = "fa fa-circle-o",
                        style = "color: #2D78C3")
          )
        )
        
      }else{
        
        shinyWidgets::updateRadioGroupButtons( 
          session = session, 
          inputId = "rtn_pitch_opt",  
          choices = c("Trunc. Normal Distribution" = "probDist",
                      "Wind speed Relationship" = "windSpeedReltn"),
          # selected = "probDist",
          checkIcon = list(
            yes = tags$i(class = "fa fa-circle",
                         style = "color: #2D78C3"),
            no = tags$i(class = "fa fa-circle-o",
                        style = "color: #2D78C3")
          )
        )
      }
    })
    
    
    # Rotation and pitch vs wind speed relationship ----------------------------
    
    # Input table
    output$hotWindSpeedRltn <- renderRHandsontable({
      
      rtn_ptch_wnd_startup %>%
        bind_rows(data.frame(wind_speed = NA, rtn_speed =NA, bld_pitch = NA)) %>%
        rhandsontable(
          rowHeaders=NULL, 
          colHeaders = c(
            "Wind speed (m/s)",
            "Rotation speed (rpm)", 
            "Blade Pitch (deg)")
          ) %>%
        hot_cols(
          colWidths = 118, 
          type = "numeric", 
          format = list(trimMantissa = TRUE, mantissa = 2),
          renderer = "
            function (instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.NumericRenderer.apply(this, arguments);
            
              if (value < 0) {
                 td.style.color = 'red';
               }
               
              if (value == null || value.length === 0) {
                 td.style.background = '#fff1f1';
               }
              if(cellProperties.readOnly == true){
                 td.style.background = '#ededed'; 
               }
            }"
          ) %>%
        hot_table(
          highlightCol = TRUE, 
          highlightRow = TRUE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })  
    
    
    # Convert hot to data frame and store it in reactive, for multiple use in module
    windspd_to_rtnspd_ptch <- reactive({
      
      hot_to_r(input$hotWindSpeedRltn) %>%
        tidyr::drop_na()
      
      }) %>%
      bindEvent(input$hotWindSpeedRltn)
    
    
    # Rotation speed Vs wind speed plot
    output$wndvsrot_plot <- renderPlot({
      
      req(windspd_to_rtnspd_ptch())
      
      windspd_to_rtnspd_ptch() %>%
        #tidyr::drop_na(rtn_speed) %>%
        area_plot(
          x = wind_speed, 
          y = rtn_speed,
          xlab = "Wind Speed (m/s)", 
          ylab = "Rotation speed (rpm)",
          point_col = "olivedrab", 
          line_col = "gray", 
          area_col = "olivedrab")
    })
    
    # Blade pitch Vs wind speed plot
    output$wndvsptch_plot <- renderPlot({
      
      req(windspd_to_rtnspd_ptch())
      
      windspd_to_rtnspd_ptch() %>%
        #tidyr::drop_na(bld_pitch) %>%
        area_plot(
          x = wind_speed, 
          y = bld_pitch,
          xlab = "Wind Speed (m/s)", 
          ylab = "Blade Pitch (deg)",
          point_col = "olivedrab", 
          line_col = "gray", 
          area_col = "olivedrab")
    })
    
    
     
    # Data processing for Module output ---------------------------------------
    blade_pitch_rtn <- reactive({

      req(input$rtn_pitch_opt)

      if(input$rtn_pitch_opt == 'probDist'){
        list(
          opt = "probDist",
          bld_pitch = bld_pitch(),
          rtn_speed = rtn_speed()
        )
      }else{
        list(
          opt = "windSpeedReltn",
          wind_spd = wind_spd(),
          windspd_to_rtnspd_ptch = windspd_to_rtnspd_ptch()
        )
      }
    })

    


    # # Storing input values in "global" `reactiveValues` list `scrm_inputs` -----------
    # observe({
    # 
    #   scrm_inputs$wf_scens[[wf_id]]$wf_inputs <- list(
    # 
    #     valid = iv$is_valid(),
    # 
    #     wf_tp_id = wf_tp_id,
    # 
    #     wf_ftrs = list(
    #       nturb = input$nturb,
    #       lat = input$lat,
    #       wfwidth = input$wfwidth,
    #       tdloffset = input$tdloffset,
    #       lac =  input$lac
    #     ),
    # 
    #     trbn_ftrs = list(
    #       nblades = input$nblades,
    #       rtradius = input$rtradius,
    #       airgap = input$airgap,
    #       bladewth = input$bladewth,
    #       blade_pitch_rtn = blade_pitch_rtn()
    #     ),
    # 
    #     oper = list(
    #       trbdwnt = trbdwnt(),
    #       wndavlb = wndavlb()
    #     )
    #   )
    # 
    # })
    
    
    
    # Data processing for Module output ---------------------------------------
    wf_inputs <- reactive({

      list(

        valid = iv$is_valid(),

        wf_tp_id = wf_tp_id,

        wf_ftrs = list(
          nturb = input$nturb,
          lat = input$lat,
          wfwidth = input$wfwidth,
          tdloffset = input$tdloffset,
          lac =  input$lac
        ),

        trbn_ftrs = list(
          nblades = input$nblades,
          rtradius = input$rtradius,
          airgap = input$airgap,
          bladewth = input$bladewth,
          blade_pitch_rtn = blade_pitch_rtn()
        ),

        oper = list(
          trbdwnt = trbdwnt(),
          wndavlb = wndavlb()
        )
      )
      
    })


    # Module output ---------------------------------------
    wf_inputs
    
  })
}

