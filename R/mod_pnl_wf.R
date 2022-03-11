#' pnl_wf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
#' @importFrom dplyr filter
mod_pnl_wf_ui <- function(id){
  ns <- NS(id)
  
  # # default values only applied to first scenario
  # scen_id <- stringr::str_split(id, "-")[[1]][3]
  # if(scen_id != "1"){
  #   wf_pars_lookup$dflt_mean <- NA
  #   wf_pars_lookup$dflt_sd <- NA
  # }
  
  tagList(
    br(),
    fluidRow(
      col_2(
        # shinydashboard::box preferable here because doesn't insert space for title
        shinydashboard::box(
          width = 12, 
          #title = "Scenario Name",
          status = "primary",
          textInput(
            inputId = "txtinput-scen_name", 
            label = strong("Scenario Name"), 
            value = "",
            placeholder = "Provide unique name")
        ),
        shinydashboardPlus::box(
          title = "Wind farm features",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          wf_feat_inputs(ns = ns, inputs_width = "90%")
        )
      ),
      col_10(
        shinydashboardPlus::box(
          title = "Turbine Features",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          fluidRow(
            col_3(
              # shinydashboard::box preferable as it doesn't insert space for title
              # which is not being used here
              shinydashboard::box(
                #title = "Configuration",
                width = 12,
                status = "primary",
                trbn_cfg_inputs(ns = ns, inputs_width = "80%")
              )
            ),
            col_9(
              shinydashboardPlus::box(
                title = "Blade Rotation Speed and Pitch",
                width = 12, 
                status = "primary",
                p("Choose between simulating rotor speed and pitch from 
                         probability distributions OR from a relationship with 
                         wind speed"),
                shinyWidgets::radioGroupButtons(
                  inputId = ns("rtn_pitch_opt"),
                  individual = TRUE,
                  justified = TRUE, 
                  label = NULL,
                  width = "50%",
                  choices = c("Probability distributions" = "probDist",
                              "Wind speed relationship" = "windSpeedReltn"),
                  checkIcon = list(yes = tags$i(class = "fa fa-circle",
                                                style = "color: steelblue"),
                                   no = tags$i(class = "fa fa-circle-o",
                                               style = "color: steelblue"))),
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
                          filter(par_name %in% c("bld_pitch", "rtn_speed"))
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
                          filter(par_name == "wind_spd")
                      )
                    )
                  ),
                  hr(),
                  p(strong("Rotation speed & blade pitch Vs Wind speed")),
                  fluidRow(
                    br(),
                    col_7(
                        rhandsontable::rHandsontableOutput(
                          outputId = ns("hotWindSpeedRltn"),
                          width = "100%",
                          height = "300px"),
                        tags$style(type="text/css", paste0("#", ns("hotWindSpeedRltn"), " th {font-weight:bold;}"))
                    ),
                    col_2(
                      shinyWidgets::dropMenu(
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
          ),
          mod_trbn_oper_ui(id = ns("trbnoper"))
        )
      )
    )
  )
}
    


#' pnl_wf Server Functions
#'
#' @import rhandsontable
#' 
#' @noRd
mod_pnl_wf_server <- function(id, band_mode){
  
  stopifnot(is.reactive(band_mode))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rv <- reactiveValues(
      rtn_ptch_wnd_df = stochLAB::wndspd_rtn_ptch_example,
    )
    
    
    # --- sub-modules' server side ---------------------------------------------
    mod_prob_inputs_server(
      id = "blade", 
      pars_lkp = wf_pars_lookup %>% 
        filter(par_name %in% c("bld_pitch", "rtn_speed")),
      band_mode = band_mode
    )
    
    mod_prob_inputs_server(
      id = "windspeed", 
      pars_lkp = wf_pars_lookup %>% 
        filter(par_name == "wind_spd"),
      band_mode = band_mode
    )
    
    # Monthly Operation panel
    mod_trbn_oper_server(id = "trbnoper", band_mode = band_mode)

    
    # --- Rotation and pitch vs wind speed relationship -------------------------
    
    # Input table
    output$hotWindSpeedRltn <- renderRHandsontable({
      
      rv$rtn_ptch_wnd_df %>%
        bind_rows(data.frame(wind_speed = NA, rtn_speed =NA, bld_pitch = NA)) %>%
        rhandsontable(
          rowHeaders=NULL, 
          colHeaders = c(
            "Wind speed (m/s)",
            "Rotation speed (rpm)", 
            "Blade Pitch (deg)")) %>%
        hot_cols(colWidths = 120) %>%
        hot_table(highlightCol = TRUE, 
                  highlightRow = TRUE) %>%
        hot_context_menu(allowRowEdit = TRUE, allowColEdit = FALSE)
    })  
    
    
    # Rotation speed Vs wind speed plot
    output$wndvsrot_plot <- renderPlot({
      
      req(input$hotWindSpeedRltn)
      
      hot_to_r(input$hotWindSpeedRltn) %>%
        tidyr::drop_na() %>%
        area_plot(x = wind_speed, y = rtn_speed,
                  xlab = "Wind Speed (m/s)", ylab = "Rotation speed (rpm)",
                  point_col = "olivedrab", line_col = "gray", area_col = "olivedrab")
    })
    
    # Blade pitch Vs wind speed plot
    output$wndvsptch_plot <- renderPlot({
      
      req(input$hotWindSpeedRltn)
      
      hot_to_r(input$hotWindSpeedRltn) %>%
        tidyr::drop_na() %>%
        area_plot(x = wind_speed, y = bld_pitch,
                  xlab = "Wind Speed (m/s)", ylab = "Blade Pitch (deg)",
                  point_col = "olivedrab", line_col = "gray", area_col = "olivedrab")
    })
  })
}

