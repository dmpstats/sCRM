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
          shinydashboardPlus::box(
            title = "Monthly Operation",
            status = "primary",
            width = 12,
            fluidRow(
              col_11(
                h5(strong("Maintenance Downtime (%)")),
                div(class = "hot-monthly",
                    rhandsontable::rHandsontableOutput(
                      outputId = ns("hottrbdwnt"),
                      width = "100%")
                    )
              ),
              col_1(
                br(),
                br(),
                shinyWidgets::dropMenu(
                  actionButton(ns("trbdwn-plotbtn"), "", icon = icon("chart-area")), 
                  placement = "left-end",
                  plotOutput(ns("trbdwnt_plot"),  width = "450px", height = "200px")
                )
              )
            ),
            hr(),
            fluidRow(
              col_11(
                h5(strong("Operational Wind Availability (%)")),
                div(class = "hot-monthly",
                  rhandsontable::rHandsontableOutput(
                    outputId = ns("hotwndavlb"),
                    width = "100%")
                )
              ),
              col_1(
                br(),
                br(),
                shinyWidgets::dropMenu(
                  actionButton(ns("wndavlb-plotbtn"), "", icon = icon("chart-area")), 
                  placement = "left-end",
                  plotOutput(ns("wndavlb_plot"),  width = "400px", height = "170px")
                )
              )
            ),
            hr(),
            h5(strong("Proportion of Month Operational")),
            helpText("The difference between wind availability and downtime"), 
            col_8(
              offset = 2,
              plotOutput(ns("opermth_plot"), width = "100%", height = 255)
            )
          )
        )
      )
    )
  )
}
    


#' pnl_wf Server Functions
#'
#' @noRd
#' @import rhandsontable
mod_pnl_wf_server <- function(id, band_mode){
  
  stopifnot(is.reactive(band_mode))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    rv <- reactiveValues(
      rtn_ptch_wnd_df = stochLAB::wndspd_rtn_ptch_example,
      trb_downtime_df = startup_trb_dwntm,
      wind_avlb_df = startup_wind_avbl
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
    
    
    # --- Turbine Downtime -----------------------------------------------------
    
    # Input table
    output$hottrbdwnt <- renderRHandsontable({
      
      if(band_mode() == FALSE){
        
        rv$trb_downtime_df %>%
          rhandsontable(
            rowHeaders = c(
              "Mean", 
              "SD"),
            rowHeaderWidth = 75
          ) %>%
          hot_cols(colWidths = 78, 
                   renderer = "function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.NumericRenderer.apply(this, arguments);
               if (value == null || value.length === 0) {
               td.style.background = 'pink';
               }}") %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        
      }else{
        rv$trb_downtime_df %>%
          slice(1) %>%
          rhandsontable(
            rowHeaders = NULL
          ) %>%
          hot_cols(colWidths = 78, 
                   renderer = "function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.NumericRenderer.apply(this, arguments);
               if (value == null || value.length === 0) {
               td.style.background = 'pink';
               }}") %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
          hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      }
    })
    

    output$trbdwnt_plot <- renderPlot({
      
      req(input$hottrbdwnt)
      
      #browser()
      if(band_mode() == FALSE){
      
      hot_to_r(input$hottrbdwnt) %>% 
        tibble::rownames_to_column(var="Variable") %>% 
        tidyr::pivot_longer(January:December) %>% 
        tidyr::pivot_wider(names_from = Variable) %>%
        dplyr::mutate(
          month = factor( month.abb, levels = month.abb),
          lwBound = msm::qtnorm(p = 0.025, Mean, SD, lower = 0),
          upBound = msm::qtnorm(p = 0.975, Mean, SD, lower = 0)
          ) %>%
        ggplot2::ggplot(aes(x=month, y = Mean, group=month)) +
        ggplot2::geom_pointrange(aes(ymin=lwBound, ymax=upBound), col = "olivedrab", size =0.8) +
        ggplot2::labs(y = "Downtime (%)", x = "", title = "Monthly turbine downtime (Means & 95% CIs)")
        
      }else{
        
        hot_to_r(input$hottrbdwnt) %>% 
          tidyr::pivot_longer(cols = everything(), names_to = "month", values_to = "dwnt") %>%
          dplyr::mutate(month = factor( month.abb, levels = month.abb)) %>%
          lolli_plot(x = month, y = dwnt, 
                     xlab = "", ylab = "Downtime (%)", 
                     point_col = "olivedrab", line_col = "gray")
      }
      
    })  
    
    
    
    # --- Wind Availability   --------------------------------------------------
    
    # -- Input data
    output$hotwndavlb <- renderRHandsontable({
      
      rv$wind_avlb_df %>%
        rhandsontable(rowHeaders = NULL) %>%
        hot_cols(colWidths = 78, 
                 renderer = "function (instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.NumericRenderer.apply(this, arguments);
               if (value == null || value.length === 0) {
               td.style.background = 'pink';
               }}") %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
    
    # -- Plot
    output$wndavlb_plot <- renderPlot({
      
      req(input$hotwndavlb)
      
      hot_to_r(input$hotwndavlb) %>% 
        tidyr::pivot_longer(cols = everything(), names_to = "month", values_to = "avlb") %>%
        dplyr::mutate(month = factor( month.abb, levels = month.abb)) %>%
        lolli_plot(x = month, y = avlb, 
                   xlab = "", ylab = "Wind Availability (%)", 
                   point_col = "olivedrab", line_col = "gray")
    })
    
    
    
    # --- Monthly Operational time ---------------------------------------------
    
    # plot
    output$opermth_plot <- renderPlot({
      
      req(input$hotwndavlb, input$hottrbdwnt)
      
      wndavlb <- hot_to_r(input$hotwndavlb) %>% 
        tidyr::pivot_longer(cols = everything(), values_to = "avlb")
      
      if(band_mode() == FALSE){
        
        hot_to_r(input$hottrbdwnt) %>% 
          tibble::rownames_to_column(var="Variable") %>% 
          tidyr::pivot_longer(January:December) %>% 
          tidyr::pivot_wider(names_from = Variable) %>%
          dplyr::left_join(wndavlb, by = "name") %>%
          dplyr::mutate(
            month = factor( month.abb, levels = month.abb),
            prop_oper_mean = 0.01 * (avlb - Mean),
            prop_oper_lwb = 0.01 * (avlb - msm::qtnorm(p = 0.025, Mean, SD, lower = 0)),
            prop_oper_upb = 0.01 * (avlb - msm::qtnorm(p = 0.975, Mean, SD, lower = 0))
          ) %>%
          ggplot2::ggplot(aes(x=month, y = prop_oper_mean, group=month)) +
          ggplot2::geom_pointrange(aes(ymin = prop_oper_lwb, ymax = prop_oper_upb), 
                                   col = "olivedrab", size =0.8) +
          ggplot2::labs(y = "Proportion of month operational", x = "")
          
      }else{
        
        hot_to_r(input$hottrbdwnt) %>%
          slice(1) %>%
          tidyr::pivot_longer(cols = everything(), values_to = "dwnt") %>%
          dplyr::left_join(wndavlb, by = "name") %>%
          dplyr::mutate(
            month = factor( month.abb, levels = month.abb),
            prop_oper_mean = 0.01 * (avlb - dwnt)
            ) %>%
          lolli_plot(x = month, y = prop_oper_mean,
                     xlab = "", ylab = "Proportion of month operational",
                     point_col = "olivedrab", line_col = "gray")
      }
    })
    

    
    
    # output$plot2 <- renderPlot({
    #   shinipsum::random_ggplot(type = "col") + 
    #     labs(title = "Random plot 2") + 
    #     theme_bw()
    # })
    
  
  })
}

