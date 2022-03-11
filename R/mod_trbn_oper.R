#' trbn_oper UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_trbn_oper_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    
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
}
    
#' trbn_oper Server Functions
#'
#' @noRd 
#' @import rhandsontable
#' @import dplyr
#' @importFrom truncnorm qtruncnorm
mod_trbn_oper_server <- function(id, band_mode){
  
  stopifnot(is.reactive(band_mode))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # initialize non-reactive globals
    trbdwnt_sds <- rep(NA, 12)
    
    # initialize reactive values
    rv <- reactiveValues(
      rtn_ptch_wnd_df = stochLAB::wndspd_rtn_ptch_example,
      trb_downtime_df = startup_trb_dwntm,
      wind_avlb_df = startup_wind_avbl
    )
    
    # If server initialized on band mode, change sds to 0
    observeEvent(band_mode(), once = TRUE, {
      if(band_mode()){
        rv$trb_downtime_df[2, ] <- 0
      }
    })
    
    
    # --- Turbine Downtime -----------------------------------------------------
    
    # Functionality to modify row with SDs based on band_mode
    observeEvent(band_mode(), {
      
      # capture current table
      dt <- hot_to_r(input$hottrbdwnt)
      
      if(band_mode()) {
        # store current sds
        trbdwnt_sds <<- dt[2, ]
        # Assign 0s to sds
        dt[2, ] <- 0
        # update reactive
        rv$trb_downtime_df <- dt
      }else{
        # Assign stored sd
        dt[2, ] <- trbdwnt_sds
        # update reactive
        rv$trb_downtime_df <- dt  
      }
    }, 
    ignoreInit = TRUE)
    
    
    # Input table
    output$hottrbdwnt <- renderRHandsontable({
      
      rv$trb_downtime_df %>%
        rhandsontable(
          rowHeaders = c(
            "Mean", 
            "SD"),
          rowHeaderWidth = 75
        ) %>%
        hot_row(2, readOnly = band_mode()) %>%
        hot_cols(colWidths = 78, 
                 renderer = "
                 function (instance, td, row, col, prop, value, cellProperties) {
                 Handsontable.renderers.NumericRenderer.apply(this, arguments);
                 
                 if (value == null || value.length === 0) {
                 td.style.background = '#ffcfd7';
                 }
                 if(cellProperties.readOnly == true){
                 td.style.background = '#ededed'; 
                 }
                 }") %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE) %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
    })
    
    
    
    
    output$trbdwnt_plot <- renderPlot({
      
      req(input$hottrbdwnt)
      
      dt <- hot_to_r(input$hottrbdwnt) %>%
        tibble::rownames_to_column(var="Variable") %>%
        tidyr::pivot_longer(January:December) %>%
        tidyr::pivot_wider(names_from = Variable) %>%
        mutate(
          month = factor( month.abb, levels = month.abb)
        )
      
      if(band_mode() == FALSE){
        
        dt %>%
          mutate(
            lwBound = qtruncnorm(p = 0.025, mean = Mean, sd = SD, a = 0),
            upBound = qtruncnorm(p = 0.975, mean = Mean, sd = SD, a = 0)
          ) %>%
          ggplot2::ggplot(aes(x = month, y = Mean, group=month)) +
          ggplot2::geom_pointrange(aes(ymin=lwBound, ymax=upBound), col = "olivedrab", size =0.8) +
          ggplot2::labs(y = "Downtime (%)", 
                        x = "", 
                        title = "Monthly turbine downtime (Means & 95% CIs)")
        
      }else{
        
        dt %>%
          lolli_plot(x = month, y = Mean,
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
        mutate(month = factor( month.abb, levels = month.abb)) %>%
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
          left_join(wndavlb, by = "name") %>%
          mutate(
            month = factor( month.abb, levels = month.abb),
            prop_oper_mean = 0.01 * (avlb - Mean),
            prop_oper_lwb = 0.01 * (avlb - qtruncnorm(p = 0.025, mean= Mean, sd = SD, a = 0)),
            prop_oper_upb = 0.01 * (avlb - qtruncnorm(p = 0.975, mean = Mean, sd = SD, a = 0))
          ) %>%
          ggplot2::ggplot(aes(x=month, y = prop_oper_mean, group=month)) +
          ggplot2::geom_pointrange(aes(ymin = prop_oper_lwb, ymax = prop_oper_upb),
                                   col = "olivedrab", size =0.8) +
          ggplot2::labs(y = "Proportion of month operational", x = "")
        
      }else{
        
        hot_to_r(input$hottrbdwnt) %>%
          slice(1) %>%
          tidyr::pivot_longer(cols = everything(), values_to = "dwnt") %>%
          left_join(wndavlb, by = "name") %>%
          mutate(
            month = factor( month.abb, levels = month.abb),
            prop_oper_mean = 0.01 * (avlb - dwnt)
          ) %>%
          lolli_plot(x = month, y = prop_oper_mean,
                     xlab = "", ylab = "Proportion of month operational",
                     point_col = "olivedrab", line_col = "gray")
      }
    })
 
  })
}
    
## To be copied in the UI
# mod_trbn_oper_ui("trbn_oper_ui_1")
    
## To be copied in the server
# mod_trbn_oper_server("trbn_oper_ui_1")
