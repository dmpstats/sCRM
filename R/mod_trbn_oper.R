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
    
    tags$table(
      #style = "width: 80%",
      tags$tr(
        tags$td(
          style = "text-align: left",
          mod_monthly_hotab_ui(
            id = ns("trbdwnt"),
            hot_title = strong("Maintenance Downtime (%)")
          )
        ),
        tags$td(
          style = "padding-top: 45px",
          shinyWidgets::dropMenu(
            actionButton(ns("trbdwn-plotbtn"), "", icon = icon("chart-area")),
            placement = "bottom-end",
            trigger = "click", #"mouseenter",
            theme = "light-border",
            plotOutput(ns("trbdwnt_plot"),  width = "650px", height = "280px")
          )
        )
      )
    ),
    
    hr(),
    
    tags$table(
      tags$tr(
        tags$td(
          style = "text-align: left",
          mod_monthly_hotab_ui(
            id = ns("wndavlb"),
            hot_title = strong("Operational Wind Availability (%)")
          )
        ),
        tags$td(
          style = "padding-top: 35px",
          shinyWidgets::dropMenu(
            actionButton(ns("wndavlb-plotbtn"), "", icon = icon("chart-area")),
            placement = "bottom-end",
            trigger = "click", #"mouseenter",
            theme = "light-border",
            plotOutput(ns("wndavlb_plot"),  width = "650px", height = "250px")
          )
        )
      )
    ),
    
    hr(),
    
    h5(strong("Proportion of Month Operational")),
    helpText("The difference between wind availability and downtime"), 
    #span(textOutput(ns("trboper_iv_fbck")), style = "color: #fa8c05"),
    col_8(
      offset = 2,
      plotOutput(ns("opermth_plot"), width = "100%", height = 300)
    )
  )
}

#' trbn_oper Server Functions
#'
#' @import rhandsontable
#' @import zeallot
#' @import dplyr
#' @import shinyvalidate
#' @importFrom truncnorm qtruncnorm


mod_trbn_oper_server <- function(id, band_mode, is_demo){
  
  stopifnot(is.reactive(band_mode))
  stopifnot(!is.reactive(is_demo))
  
  if(!is_demo){
    startup_trb_dwntm <- dplyr::mutate(startup_trb_dwntm, across(everything(), ~NA))
    startup_wind_avbl <- dplyr::mutate(startup_wind_avbl, across(everything(), ~NA))
  }
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    # Object Initialization --------------------------------------------------
    
    # Initialize waiter screens for density plots
    w <- waiter::Waiter$new(
      #id = c(ns("trbdwnt_plot"), ns("wndavlb_plot"), ns("opermth_plot")),
      id = c(ns("opermth_plot")),
      html = waiter::spin_loaders(15, color = "#434C5E")
    )
    
    # Sub-modules' server side -------------------------------------------------
    
    c(trbdwnt_df, trbdwnt_iv) %<-% mod_monthly_hotab_server(
      id = "trbdwnt", 
      band_mode = band_mode, 
      startup_tab =  startup_trb_dwntm, 
      is_pdist = TRUE,
      col_widths = 85
    )
    
    
    c(wndavlb_df, wndavlb_iv) %<-% mod_monthly_hotab_server(
      id = "wndavlb", 
      band_mode = band_mode, 
      startup_tab =  startup_wind_avbl, 
      is_pdist = FALSE,
      col_widths = 89
    ) 
    
    
    # ---  Input Validation  ---------------------------------------------------
    
    # Gather validators from sub-modules to pass to parent module
    iv <- InputValidator$new()
    iv$add_validator(trbdwnt_iv)
    iv$add_validator(wndavlb_iv)
    

    # --- Turbine Downtime Plot ------------------------------------------------
    
    output$trbdwnt_plot <- renderPlot({
      
      if(band_mode() == FALSE){
        
        # trbdwnt_df() %>%
        #   dplyr::mutate(
        #     lwBound = qtruncnorm(p = 0.025, mean = Mean, sd = SD, a = 0),
        #     upBound = qtruncnorm(p = 0.975, mean = Mean, sd = SD, a = 0)
        #   ) %>%
        #   ggplot2::ggplot(aes(x = month, y = Mean, group=month)) +
        #   ggplot2::geom_pointrange(aes(ymin=lwBound, ymax=upBound),
        #                            col = wf_colour, size =0.8) +
        #   ggplot2::labs(y = "Downtime (%)",
        #                 x = "",
        #                 title = "Monthly turbine downtime (Means & 95% PIs)")
        
        trbdwnt_df() %>%
          dplyr::mutate(
            dist = 
              distributional::dist_truncated(
                distributional::dist_normal(Mean, SD), lower = 0)
          ) %>%
          ggplot2::ggplot(ggplot2::aes(x = month, ydist = dist)) +
          ggdist::stat_eye(
          #ggdist::stat_halfeye(
            slab_fill = wf_colour,
            slab_alpha = 0.5,
            slab_colour = "black", 
            slab_size = 0.5, 
            shape = 21,
            point_fill = "white",
            stroke = 1.5,
            point_interval = "mean_qi"
            ) +
          ggplot2::labs(
            y = "Downtime (%)",
            x = "",
            title = "Distribution of turbine downtime per month")
      
      }else{
        
        trbdwnt_df() %>%
          lolli_plot(
            x = month, y = Mean,
            xlab = "", ylab = "Downtime (%)",
            title = "Turbine downtime per month", 
            point_col = wf_colour, line_col = "gray")
      }
    })  
    
    
    
    # --- Wind Availability Plot  ----------------------------------------------
   
    output$wndavlb_plot <- renderPlot({
      
      wndavlb_df() %>%
        lolli_plot(
          x = month, y = wndavlb, 
          title = "Wind Availability per Month",
          xlab = "", ylab = "Wind Availability (%)", 
          point_col = wf_colour, line_col = "gray")
    })
    
    
    
    # --- Monthly Operational time Plot ---------------------------------------
    
    output$opermth_plot <- renderPlot({
      
      w$show()
      
      if(band_mode() == FALSE){
        trbdwnt_df() %>%
          dplyr::left_join( wndavlb_df(), by = c("name", "month")) %>%
          dplyr::mutate(
            trbdwnt_dist = distributional::dist_truncated(
              distributional::dist_normal(Mean, SD), 
              lower = 0)
          ) %>%
          dplyr::mutate(
            oper_dist = 0.01 * (wndavlb - trbdwnt_dist),
            mean_oper = mean(oper_dist)
          ) %>%
          ggplot2::ggplot(ggplot2::aes(x = month, ydist = oper_dist)) +
          ggdist::stat_interval(
            interval_size = 10,
            .width = c(.5, .8, .95), #, .99)
          ) +
          ggplot2::geom_point(
            ggplot2::aes(x = month, y = mean_oper),
            size = 5,
            color = "black", #"#022601", #"#034701"
          ) +
          ggplot2::scale_color_manual(
            values = MetBrewer::met.brewer("VanGogh3", n = 4),
            name = "Quantile\nInterval") +
          ggplot2::labs(
            y = "Proportion of month operational", 
            title = "Mean and Quantile Intervals of proportion of time operational per month", 
            x = "")
        
          
        
      }else{
        
        trbdwnt_df() %>%
          left_join( wndavlb_df(), by = c("name", "month")) %>%
          mutate(
            dwnt = Mean,
            prop_oper_mean = 0.01 * (wndavlb - dwnt)
          ) %>%
          lolli_plot(x = month, y = prop_oper_mean,
                     xlab = "", ylab = "Proportion of month operational", 
                     title = "Proportion of time operational per month",
                     point_col = wf_colour, line_col = "gray")
      }
    })
 
    
    # -- Module Outputs ------------------------------------------------------
    list(
      trbdwnt_df = trbdwnt_df,
      wndavlb_df = wndavlb_df,
      iv = iv
    )
    
  })
}
    
## To be copied in the UI
# mod_trbn_oper_ui("trbn_oper_ui_1")
    
## To be copied in the server
# mod_trbn_oper_server("trbn_oper_ui_1")
