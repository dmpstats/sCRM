#' Species density data inputs - UI Function
#'
#' @description A shiny Module for panel with species survey data inputs, i.e. Proportion of upwind flights
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_bird_dens_ui <- function(id, spp_label){
  
  ns <- NS(id)
  
  tagList(
    
    # Hidden textInputs to allow for shinyvalidate user's uploaded data
    shinyjs::hidden(
      purrr::map(
        c("pctlsOK", "drawsOK"), function(x){
          textInput(inputId = ns(x), label = "", value = "yes")
        }
      )
    ),

    col_12(
      shinyWidgets::radioGroupButtons(
        inputId = ns("dttype"),
        individual = TRUE,
        justified = FALSE, 
        label = "Type of density inputs",
        choices = c("Trunc. Normal Distribution" = "tnorm",
                    "Percentile Estimates" = "pctls",
                    "Random Draws" = "draws"),
        selected = "tnorm",
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle",
                       style = "color: #2D78C3"),
          no = tags$i(class = "fa fa-circle-o",
                      style = "color: #2D78C3")
        )
      )
    ),
    rep_br(4),
    
    conditionalPanel(
      condition = "input.dttype == 'tnorm'",
      ns = ns,
      br(),
      mod_monthly_hotab_ui(
        id = ns("tnorm"),
        hot_title = strong(
          "Estimates of daytime in-flight density within the windfarm footprint (birds/km",
          tags$sup(2, .noWS = "before"),
          strong(")", .noWS = c("before"))
        )
      ),
      fluidRow(
        col_10(
          rep_br(1),
          offset = 1, 
          plotOutput(ns("tnorm_summ_plot"), width = "100%", height = 300)
        )
      )
    ),

    conditionalPanel(
      condition = "input.dttype == 'pctls'",
      ns = ns,
      br(),
      col_12(
        p(id = ns("pctls_label"), 
          strong("Percentiles of daytime in-flight density within the windfarm
                 footprint (birds/km", 
                 tags$sup(2, .noWS = "before"), 
                 strong(")", .noWS = c("before")))
        )
      ),
      
      col_4(
        fileInput(
          inputId = ns("flinput-pctls"),
          label = NULL,
          #label = "Upload percentiles of daytime in-flight density within the windfarm footprint",
          multiple = FALSE,
          accept = c(".csv")) %>%
          bsplus::bs_embed_tooltip(
            title = c("Data with percentile estimates of in-flight densities",
                      "per month (birds/km^2). Provide at least 1st, 2.5th, 50th, 97.5th ",
                      "& 99th percentiles. Please download & use the adjacent template."), 
            placement = "left")
      ),
      col_1(
        style = "margin-left: -20px",
        downloadButton(
          outputId = ns("dwnld_tmplt_pctls"), 
          label = NULL, 
          class = "dwnld-butt") %>%
          bsplus::bs_embed_tooltip(
            title =   c("Template dataset. Fill it in, save ",
                        "locally and upload it on the adjacent field"),
            placement = "right")
      ),
      
      #rep_br(3),
      
      fluidRow(
        col_10(
          rep_br(1),
          offset = 1,
          plotOutput(ns("pctls_dt_plot"), width = "100%", height = 300)
        )
      )
    ),
    conditionalPanel(
      condition = "input.dttype == 'draws'",
      ns = ns,
      br(),
      col_12(
        p(id = ns("draws_label"), 
          strong("Random draws of daytime in-flight density within the windfarm
                 footprint (birds/km", 
                 tags$sup(2, .noWS = "before"), 
                 strong(")", .noWS = c("before"))
          )
        )
      ),
      col_4(
        fileInput(
          inputId = ns("flinput-draws"),
          label = NULL,
          multiple = FALSE,
          accept = c(".csv")) %>%
          bsplus::bs_embed_tooltip(
            title = c("Data with random samples of estimated monthly daytime ", 
                      "in-flight densities. Provide at least 1000 draws. Please ",
                      "download & use the adjacent template."), 
            placement = "left")
      ),
      col_1(
        style = "margin-left: -20px",
        downloadButton(
          outputId = ns("dwnld_tmplt_draws"), 
          label = NULL, 
          class = "dwnld-butt") %>%
          bsplus::bs_embed_tooltip(
            title =   c("Template dataset. Fill it in, save ",
                        "locally and upload it on the adjacent field"),
            placement = "right")
      ),
      br(),
      fluidRow(
        col_10(
          rep_br(1),
          offset = 1,
          plotOutput(ns("draws_dt_plot"), width = "100%", height = 300)
        )
      )
    ),
    br()
  )
}
    
#' Species survey data inputs - Server Functions
#'
#' @import rhandsontable
#' @import shinyvalidate
#' @import zeallot
#' 
#' @noRd 
mod_bird_dens_server <- function(id, band_mode, spp_label){
  
  stopifnot(is.reactive(band_mode))
  stopifnot(!is.reactive(spp_label))
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    # Object Initialization --------------------------------------------------
    
    # Initialize non-reactive globals
    tnorm_sds <- rep(NA, 12)
    
    # Start-up table for trunc Normal parameters
    mth_dens_tnorm <- dplyr::filter(spp_dflts, spp_id == label2id(spp_label)) %>% 
      dplyr::pull(mth_dens) %>% 
      purrr::pluck(1) 
    
    if(is.null(mth_dens_tnorm)){
      mth_dens_tnorm <- data.frame(
        matrix(nrow = 2, ncol = 12, dimnames = list(c("Mean", "SD"), month.name))
      )
    }
    
    # Initialize reactive values
    rv <- reactiveValues(
      tnorm_pars_df = mth_dens_tnorm
    )
    
    
    # Initialize waiter screens for density plots
    w <- waiter::Waiter$new(
      id = c(ns("tnorm_summ_plot"), ns("pctls_dt_plot"), ns("draws_dt_plot")),
      html = waiter::spin_loaders(15, color = "#434C5E")
    )
    
    
    # Sub-modules' server side -------------------------------------------------
    
    c(dens_tnorm, tnorm_iv) %<-% mod_monthly_hotab_server(
      id = "tnorm", 
      band_mode = band_mode, 
      startup_tab =  mth_dens_tnorm, 
      is_pdist = TRUE, 
      col_widths = 78
    )
    
    
    # -- Input Validation  ---------------------------------------------------
    
    ## InputValidator rules -------
    iv <- InputValidator$new()
    
    # trunc normal parameter table
    tnorm_iv$condition(~ input$dttype == "tnorm")
    
    # user's percentiles
    pctl_iv <- InputValidator$new()
    pctl_iv$condition(~ input$dttype == "pctls")
    pctl_iv$add_rule("flinput-pctls", sv_required(message = ""))
    pctl_iv$add_rule("pctlsOK", ~ if(. != "yes") . )
    
    # user's random draws
    draws_iv <- InputValidator$new()
    draws_iv$condition(~ input$dttype == "draws")
    draws_iv$add_rule("flinput-draws", sv_required(message = ""))
    draws_iv$add_rule("drawsOK", ~ if(. != "yes") . )
    
    # add sub-validators to main validator
    iv$add_validator(tnorm_iv)
    iv$add_validator(pctl_iv)
    iv$add_validator(draws_iv)
    
    

    ## Highlight labels of fileInputs if no file uploaded -----
    observe({

      if(is.null(input$"flinput-pctls")){
        shinyjs::js$fontCol(ns("pctls_label"), '#dd4b39')
      }else{
        shinyjs::js$fontCol(ns("pctls_label"),"#333")
      }

      if(is.null(input$"flinput-draws")){
        shinyjs::js$fontCol(ns("draws_label"), '#dd4b39')
      }else{
        shinyjs::js$fontCol(ns("draws_label"),"#333")
      }

    })

    
    # observe({
    #   #browser()
    #   #iv$is_valid()
    #   print(iv$validate())
    # })

    
    # Options for type of density data, based on band_mode -----------------------------
    observeEvent(band_mode(), {
      
      if(band_mode()){
        
        shinyWidgets::updateRadioGroupButtons( 
          session = session, 
          inputId = "dttype",  
          choices = c("Point Estimates" = "tnorm"), 
          selected = "tnorm",
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
          inputId = "dttype",  
          choices = c("Trunc. Normal Distribution" = "tnorm",
                      "Percentile Estimates" = "pctls",
                      "Random Draws" = "draws"),
          # selected = "tnorm",
          checkIcon = list(
            yes = tags$i(class = "fa fa-circle",
                         style = "color: #2D78C3"),
            no = tags$i(class = "fa fa-circle-o",
                        style = "color: #2D78C3")
          )
        )
      }
    })
    
    
    
    # User's density data loading  ---------------------------------------------
    
    ## Read-in percentile estimates uploaded by user -----
    dens_pctls <- reactive({
      
      input_id <- "flinput-pctls"
      file <- input[[input_id]]
      
      # Upload if user has provided file to read data from. Otherwise return NULL
      if(not_null(file)){
        
        #req(file)
        
        shinyFeedback::hideFeedback(input_id)
        
        dens_pctls <- load_dens(file$name, file$datapath, pctl_dt = TRUE)
        
        if(dens_pctls$error){
          updateTextInput(inputId = "pctlsOK", value = dens_pctls$msg)
          shinyFeedback::showFeedbackDanger(input_id, text = dens_pctls$msg)
          NULL
        }else{
          updateTextInput(inputId = "pctlsOK", value = "yes")
          dens_pctls$dt
        }
      }else{
        NULL
      }
    })
    
    
    
    ## Read-in random samples uploaded by user ----------
    dens_draws <- reactive({
      
      input_id <- "flinput-draws"
      file <- input[[input_id]]
      
      # Upload if user has provided file to read data from. Otherwise return NULL
      if(not_null(file)){
      
        #req(file)
        
        shinyFeedback::hideFeedback(input_id)
        
        dens_draws <- load_dens(file$name, file$datapath, pctl_dt = FALSE)
        
        if(dens_draws$error){
          updateTextInput(inputId = "drawsOK", value = dens_draws$msg)
          shinyFeedback::showFeedbackDanger(input_id, text = dens_draws$msg)
          NULL
        }else{
          updateTextInput(inputId = "drawsOK", value = "yes")
          dens_draws$dt
        }
      }else{
        NULL
      }
    })
    
    
    
    # Plots -------------------------------------------------------------
    
    ## Point-range plot for Truncated Normal parameters ----
    output$tnorm_summ_plot <- renderPlot({
      
      w$show()

      if(!band_mode()){
        
        # dens_tnorm() %>%
        #   mutate(
        #     lwBound = qtruncnorm(p = 0.025, mean = Mean, sd = SD, a = 0),
        #     upBound = qtruncnorm(p = 0.975, mean = Mean, sd = SD, a = 0)
        #   ) %>%
        #   ggplot2::ggplot(aes(x = month, y = Mean, group = month)) +
        #   ggplot2::geom_pointrange(aes(ymin=lwBound, ymax = upBound), 
        #                            col = spp_colour, size =0.8) +
        #   ggplot2::labs(
        #     y = bquote('Number of birds per' ~km^2),
        #     x = "", 
        #     title = paste0("Monthly density of ", spp_label," (Means & 95% CIs)")
        #   )
        
        dens_tnorm() %>%
          dplyr::mutate(
            dist = 
              distributional::dist_truncated(
                distributional::dist_normal(Mean, SD), lower = 0)
          ) %>%
          ggplot2::ggplot(ggplot2::aes(x = month, ydist = dist)) +
          #ggdist::stat_eye(
          ggdist::stat_halfeye(
            slab_fill = spp_colour,
            slab_alpha = 0.5,
            slab_colour = "black", 
            slab_size = 0.5, 
            shape = 21,
            point_fill = "white",
            stroke = 1.5,
            point_interval = "mean_qi"
          ) +
          ggplot2::labs(
            y = bquote('Number of birds per' ~km^2),
            x = "",
            title = paste0("Probability distribution of density of ", spp_label, " per month")
          )
        
      }else{
        
        dens_tnorm() %>%
          lolli_plot(
            x = month, 
            y = Mean,
            xlab = "",
            ylab = bquote('Number of birds per' ~km^2), 
            title = paste0("Monthly Density of ", spp_label),
            point_col = spp_colour, 
            line_col = "gray"
          )
      }
    })

    
    ## Plot for percentile estimates --------
    output$pctls_dt_plot <- renderPlot({
      
      req(dens_pctls())
      
      w$show()
      
      densbird_pctl_plot(
        data = dens_pctls(), 
        pctl = pctl, 
        spp_label = spp_label
      )
    })
    
    
    ## Ridge-plot for random draws ---------
    output$draws_dt_plot <- renderPlot({
      
      req(dens_draws())
      
      w$show()
      
      densbird_draws_plot(
        data = dens_draws(), 
        spp_label = spp_label
      )
    })
    
    
    
    
    # Template data downloading ------------------------------------------------
    
    # percentile estimates of monthly densities
    output$dwnld_tmplt_pctls <- downloadHandler(
      filename = function() {
        "monthDens_pctls_template.csv"
      },
      content = function(file) {
        write.csv(monthDens_pctls_template, file, row.names = FALSE)
      }
    )
    
    
    # random samples of monthly densities
    output$dwnld_tmplt_draws <- downloadHandler(
      filename = function() {
        "monthDens_draws_template.csv"
      },
      content = function(file) {
        write.csv(monthDens_draws_template, file, row.names = FALSE)
      }
    )
    
    
    
    
    # -- Data pre-processing for module Outputs --------------------------------

    dens <- reactive({
      
      req(input$dttype)
      
      # select dataset with chosen type of data
      if(input$dttype == "tnorm"){
        dt <- dens_tnorm()
      }else if(input$dttype == "pctls"){
        dt <- dens_pctls()
      }else if(input$dttype == "draws"){
        dt <- dens_draws()
      }
      
      # list comprising data type and dataset
      list(
        dens_type = input$dttype,
        dens_dt = dt
      )
      
    })
     
    
    # -- Module Outputs ------------------------------------------------------
    list(
      iv = iv,
      dens = dens
    )
    
  })
}
  
## To be copied in the UI
# mod_survey_dt_ui("survey_dt_ui_1")
    
## To be copied in the server
# mod_survey_dt_server("survey_dt_ui_1")
