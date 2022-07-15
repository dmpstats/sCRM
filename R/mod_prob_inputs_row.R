#' prob_inputs_row UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_prob_inputs_row_ui <- function(id, par_label, par_dist, dflt_mean, dflt_sd,
                                   step = 0.01, band_mode){
  ns <- NS(id)
  
  #browser()
  
  tagList(
    tags$tr(style = "vertical-align: top",
      tags$th(
        style = "text-align: left; padding-top: 14px;",
        p(id = ns("rowheader"), as.character(par_label))
      ),
      tags$td(
        tagAppendAttributes(
          numericInput(
            inputId = ns("mean"), 
            label = NULL, 
            value = dflt_mean, 
            min = 0,
            max = ifelse(par_dist == "beta", 1, Inf),
            step = step),
          style = "margin-bottom: 0px"
        )
      ),
      tags$td(
        #uiOutput(outputId = ns("sd_input"))
        
        if(band_mode){
          
          shinyjs::disabled(
            tagAppendAttributes(
              numericInput(
                inputId = ns("sd"),
                label = NULL,
                value = 0,
                min = 0,
                step = step),
              style = "margin-bottom: 0px"
            )  
          )
          
        }else{
          tagAppendAttributes(
            numericInput(
              inputId = ns("sd"),
              label = NULL,
              value = dflt_sd,
              min = 0,
              step = step),
            style = "margin-bottom: 0px"
          )  
        }
        
      ),
      tags$td(
        shinyWidgets::dropMenu(
          maxWidth = "350px",
          actionButton(ns("plotbtn"), "", icon = icon("chart-area")),
          trigger = 'click', #c("mouseenter"),
          #showOnCreate = TRUE, 
          theme = "light-border",
          plotOutput(ns("dplot"), 
                     width = "300px", 
                     height = "200px"),
          br(),
          verbatimTextOutput(ns("qtls")),
          placement = "right")
      )
    )
  )
}



#' prob_inputs_row Server Functions
#'
#' @import shinyvalidate
#' @noRd 
mod_prob_inputs_row_server <- function(id, par_label, par_dist, par_name, 
                                       band_mode, plot_fill){
  
  stopifnot(!is.reactive(par_label))
  stopifnot(!is.reactive(par_dist))
  stopifnot(!is.reactive(par_name))
  stopifnot(!is.reactive(plot_fill))
  stopifnot(is.reactive(band_mode))
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    # initialize non-reactive globals
    sd_val <- NA
    
    # --- Input validation -----------------------------------------------------
    
    # --- Initialize input validator variable
    iv <- InputValidator$new()
    
    purrr::walk(
      .x = c("mean", "sd"), 
      .f = function(x){
        
        iv$add_rule(x, sv_required(message = ""))
        iv$add_rule(x, sv_numeric())
        
        if(par_dist == "tnorm"){
          if(x == "mean"){
            iv$add_rule(x, sv_gt(0, message_fmt = "Must be positive"))
          }
          if(x == "sd"){
            iv$add_rule(x, sv_gte(0, message_fmt = "Must be non-negative"))
          } 
        }
        
        if(par_dist == "beta"){
          if(x == "mean"){
            iv$add_rule(x, sv_between(0, 1, message_fmt = "Must be between {left} and {right}"))
          }
          if(x == "sd"){
            iv$add_rule(x, sv_gte(0, "Must be non-negative"))
            
            # Valid SDs of Beta depend on associated Mean value, so the rule below
            # gets updated on selected input for mean. This required a less obvious
            # trick to deal with a dynamic threshold, as posted in:
            # https://stackoverflow.com/questions/69067458/shinyvalidate-using-a-reactive-expression-witin-add-rule
            iv$add_rule(x, function(value){valid_beta_sd(value, input$mean)}) 
          }
        }
      })
    

    # --- Highlight widget background if input missing
    observe({
      par_ids <- c("mean", "sd")
      purrr::walk(par_ids, function(x){
        val <- input[[x]]
        if(not_null(val)){
          if(is.na(val)){
            shinyjs::js$backgroundCol(ns(x), '#fff1f1')
          }else{
            if(band_mode()){
              if(x == "sd"){
                shinyjs::js$backgroundCol(ns(x),"#ededed")  
              }else{
                shinyjs::js$backgroundCol(ns(x),"white")
              }
            }else{
              shinyjs::js$backgroundCol(ns(x),"white")
            }
          }
        }
      })
    })
    
    # --- Set row header text to red if input missing for either mean or sd
    observe({
      if(iv$is_valid()){
        shinyjs::js$fontCol(ns("rowheader"),"#333")
      }else{
        shinyjs::js$fontCol(ns("rowheader"), '#dd4b39')
      }
    })
    


    # --- Dynamic UI -----------------------------------------------------------
    
    # toggle sd numeric input based on band mode, and manage previously defined
    # sd values
    observeEvent(band_mode(), ignoreInit = TRUE, { 
      
      shinyjs::toggleState(id = "sd", condition = !band_mode())
      #shinyjs::toggleState(id = "plotbtn", condition = !band_mode())
      #shinyjs::toggle(id = "dplot", condition = !band_mode())
      
      if(band_mode()){
        # store current sd value
        sd_val <<- input$sd
        # change sd to 0
        updateNumericInput(session, "sd", value = 0)
      }else{
        # retrieve latest sd value
        updateNumericInput(session, "sd", value = sd_val)
      }
      
    })
    
    
    
    # Density plot and quantiles summary
    #observeEvent(input$plotbtn, {
    observe({
      
      if(par_dist == "tnorm") {
        
        output$dplot <- renderPlot({
          tnorm_dplot(
            mean = input$mean, 
            sd = input$sd, 
            lower = 0, 
            fill = plot_fill,
            xlab = par_label)
        })
      
        output$qtls <- renderPrint({
          tnorm_qtl_tbl(
            mean = input$mean,
            sd = input$sd,
            lower = 0,
            varTag = par_name)
        })
         
      }else if(par_dist == "beta"){
        
        output$dplot <- renderPlot({
          beta_dplot(
            p = input$mean, 
            sd = input$sd, 
            xlab = par_label, 
            fill = plot_fill)
        })
        
        output$qtls <- renderPrint({
          beta_qtl_tbl(
            p = input$mean,
            sd = input$sd,
            varTag = par_name)
        })
      }
    })
    
    # return InputValidator object
    list(
      iv = iv,
      par_df = reactive(
        data.frame(mean = input$mean, sd = input$sd)
      )
    )
    
  })
}
    
## To be copied in the UI
# mod_prob_inputs_row_ui("prob_inputs_row_ui_1")
    
## To be copied in the server
# mod_prob_inputs_row_server("prob_inputs_row_ui_1")

