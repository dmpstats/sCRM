#' monthly_hotabs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import rhandsontable
#' @importFrom shiny NS tagList 
mod_monthly_hotab_ui <- function(id, hot_title){
  
  ns <- NS(id)
  
  tagList(
    
    # Hidden textInput to unleash shinyvalidate on the rhandsontable
    shinyjs::hidden(
      textInput(inputId = ns("inputs_ok"), label = "", value = "yes")
    ),
    
    col_12(
      p(id = ns("title"), hot_title),
      fluidRow(
        div(class = "hot-monthly", 
            rhandsontable::rHandsontableOutput(
              outputId = ns("hotab"),
              width = "100%")
        ),
        div(
          class = "hot-feedback", 
          textOutput(ns("iv_fbck"))
        )
      )
    )
  )
}

#' monthly_hotabs Server Functions
#'
#' @import rhandsontable
#' @import shinyvalidate
#' 
#' @noRd 
mod_monthly_hotab_server <- function(id, band_mode, startup_tab, is_pdist, 
                                     col_widths = 70){
  
  stopifnot(is.reactive(band_mode))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Object Initialization --------------------------------------------------
    
    # Initialize non-reactive globals
    sds <- rep(NA, 12)
    
    # Initialize reactive values
    rv <- reactiveValues(
     raw_dt = startup_tab
    )
    
    # -- Input Validation  ---------------------------------------------------
    
    ## InputValidator rules -------
    iv <- InputValidator$new()
    iv$add_rule("inputs_ok", sv_required(message = ""))
    iv$add_rule("inputs_ok", ~ if(. != "yes") . )


    ## rhandsontable validation -----
    observeEvent(input$hotab, {
      
      df <- hot_to_r(input$hotab)
      
      # drop 2nd row with SDs if band_mode is active (and if table of probdist pars)
      if(band_mode()){
        if(is_pdist){
          df <- df[1, ]  
        }
      }
      
      if(all(is.na(df))){ # check for empty table
        
        updateTextInput(inputId = "inputs_ok", value = "")
        shinyjs::js$fontCol(ns("title"), '#dd4b39')
        output$iv_fbck <- ({invisible()})
        
      }else if(any(df < 0, na.rm = TRUE)){ # check for negative values
          
        msg <- "Values must be positive"
        output$iv_fbck <- renderText({msg})
        updateTextInput(inputId = "inputs_ok", value = msg)
        shinyjs::js$fontCol(ns("title"), '#dd4b39')
        
      }else if(incomplete_cols(df)){ # check for incomplete columns
        
        msg <- "Incomplete columns are not permitted"
        output$iv_fbck <- renderText({msg})
        updateTextInput(inputId = "inputs_ok", value = msg)
        shinyjs::js$fontCol(ns("title"), '#dd4b39')
        
      }else{
        
        # All good
        output$iv_fbck <- ({invisible()})
        updateTextInput(inputId = "inputs_ok", value = "yes")
        shinyjs::js$fontCol(ns("title"),"#333")
      }
    })
    
    
    
    
    # Render input handsontable table ------------------------------------------
    output$hotab <- renderRHandsontable({
      
      if(is_pdist){
        row_headers <- c( "Mean", "SD")
      }else{
        row_headers <- NULL
      }
      
      dt <- rv$raw_dt %>%
        rhandsontable(rowHeaders = row_headers) %>%
        hot_cols(
          colWidths = col_widths, 
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
            }") %>%
        hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
      
      if(is_pdist){
        dt <- dt %>% hot_row(2, readOnly = band_mode())
      }
      
      dt

    })
    
    
    # Convert handsontable to data frame ---------------------------------------
    hotab_df <- reactive({
      
      req(input$hotab)
      
      # Note: negative values are converted to NAs
      
      if(is_pdist){
        
        #browser()
        
        hot_to_r(input$hotab) %>%
          tibble::rownames_to_column(var="Variable") %>%
          tidyr::pivot_longer(January:December) %>%
          tidyr::pivot_wider(names_from = Variable) %>%
          dplyr::mutate(across(c(Mean, SD), ~ifelse(Mean < 0 | SD < 0, NA, .))) %>%
          dplyr::mutate(
            month = factor( month.name, levels = month.name),
            Mean = as.numeric(Mean),
            SD = as.numeric(SD)
          )
        
      }else{
        
        hot_to_r(input$hotab) %>%
          tidyr::pivot_longer(cols = everything(), values_to = "value") %>%
          dplyr::mutate(
            value = ifelse(value < 0, NA, value),
            month = factor( month.name, levels = month.name),
            value = as.numeric(value)
          ) %>%
        dplyr::rename({{id}} := value)
      }
      
    })
    
    
    
    # Management of SD values (for probdist tables) ----------------------------
    
    ### If server initialized on band mode, set SDs to 0
    observeEvent(band_mode(), once = TRUE, {
      
      if(is_pdist){
        
        if(band_mode()){
          rv$raw_dt[2, ] <- 0
        }
        
      }
    })
    
    ### Reactively change SD values based on band_mode choice, storing/restoring
    ### previously specified values
    observeEvent(band_mode(), {
      
      if(is_pdist){
        
        # capture current table
        dt <- hot_to_r(input$hotab)
        
        if(band_mode()) {
          # store current sds
          sds <<- dt[2, ]
          # Assign 0s to sds
          dt[2, ] <- 0
          # update reactive
          rv$raw_dt <- dt
        }else{
          # Assign stored sd
          dt[2, ] <- sds
          # update reactive
          rv$raw_dt <- dt  
        }
        
      }
    }, 
    ignoreInit = TRUE, 
    priority = 2
    )
    
    
    
    # -- Module Outputs ------------------------------------------------------
    
    list(
      hotab_df = hotab_df,
      iv = iv
    )
    
  })
}
    
## To be copied in the UI
# mod_monthly_hotab_ui("monthly_hotabs_ui_1")
    
## To be copied in the server
# mod_monthly_hotab_server("monthly_hotabs_ui_1")
