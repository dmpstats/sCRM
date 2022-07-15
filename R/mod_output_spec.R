#' output_spec UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_output_spec_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    
    # Trick with hidden textInput to allow shinyvalidate on rhandsontable
    shinyjs::hidden(
      textInput(inputId = ns("hotseasons_ok"), label = "", value = "yes")
    ),
    
    # Trick with hidden textInput to allow shinyvalidate on annum option
    shinyjs::hidden(
      textInput(inputId = ns("annum_ok"), label = "", value = "yes")
    ),
    
    shinyWidgets::prettyRadioButtons(
      inputId = ns("output_aggr"),
      label = strong("Aggregation level for collision estimates", id = ns("agg_level_label")), 
      choices = c(
        "Per Annum" = "annum",
        "By Season" = "seasons",
        "By Month" = "months"
      ),
      fill = FALSE
    ),
    
    # render text for annum validation feedback
    div(
      class = "hot-feedback",
      textOutput(ns("annum_iv_fbck"))
    ),
    
    hr(),
    
    conditionalPanel(
      condition = "input.output_aggr == 'seasons'",
      ns = ns,
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
}
    
#' output_spec Server Functions
#'
#' @import shinyvalidate
#' @import zeallot
#' 
#' @noRd 
mod_output_spec_server <- function(id, spp_label, wf_label, wf_oper, dens){
  
  stopifnot(is.reactive(wf_oper))
  stopifnot(is.reactive(dens))
  
  stopifnot(!is.reactive(spp_label))
  stopifnot(!is.reactive(wf_label))
  
  
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
    
    
    # Input validation -----------------------------------------------------
    
    ## InputValidator rules ------------
    
    # Initialize input validator variable
    iv <- InputValidator$new()
    
    ## Conditional validation  -----
    seasons_iv <- InputValidator$new()
    #seasons_iv$condition(~ input$swtchseasons == TRUE)
    seasons_iv$condition(~ input$output_aggr == 'seasons')
    seasons_iv$add_rule("hotseasons_ok", sv_required(message = ""))
    seasons_iv$add_rule("hotseasons_ok", ~ if(. != "yes") . )
    
    annum_iv  <- InputValidator$new()
    annum_iv$condition(~ input$output_aggr == 'annum')
    annum_iv$add_rule("annum_ok", sv_required(message = ""))
    annum_iv$add_rule("annum_ok", ~ if(. != "yes") . )
    
    # Append conditional validators
    iv$add_validator(seasons_iv)
    iv$add_validator(annum_iv)
    
    
    
    
    ## Extract months covered by each monthly dataset ------------
    monthly_dt_months <- reactive({
      
      req(input$output_aggr)
      
      if(input$output_aggr %in% c('seasons', 'annum')){
        
        out <- list()
        
        # wf downtime
        out$dwnt_months <- wf_oper()$trbdwnt %>%
          tidyr::drop_na() %>%
          pull(month) %>%
          as.character()
        
        # wind availability
        out$wind_months <- wf_oper()$wndavlb %>%
          tidyr::drop_na() %>%
          pull(month) %>%
          as.character()
        
        # density data
        c(dens_type, dens_dt) %<-% dens()
        
        if(dens_type == "tnorm"){
          out$dens_mths <- tidyr::drop_na(dens_dt) %>%
            pull(month) %>%
            as.character()
        }else if(not_null(dens_dt)){
          out$dens_mths <- tidyr::drop_na(dens_dt) %>% 
            names() %>%
            intersect(., month.name)
        }else{
          out$dens_mths <- c()
        }
        
        return(out)
        
      }else{
        NULL
      }
    })
    
    
    
    ## Annual outputs data validation ----------------
    observe({
      
      req(input$output_aggr)
      
      if(input$output_aggr == 'annum'){
        
        c(dwnt_months, wind_months, dens_mths = NULL) %<-% monthly_dt_months()
        
        req(dens_mths)
        
        msg_fn <- function(txt){
          glue::glue(
            "Missing values for at least 1 month in {txt}.", 
            " Per Annum outputs require complete monthly datasets.")
        }
        
        if(!identical(dens_mths, month.name)){
          
          msg <- msg_fn("In-flight Density data")
          output$annum_iv_fbck <- renderText({msg})
          shinyjs::js$fontCol(ns("agg_level_label"), '#dd4b39')
          updateTextInput(inputId = "annum_ok", value = msg)
          
        }else if(!identical(dwnt_months, month.name)){
          
          msg <- glue::glue(msg_fn("Maintenance Downtime data for {wf_label}"))
          output$annum_iv_fbck <- renderText({msg})
          shinyjs::js$fontCol(ns("agg_level_label"), '#dd4b39')
          updateTextInput(inputId = "annum_ok", value = msg)
          
        }else if(!identical(wind_months, month.name)){
          
          msg <- glue::glue(msg_fn("Wind Availability data for {wf_label}"))
          output$annum_iv_fbck <- renderText({msg})
          shinyjs::js$fontCol(ns("agg_level_label"), '#dd4b39')
          updateTextInput(inputId = "annum_ok", value = msg)
          
        }else{  # All good
          
          output$annum_iv_fbck <- renderText({invisible()})
          updateTextInput(inputId = "annum_ok", value = "yes")
          shinyjs::js$fontCol(ns("agg_level_label"), "#333")
          
        }
        
      }else{ # all okay when option is not per Annum
        
        output$annum_iv_fbck <- renderText({invisible()})
        updateTextInput(inputId = "annum_ok", value = "yes")
        shinyjs::js$fontCol(ns("agg_level_label"), "#333")
      }
      
    })
    
    
    
    
    ## Seasonal periods data validation --------------
    observe({
      
      req(input$hotseasons)
      
      #if(input$swtchseasons){
      if(input$output_aggr == 'seasons'){
        
        # seasons
        seas_df <- hot_to_r(input$hotseasons) %>%
          dplyr::mutate(dplyr::across(.fns = ~ifelse( . == "", NA, .)))
        
        c(dwnt_months, wind_months, dens_mths = NULL) %<-% monthly_dt_months()
        
        req(dens_mths)
        
        if(all(is.na(seas_df))){   # Check for empty table
          
          shinyjs::js$fontCol(ns("seasons_label"), '#dd4b39')
          updateTextInput(inputId = "hotseasons_ok", value = "")
          
        }else if(any(is.na(seas_df))){  # Check for any missing value
          
          msg <- "All cells must be populated"
          output$seasons_iv_fbck <- renderText({msg})
          shinyjs::js$fontCol(ns("seasons_label"), '#dd4b39')
          updateTextInput(inputId = "hotseasons_ok", value = msg)
          
        }else if(seas_uncovered(seas_df, dens_mths)){  # check if period is covered by density data
          msg <- "Specified seasonal period(s) comprising at least 1 month with no density data."
          output$seasons_iv_fbck <- renderText({msg})
          shinyjs::js$fontCol(ns("seasons_label"), '#dd4b39')
          updateTextInput(inputId = "hotseasons_ok", value = msg) 
          
        } else if(seas_uncovered(seas_df, dwnt_months)){ # check if period is covered by wf downtime
          msg <- glue::glue("Specified seasonal period(s) comprising at least 1 month with no",
                            " maintenance downtime data - see Monthly Operation",
                            " section of {wf_label}.")
          output$seasons_iv_fbck <- renderText({msg})
          shinyjs::js$fontCol(ns("seasons_label"), '#dd4b39')
          updateTextInput(inputId = "hotseasons_ok", value = msg) 
          
        }else if(seas_uncovered(seas_df, wind_months)){ # check if period is covered by wf wind availability
          msg <- glue::glue("Specified seasonal period(s) comprising at least 1 month with no", 
                            " wind availability data - see Monthly Operation",
                            " section of {wf_label}.")
          output$seasons_iv_fbck <- renderText({msg})
          shinyjs::js$fontCol(ns("seasons_label"), '#dd4b39')
          updateTextInput(inputId = "hotseasons_ok", value = msg) 
          
        } else {  # All good
          output$seasons_iv_fbck <- renderText({invisible()})
          shinyjs::js$fontCol(ns("seasons_label"),"#333")
          updateTextInput(inputId = "hotseasons_ok", value = "yes")
          
        }
      }
    })
    
    
    
    # Input table for seasonal definitions --------------------------------------
    output$hotseasons <- renderRHandsontable({
      
      seasons_df %>%
        rhandsontable(
          height = 200,
          #stretchH = "all",
          rowHeaders = NULL,
          colHeaders = c(
            "Period", #"Period\nName",
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
    
    
    ## Assign hotseasons table to reactive , for exporting --------------------
    seas_dt <- reactive({
      
      if(input$output_aggr == "seasons"){
        hot_to_r(input$hotseasons)
      }else{
        NULL
      }
        
    })
    
    
    # -- Module Outputs --------------------------------------------------------
    list(
      iv = iv,
      out_opt = reactive(input$output_aggr),
      seas_dt = seas_dt
    )
 
  })
}
    
## To be copied in the UI
# mod_output_spec_ui("output_spec_ui_1")
    
## To be copied in the server
# mod_output_spec_server("output_spec_ui_1")
