#' fhd_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fhd_inputs_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    
    # waiter::autoWaiter(
    #   id = c(ns("gen_dflt_plot"), ns("gen_other_plot"), ns("site_plot")), 
    #   html = waiter::spin_loaders(4),
    #   color = "black"
    #   ),
    
    # Hidden textInputs to allow for shinyvalidate on user's uploaded data
    shinyjs::hidden(
      purrr::map(
        c("sitefhd_OK", "othergenfhd_OK", "dfltgenfhd_OK"), function(x){
          textInput(inputId = ns(x), label = "", value = "yes")
        }
      )
    ),
    
    col_6(
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("type"),
        label = "Type of flight height distribution data", 
        choices = list(
          "Generic" = "gen_fhd",
          "Site-specific" = "site_fhd"
        ),
        #status = "primary",  
        selected = "gen_fhd",
        individual = TRUE,
        justified = TRUE,
        checkIcon = list(
          yes = tags$i(
            class = "fa fa-check-square",
            style = "color: #2D78C3"),
          no = tags$i(
            class = "fa fa-square-o",
            style = "color: #2D78C3")
          )
      )
    ),
    br(),
    br(),
    br(),
    br(),
    
    col_12(
      shinyWidgets::verticalTabsetPanel(
        color = "#434C5E",
        id = ns("vtbpnl"),
        contentWidth = 10,
        
        shinyWidgets::verticalTabPanel(
          title = span("Generic FHD", class = "vtab-title"),
          icon = icon("globe"), 
          box_height = "215px",
          uiOutput(ns("genfhd_inputs"))
        )
      )
    ),
    fluidRow(
      br(),
      br()  
    )
  )
}

    
#' fhd_inputs Server Functions
#'
#' @import shinyvalidate
#' 
#' @noRd 
mod_fhd_inputs_server <- function(id, spp_label, band_mode){
  
  stopifnot(is.reactive(band_mode))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    env <- environment()
    env$prev_fhd_type <- c("gen_fhd")
    
    
    # Initialize waiter screens for fhd plots ---------------------------------
    w <- waiter::Waiter$new(
      id = c(ns("gen_dflt_plot"), ns("gen_other_plot"), ns("site_plot")),
      html = waiter::spin_loaders(15, color = "#434C5E") #f37403")
      )
    
    
    # -- Input Validation  ---------------------------------------------------

    # Initialize main validator
    iv <- InputValidator$new()
    
    
    iv$add_rule("type", sv_required(message = ""))


    ## -- Conditional validators

    # Site FHD
    site_iv <- InputValidator$new()
    site_iv$condition(~ "site_fhd" %in% input$type)

    site_iv$add_rule("flinput-sitefhd", sv_required(message = ""))
    site_iv$add_rule("sitefhd_OK", sv_required())
    site_iv$add_rule("sitefhd_OK", ~ if(. != "yes") . )

    # User's Generic FHD
    usergen_iv <- InputValidator$new()
    usergen_iv$condition(~ isTRUE(("gen_fhd" %in% input$type) && (input$gensrc == 'other')))

    usergen_iv$add_rule("flinput-othergenfhd", sv_required(message = ""))
    usergen_iv$add_rule("othergenfhd_OK", sv_required())
    usergen_iv$add_rule("othergenfhd_OK", ~ if(. != "yes") . )


    # Default generic FHD
    dfltgen_iv <- InputValidator$new()
    dfltgen_iv$condition(~ isTRUE(("gen_fhd" %in% input$type) && (input$gensrc == 'default')))
    
    dfltgen_iv$add_rule("dfltgenfhd_OK", sv_required())
    dfltgen_iv$add_rule("dfltgenfhd_OK",  ~ if(. != "yes") . )


    ## -- Append conditional validators to main validator
    iv$add_validator(site_iv)
    iv$add_validator(usergen_iv)
    iv$add_validator(dfltgen_iv)
    
    
    
    # -- Dynamic UI: Append/Remove vertical tabset panels ------------------------
    
    observeEvent(input$type, ignoreInit = TRUE, ignoreNULL = FALSE, {
      
      # case when of the types is deselected
      if(length(input$type) < length(env$prev_fhd_type)){
        
        # remove tab
        tab_to_rmv <- setdiff(env$prev_fhd_type, input$type)
        tab_to_rmv_idx <- which(tab_to_rmv == env$prev_fhd_type)
        
        shinyWidgets::removeVerticalTab(
          inputId = ns("vtbpnl"), 
          tab_to_rmv_idx)
        
        # select outstanding tab
        shinyWidgets:::updateVerticalTabsetPanel(
          session = session,
          inputId = "vtbpnl",
          selected = ifelse(tab_to_rmv == "site_fhd", 
                            "Generic FHD", 
                            "Site-specific FHD")
        )
        
        # Update previously selected tabs
        env$prev_fhd_type <- input$type
        
      }else if(length(input$type) > length(env$prev_fhd_type)){
        
        # Append tab
        tab_to_append <- input$type[input$type %not_in% env$prev_fhd_type]
        
        if(tab_to_append == "site_fhd"){
          
          # append site FHD tab
          shinyWidgets::appendVerticalTab(
            inputId = ns("vtbpnl"),
            shinyWidgets::verticalTabPanel(
              title = span("Site-specific FHD", class = "vtab-title"),
              icon = icon("location-dot", verify_fa = FALSE),
              box_height = "215px",
              
              # rendered site fhd inputs
              uiOutput(ns("sitefhd_inputs"))
            )
          )
          
        }else{
          
          # append generic FHD tab
          shinyWidgets::appendVerticalTab(
            inputId = ns("vtbpnl"),
            shinyWidgets::verticalTabPanel(
              title = "Generic FHD",
              icon = icon("globe"),
              box_height = "215px",
              
              # rendered generic fhd inputs
              uiOutput(ns("genfhd_inputs"))
            )
          )
        }
        
        # select added tab
        shinyWidgets:::updateVerticalTabsetPanel(
          inputId = "vtbpnl",
          selected = ifelse(tab_to_append == "site_fhd", 
                            "Site-specific FHD", 
                            "Generic FHD"),
          session = session
        )
        
        # Update previously selected tabs
        env$prev_fhd_type <- c(env$prev_fhd_type, tab_to_append)
      }
    })
    
    
    
    
    # Dynamic UI: render generic FHD inputs   ----------------------------------
    output$genfhd_inputs <- renderUI({
      
      tagList(
        col_9(
          shinyWidgets::radioGroupButtons(
            inputId = ns("gensrc"),
            label = "Data source",
            choices = list(
              "Johnson et al (2014)" = "default",
              "Other" = "other"),
            size = "normal",
            #status = "primary",
            selected = "default",
            individual = TRUE,
            justified = TRUE,
            checkIcon = list(
              yes = tags$i(
                class = "fa fa-circle",
                style = "color: #2D78C3"),
              no = tags$i(
                class = "fa fa-circle-o",
                style = "color: #2D78C3"))
          )
        ),
        
        rep_br(5),
        
        # Panel for default FHD data
        conditionalPanel(
          condition = "input.gensrc == 'default'",
          ns = ns,
          col_12(
            plotOutput(ns("gen_dflt_plot"), width = "100%", height = 350)
            ),
          div(
            a("Source: Johnson et al (2014)",
              icon("up-right-from-square", verify_fa = FALSE),
              href ="http://onlinelibrary.wiley.com/doi/10.1111/1365-2664.12191/full", 
              target = "_blank"),
            style = "text-align: right; font-size: 12px"
          ),
          br()
        ),
        conditionalPanel(
          condition = "input.gensrc == 'other'",
          ns = ns,
          col_7(
            fileInput(
              inputId = ns("flinput-othergenfhd"),
              label = "Upload generic data",
              multiple = FALSE,
              accept = c(".csv")) %>%
              bsplus::bs_embed_tooltip(
                title = file_tooltip_text(band_mode()),
                placement = "left"
              )
          ),
          col_1(
            style = "margin-top: 25px; margin-left: 3px",
            downloadButton(
              outputId = ns("dwnld_tmplt_fhd_gen"), 
              label = NULL, 
              class = "dwnld-butt") %>%
              bsplus::bs_embed_tooltip(
                title = dwnl_tooltip_text(band_mode()), 
                placement = "right")
          ),
          col_12(
            br(),
            plotOutput(ns("gen_other_plot"), width = "100%", height = 350)
          )
        )
      )
    })
    
    
    
    # Dynamic UI: render site-specific FHD inputs -------------------------------
    output$sitefhd_inputs <- renderUI({ 
      
      tagList(
        col_7(
          fileInput(
            inputId = ns("flinput-sitefhd"),
            label = "Upload site-specific data",
            multiple = FALSE,
            accept = c(".csv")) %>%
            bsplus::bs_embed_tooltip(
              title = file_tooltip_text(band_mode()), 
              placement = "left")
        ),
        col_1(
          style = "margin-top: 25px; margin-left: 3px",
          downloadButton(
            outputId = ns("dwnld_tmplt_fhd_site"), 
            label = NULL, 
            class = "dwnld-butt") %>%
            bsplus::bs_embed_tooltip(
              title = dwnl_tooltip_text(band_mode()), 
              placement = "right")
        ),
        col_12(
          plotOutput(ns("site_plot"), width = "100%", height = 350)
        )
      )
      
    })
    
    
    
    # Generic FHD data ---------------------------------------------------------
    
    ## Default data (Johnson's et al, 2014) ----------------------------------
    
    # Both datasets are available as list elements of `spp_dflts`, one of the
    # {sCRM} datasets
    
    gen_fhd_dflt <- reactive({
      
      w$show()
      
      if(!band_mode()){
        
        # subset bootstraps for current species
        dplyr::filter(spp_dflts, spp_id == label2id(spp_label)) %>%
          dplyr::pull(fhd_boot) %>%
          purrr::pluck(1)
        
      }else{
        
        # subset estimates for current species
        dplyr::filter(spp_dflts, spp_id == label2id(spp_label)) %>%
          dplyr::pull(fhd_est) %>%
          purrr::pluck(1) %>%
          dplyr::rename(prop = est)
      }
    })
    
    
    ## User's FHD data ---------------------------------------------------------
    
    # Read-in generic FHD uploaded by user, with formatting dependent on
    # band_mode.
    #
    # Content data is also validated here. Using both {shinyFeedback} and
    # {shinyvalidate} together - the former provides better-looking feedback on
    # fileInput widgets, but it doesn't offer the key validation support
    # available on the latter. Data content validation is traced by a hidden
    # input, which itself is tracked by the {shinyvalidate} machinery
    
    gen_fhd_other <- reactive( {
      
      #req(input$"flinput-othergenfhd")
      
      
      # Only upload if user has specified a file to read from. Otherwise return NULL
      if(not_null(input$"flinput-othergenfhd")){
        
        w$show()
        
        input_id <- "flinput-othergenfhd"
        
        # get file info
        file <- input[[input_id]]
        
        # hide any old feedback message, if existent
        shinyFeedback::hideFeedback(input_id)
        
        # load -> validate -> process data
        if(!band_mode()){
          # loading for FHD boostrap replicates
          g_fhd <- load_fhd_boot(name = file$name, path = file$datapath) 
        }else{
          # loading for FHD estimates
          g_fhd <- load_fhd_est(name = file$name, path = file$datapath) 
        }
        
        # Provide error feedback or return processed daa
        if(g_fhd$error){
          # {shinyFeedback}
          shinyFeedback::showFeedbackDanger(input_id, text = g_fhd$msg)
          # Update tracer with error message, which is being tracked by {shinyvalidate}
          updateTextInput(inputId = "othergenfhd_OK", value = g_fhd$msg)
          NULL
        }else{
          # Update tracer with OK'ed data
          updateTextInput(inputId = "othergenfhd_OK", value = "yes")
          # return data
          g_fhd$dt
        }
        
      }else{
        NULL
      }
    })
    
    
    # Site-specific FHD data ---------------------------------------------------------
    
    # Read-in site FHD uploaded by user, with formatting dependent on band_mode.
    # As above, content data also validated on the fly using both
    # {shinyFeedback} and {shinyvalidate} together
    
    site_fhd <- reactive({
      
      #req(input$"flinput-sitefhd")
      
      # Only upload if user has specified a file to read from. Otherwise return NULL.
      if(not_null(input$"flinput-sitefhd")){
        
        w$show()
        
        input_id <- "flinput-sitefhd"
        file <- input[[input_id]]
        
        shinyFeedback::hideFeedback(input_id)
        
        # load -> validate -> process data
        if(!band_mode()){
          s_fhd <- load_fhd_boot(file$name, file$datapath)
        }else{
          s_fhd <- load_fhd_est(file$name, file$datapath)
        }
        
        # Provide error feedback or return processed data
        if(s_fhd$error){
          shinyFeedback::showFeedbackDanger(input_id, text = s_fhd$msg)
          updateTextInput(inputId = "sitefhd_OK", value = s_fhd$msg)
          NULL
        }else{
          updateTextInput(inputId = "sitefhd_OK", value = "yes")
          s_fhd$dt
        }
        
      }else{
        NULL
      }
    })
    
    
    
    # Output Plots -------------------------------------------------------------
    
    # Default generic FHD
    output$gen_dflt_plot <- renderPlot({
      
      # Validation under {shinyvalidate}
      if(is.null(gen_fhd_dflt())){
        updateTextInput(inputId = "dfltgenfhd_OK", value = "Default FHD not available")
      }else{
        updateTextInput(inputId = "dfltgenfhd_OK", value = "yes")
      }      
      
      # Validation and feedback to plot rendering area
      validate(
        need(!is.null(gen_fhd_dflt()),
             paste0("Error: default FHD data for ", spp_label, " not available. ",
                    "Select 'Other' to upload data from a different source." )),
        errorClass = "valErrorMsgClass"
      )
      
      
      if(!band_mode()){
        
        # Heatmap plot
        fhd_boots_heatmap(
          data = gen_fhd_dflt(),
          height = height, 
          spp_label = spp_label
        )
        
      }else{
        
        # lolli plot
        fhd_lolli_plt(
          data = gen_fhd_dflt(), 
          height = height, 
          prop = prop, 
          spp_label = spp_label
        )
      }
      
    })
    
    
    
    # User's generic FHD plots
    output$gen_other_plot <- renderPlot({
      
      req(gen_fhd_other())
      
      if(!band_mode()){
        
        fhd_boots_heatmap(
          data = gen_fhd_other(),
          height = height, 
          spp_label = spp_label
        )
        
      }else{
        
        fhd_lolli_plt(
          data = gen_fhd_other(), 
          height = height, 
          prop = prop, 
          spp_label = spp_label
        )
      }
    })
    
    
    # User's site-specific FHD plots
    output$site_plot <- renderPlot({
      
      req(site_fhd())

      if(!band_mode()){
        
        fhd_boots_heatmap(
          data = site_fhd(),
          height = height, 
          spp_label = spp_label
        )
        
      }else{
        
        fhd_lolli_plt(
          data = site_fhd(), 
          height = height, 
          prop = prop, 
          spp_label = spp_label
        )
      }
    })

    
    
    # Template data downloads --------------------------------------------------
    
    # Generic FHD data
    output$dwnld_tmplt_fhd_gen <- downloadHandler(
      
      filename = function() {
        ifelse(!band_mode(), "FHD_bootstraps_template.csv", "FHD_template.csv")
      },
      content = function(file) {
        if(!band_mode()){
          write.csv(FHD_bootstrap_template, file, row.names = FALSE)
        }else{
          write.csv(FHD_template, file, row.names = FALSE)
        }
      }
    )
    
    
    # site FHD data
    output$dwnld_tmplt_fhd_site <- downloadHandler(
      
      filename = function() {
        ifelse(!band_mode(), "FHD_bootstraps_template.csv", "FHD_template.csv")
      },
      content = function(file) {
        if(!band_mode()){
          write.csv(FHD_bootstrap_template, file, row.names = FALSE)
        }else{
          write.csv(FHD_template, file, row.names = FALSE)
        }
      }
    )
    
    
    
    
    # -- Data pre-processing for Module Outputs --------------------------------
    
    fhd <- reactive({
      
      req(input$gensrc)
      
      # Generic FHD - select chosen data source
      if("gen_fhd" %in% input$type){
        if(input$gensrc == 'default'){
          g_fhd <- gen_fhd_dflt()
        }else if(input$gensrc == 'other'){
          g_fhd <- gen_fhd_other()
        }
      }else{
        g_fhd <- NULL
      }
      
      # Site-specific FHD
      if("site_fhd" %in% input$type){
        s_fhd <- site_fhd()
      }else{
        s_fhd <- NULL
      }
      
      # list comprising types of FHD data
      list(
        fhd_type = input$type,
        gen_fhd = g_fhd,
        site_fhd = s_fhd
      )
    })
    
    
    # -- Module Outputs --------------------------------------------------------
    list(
      iv = iv,
      fhd = fhd
    )

  })
}
    
## To be copied in the UI
# mod_fhd_inputs_ui("fhd_inputs_ui_1")
    
## To be copied in the server
# mod_fhd_inputs_server("fhd_inputs_ui_1")
