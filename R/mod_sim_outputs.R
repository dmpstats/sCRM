#' sim_outputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sim_outputs_ui <- function(id, scrm_outputs, band_mode){
  
  ns <- NS(id)
  
  # List with UI for vertical tabs (for wfs) and their children tabboxes (for spps in wfs)
  wf_vtab_ls <- purrr::imap(scrm_outputs, function(wf_out, wf_id){
    
    spps_tbpnl_ls <- purrr::imap(wf_out, function(spp_out, spp_id){
      
      tabPanel(
        title = spp_out$spp_label, 
        mod_spp_results_ui(
          id = ns(glue::glue("{wf_id}-{spp_id}")),
          out_opt = spp_out$out_opt,
          band_mode = band_mode
        )
      )
      
    }) %>% 
      unname()
    
    shinyWidgets::verticalTabPanel(
      title = span(wf_out[[1]]$wf_label, class = "vtab-title"),
      rlang::exec(shinydashboard::tabBox, !!!spps_tbpnl_ls, width = 12)
    )
  })

  
  tagList(
    
    shinydashboardPlus::box(
      title = "Outputs",
      width = 10,
      solidHeader = TRUE,
      status = "primary",
      
      
      col_8(  
        div(
          class = "hot-feedback",
          uiOutput(ns("outstatus"))
        )
      ),
      
      col_4(
        span(
          style = "float:right;",
          
          # Download scrm run data (inputs and outputs)
          tagAppendAttributes(
            shinyWidgets::downloadBttn( 
              outputId = ns("outdwnld"),
              label = span("Download Outputs", style = "font-size: 14px"),
              icon = icon("download"),
              color = "primary",
              style = "simple",
              size = "sm",
              block = FALSE
            ),
            style = "background: #434C5E"
          ), #%>%
          #bsplus::bs_embed_tooltip("Download Outputs", placement = "bottom"),
          
          
          # Generate report of the run
          tagAppendAttributes(
            shinyWidgets::downloadBttn(
              outputId = ns("outreport"),
              label = span("Generate Report", style = "font-size: 14px"),
              icon = icon("file-lines",  verify_fa = FALSE),
              color = "primary",
              style = "simple",
              size = "sm",
              block = FALSE
            ),
            style = "background: #434C5E;"
          )
        )
      ),
      shiny::fluidRow(),
      br(),
      
      # exec required to run reactive list, one per windfarm
      rlang::exec(
        shinyWidgets::verticalTabsetPanel,
        id = ns("vtabwf"),
        contentWidth = 11,
        color = "#434C5E",
        !!!wf_vtab_ls
      )
      
      # shinyWidgets::radioGroupButtons(
      #   inputId = ns("output_aggr_opt"),
      #   individual = TRUE,
      #   label = NULL, #"Aggregation level",
      #   choices = c("By Windfarm" = "wf_level",
      #               "Cumulative" = "cumm_level"),
      #   checkIcon = list(
      #     yes = tags$i(class = "fa fa-circle",
      #                  style = "color: #2D78C3"),
      #     no = tags$i(class = "fa fa-circle-o",
      #                 style = "color: #2D78C3"))
      # ),
      # 
      # conditionalPanel(
      #   #condition = "input.swtchcmltv == false",
      #   condition = "input.output_aggr_opt == 'wf_level'",
      #   ns = ns,
      #   
      #   # exec required to run reactive list, one per windfarm
      #   rlang::exec(
      #     shinyWidgets::verticalTabsetPanel,
      #     id = ns("vtabwf"),
      #     contentWidth = 11,
      #     color = "#434C5E",
      #     !!!wf_vtab_ls
      #   )
      # ),
      # 
      # 
      # conditionalPanel(
      #   #condition = "input.swtchcmltv == true",
      #   condition = "input.output_aggr_opt == 'cumm_level'",
      #   ns = ns,
      #   
      #   shinyWidgets::verticalTabsetPanel( 
      #     id = ns("tbstpnl_cmltv"), 
      #     contentWidth = 11,
      #     color = "#434C5E",
      #     shinyWidgets::verticalTabPanel(
      #       title = span("Cumulative", class = "vtab-title"),
      #       box_height = "300px", 
      #       shinydashboard::tabBox(
      #         width = 12,
      #         tabPanel(title = "Species 1", "species 1 outputs"),
      #         tabPanel(title = "Species 2", "species 2 outputs"),
      #         tabPanel(title = "Species 3", "species 3 outputs")
      #       )
      #     )
      #   )
      # )
    )
  )
}



   
#' sim_outputs Server Functions
#' 
#' @import zeallot
#'
#' @noRd 
mod_sim_outputs_server <- function(id, scrm_inputs, scrm_outputs, band_mode, niter, inout_synced){
  
  stopifnot(is.reactive(scrm_outputs))
  stopifnot(is.reactive(inout_synced))
  stopifnot(!is.reactivevalues(scrm_inputs))
  stopifnot(!is.reactive(scrm_inputs))
  stopifnot(!is.reactive(band_mode))
  stopifnot(!is.reactive(niter))

  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    # Reactive list with displayable results (collisions estimates),
    # for each spp-in-wf combination
    # 2-level depth nested list: scrm_results[[wf]][[spp]]
    scrm_results <- eventReactive(scrm_outputs(), {
      
      purrr::imap(scrm_outputs(), function(wf_out, wf_id){
        purrr::imap(wf_out, function(spp_out, spp_id){
          
          # Plots
          plot <- plot_spp_collisions(spp_out, band_mode)
          
          # Summary objects (dataframe and flexitable)
          c(summ_df, summ_ft) %<-% summ_spp_collisions(spp_out, band_mode)
          
          # output as list
          list(
            plot = plot, 
            summ_df = summ_df, 
            summ_ft = summ_ft,
            wf_label = spp_out$wf_label,
            spp_label = spp_out$spp_label
          )
        })
      })
    })
  
    

    # generate module server elements for results for each species-at-wf combo
    purrr::iwalk(scrm_results(), function(wf, wf_id){
      purrr::iwalk(wf, function(spp_results, spp_id){
        
        mod_spp_results_server(
          id = glue::glue("{wf_id}-{spp_id}"),
          coll_plot = spp_results$plot,
          coll_summ_ft = spp_results$summ_ft
        )
        
      })
    })
    
    
    # Download inputs and outputs of current run
    output$outdwnld <- downloadHandler(
      filename = glue::glue("scrm_outputs_{time_tag()}.zip"),
      
      content = function(file){
        
        # Create temporary directory
        sess_tmp_dir <- fs::dir_create(fs::file_temp("scrm_outputs"))
        
        # create inputs and outputs sub-directories
        outputs_dir <- fs::dir_create(fs::path(sess_tmp_dir, "outputs"))
        inputs_dir <- fs::dir_create(fs::path(sess_tmp_dir, "inputs"))
        
        # Write-out inputs to files in temporary sub-folder
        export_inputs(scrm_inputs, inputs_dir, band_mode)
        
        # Write-out outputs to files in temporary sub-folder
        export_outputs(scrm_outputs(), scrm_results(), outputs_dir, band_mode)
        
        #browser()
        
        # compress all into a zip file
        zip::zip(
          zipfile = file,
          files = c(outputs_dir, inputs_dir), 
          mode = "cherry-pick"
        )
      },
      contentType = "application/zip"
    )
    
    
    
    
    # Download automatically generated report for current run
    output$outreport <- downloadHandler(
      filename = glue::glue("scrm_report_{time_tag()}.docx"), 
      
      content = function(file){
        
        #waitress_outreport$start()
        
        # Copy report file to a temporary directory before processing, 
        # to avoid issues with writing rights while in deployment
        temp_report_dir <- fs::dir_create(fs::file_temp("report_temp"))
        temp_report_path <- fs::path(temp_report_dir, "sCRM_report.rmd")
        temp_report_template <- fs::path(temp_report_dir, "sCRM_report_template.docx")
        
        fs::file_copy(
          path = "inst/app/markdown/scrm_report/sCRM_report.rmd", 
          new_path = temp_report_path, 
          overwrite = TRUE
        )
        
        fs::file_copy(
          path = "inst/app/markdown/scrm_report/sCRM_report_template.docx", 
          new_path = temp_report_template, 
          overwrite = TRUE
          )
        
        id <- showNotification(
          "Rendering report...",
          duration = NULL,
          type = "message",
          closeButton = FALSE
        )
        on.exit(removeNotification(id), add = TRUE)
        
        rmarkdown::render(
          temp_report_path, 
          output_file = file,
          params = list(
            scrm_inputs = scrm_inputs, 
            scrm_results = scrm_results(),
            band_mode = band_mode,
            niter = niter
          ),
          envir = new.env(parent = globalenv())
        )
                          
      }
    )
    
    
    
    
    output$outstatus <- renderUI({
      if(inout_synced()){
        invisible()
      }else{
        tagList( # style = "color: #d40404;"
          p(
            icon("exclamation-circle"),
            "Inputs have been modified since last CRM Run. Outputs displayed",
            " below are therefore out of date.",
            "Hit ",
            img(src='www/img_run_sCRM_bttn.png', height="7%", width="7%", 
                style = "border: 1px solid; color: black"),
            " to re-calculate and generate outputs for the currently specified inputs"
          )
        )
      }
    })
      
    
    
    

    # # Reactive list with draws of cumulative collisions for each species across windfarms
    # scrm_cumm_outputs <- eventReactive(scrm_outputs(), {
    #   
    #   # stack up all the outputs
    #   stacked_outputs <- purrr::map_dfr(scrm_outputs(), .id = "wf_id", function(wf_out){
    #     purrr::map_dfr(wf_out, .id = "spp_id", function(spp_out){
    #       
    #       # drop seaonality data, if present 
    #       spp_out["season_dt"] <- NULL
    #       
    #       dplyr::bind_rows(spp_out) %>%
    #         tidyr::unnest(colls)
    #     })
    #   })
    #   
    #   # Calculate cumulative collision estimates across winfarms, for each species
    #   # For each spp, two possibilities:
    #   # (i) if estimates across wfs are in equal temporal scale, aggregate at that scale
    #   # (ii) otherwise, aggregate at per Annum scale
    #   stacked_outputs %>%
    #     split(.$spp_id) %>%
    #     purrr::map(function(x){
    #       out_types <- unique(x$out_opt)
    #       
    #       windfarms <- glue::glue_collapse(unique(x$wf_label), " + ")
    #       
    #       if(length(out_types) == 1){
    #         
    #         x %>%
    #           dplyr::group_by(spp_label, out_opt, crm_option, time_period, iter) %>%
    #           dplyr::summarise(collisions = sum(collisions)) %>%
    #           tibble::add_column(windfarms = windfarms, .after = "spp_label")
    #         
    #       }else{
    #         x %>%
    #           dplyr::group_by(spp_label, crm_option, iter) %>%
    #           dplyr::summarise(collisions = sum(collisions)) %>%
    #           tibble::add_column(
    #             windfarms = windfarms,
    #             time_period = "Annum",
    #             .after = "spp_label")
    #       }
    #     })
    #   
    # })
    
    
 
  })
}
    
## To be copied in the UI
# mod_sim_outputs_ui("sim_outputs_ui_1")
    
## To be copied in the server
# mod_sim_outputs_server("sim_outputs_ui_1")
