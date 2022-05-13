#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import fresh
#' @noRd
app_ui <- function(request) {


  ggplot2::theme_set(theme_scrm())
  
  tagList(
    
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    tags$style("@import url(https://use.fontawesome.com/releases/v6.0.0/css/all.css);"),
    
    # Your application UI logic 
    shinydashboardPlus::dashboardPage(
      controlbar = NULL, 
      #freshTheme = fresh::use_theme("inst/app/www/scrm-custom-theme.css"),
      
      # Dashboard header -------------------------------------------------------
      shinydashboardPlus::dashboardHeader(
        #fixed = TRUE,
        title = p(
          img(src = "www/hexSticker_scrm.png", height = "35px"),
          "sCRM"),
        #fixed = TRUE, 
        controlbarIcon = NULL,

        tags$li(
          class = "dropdown",
          div(
            shinyWidgets::switchInput(
              inputId = "swtc-band-mode",
              size = "small",
              inline = TRUE,
              #width = "250px", 
              labelWidth = "50px", 
              handleWidth = "110px",
              label = strong(" Mode"),
              onStatus = "warning", 
              offStatus = "info", 
              onLabel = strong("Deterministic"),
              offLabel = strong("Stochastic")
              ),
            style="float: right;"# padding-top: 6px; margin-bottom: 0px" # ;color: #ededed"
          )
        )
      
        # tags$li(
        #   class = "dropdown",
        #   actionLink(
        #     inputId =  "appvrsn",
        #     label = tags$b(paste0("v", golem::get_golem_version())),
        #     style = "font-size: 19px"),
        # )
        # tags$li(class = "dropdown", a(icon('github', "fa-2x"), href='https://github.com/dmpstats/stochCRM', 
        #                               style = "padding-top: 10px; padding-bottom: 10px", target='_blank', id="lbl_codeLink")),
        # tags$li(class = "dropdown", a(icon('bug', "fa-2x"), href='https://github.com/dmpstats/stochCRM/issues', #exclamation-circle
        #                               style = "padding-top: 10px; padding-bottom: 10px", target='_blank', id="lbl_issuesLink")),
        # 
        # # tags$li(class = "dropdown", actionLink("bookmark_btt", label = NULL, icon("bookmark", "fa-2x", lib = "font-awesome"),
        # #                                        style = "padding-top: 10px; padding-bottom: 10px")),
        # tags$li(class = "dropdown", actionLink("saveInputs_btt", label = NULL, icon("save", "fa-2x", lib = "font-awesome"),
        #                                        style = "padding-top: 10px; padding-bottom: 10px")),
        # tags$li(class = "dropdown", actionLink("restoreInputs_btt", label = NULL, icon("window-restore", "fa-2x", lib = "font-awesome"),
        #                                        style = "padding-top: 10px; padding-bottom: 10px")),
        # 
        # 
        # tags$li(class = "dropdown", a(img(src = "bioConsultSH_Logo_2.png", height = "40px"), href='https://bioconsult-sh.de/en/',
        #                               style = "padding-top: 5px; padding-bottom: 5px;", target='_blank', id="lbl_bioConsultLogoLink"),
        #         style="float: right"),
        # tags$li(class = "dropdown", a(img(src = "HiDef Logo_2.png", height = "40px"), href='https://hidef.bioconsult-sh.de/',
        #                               style = "padding-top: 5px; padding-bottom: 5px;", target='_blank', id="lbl_hiDefLogoLink"),
        #         style="float: right"),
        # tags$li(class = "dropdown", a(img(src = "DMP_logo_1.png", height = "40px"), href='https://www.dmpstats.com',
        #                               style = "padding-top: 5px; padding-bottom: 5px", target='_blank', id="lbl_dmpLogoLink"), 
        #         style="float: right"),
        # tags$li(class = "dropdown", a(img(src = "MS Logo Linear-01_2.png", height = "30px"), href='https://www.gov.scot/Topics/marine',
        #                               style = "padding-top: 10px; padding-bottom: 10px;", target='_blank', id="lbl_marineScotlandLink"),
        #         style="float: right")
      ),
      
      # Dashboard sidebar ------------------------------------------------------
      shinydashboardPlus::dashboardSidebar(
        
        sidebarMenu(
          id = "sidebarmenu",
          style = "white-space: normal;", # required to stop long names to spill beyond sidebar
          
          # Winfarms Section
          menuItem(
            tabName = "sbm-wf",
            text = strong("Step 1: Windfarm Scenarios"),
            icon = icon("fan")
          ),
          
          # Species in Windfarms Section
          menuItem(
            tabName = "sbm-spp",
            text = strong("Step 2: Species at Windfarm"),
            icon = icon("crow"), #, #icon("swift") #icon("earlybirds"),
            menuItemOutput("subItems_spps_in_wf")
          ),
          
          # Simulation Section
          menuItem(
            tabName = "sbm-sim",
            text = strong("Step 3: Simulation & Results"),
            icon = icon("laptop-code")
          )
          
        )
        
        # # Perhaps include the following at the bottom of the sidebar?
        # fluidRow(
        #   column(
        #     align='center',
        #     width= 8,
        #     #h2("Avian Migration Collision risk"),
        #     img(src = "www/hexSticker_scrm.png", height = "150px")
        #   )
        # )
      ),
      
      # Dashboard body ------------------------------------------------------
      shinydashboard::dashboardBody(
        
        fresh::use_theme("inst/app/www/scrm-custom-theme.css"),

        # use_theme(
        #   create_theme(
        #     theme = "default",
        #     bs_vars_progress(
        #       border_radius = "15px",
        #     ),
        #     output_file = NULL
        #   )
        # ),
        
        
        # dashboardthemes::shinyDashboardThemes(
        #   theme = "grey_light"
        # ),
        

        #tabItems(
        div(class="tab-content", id="tabItemsEnvelope",  # required as reference to the dynamic UI tab for each species via insertUI()
    
          # Windfarms Section
          tabItem(
            tabName = "sbm-wf",
            
            fluidRow(
              shinydashboard::tabBox(
                id = "tbx-wf",
                width = 12,
                height =  "100%",
                
                # title contains buttons to append additional tabPanels for each windfarm
                title = add_wf_btns(
                  dpdn_add_wf_id ="drpdwn-add-wf", 
                  sltz_wf_id = "active-wfs",
                  dpdn_close_id = "drpdwn-close",
                  dpdn_upld_wf_id = "btn-upld-wf",
                  dwnl_btn_id = "dt-wf-inputs-tmpl",
                  file_input_id = "flinput-wf-inputs")
              )
            )
          ),
          
          # Simulation and Results
          tabItem(
            tabName = "sbm-sim",
            tabsetPanel(type = "tabs",
                        tabPanel("Plot"),
                        tabPanel("Summary"),
                        tabPanel("Table")
            ),
            box(
              title = "Simulation Stuff"
            )
          )
        )
      )
    )
  )
}


#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  jsCode <- '
    shinyjs.backgroundCol = function(params) {
      var defaultParams = {
        id : null,
        col : "red"
      };
      params = shinyjs.getParams(params, defaultParams);

      var el = $("#" + params.id);
      el.css("background-color", params.col);
    }
    
    shinyjs.fontCol = function(params) {
      var defaultParams = {
        id : null,
        col : "red"
      };
      params = shinyjs.getParams(params, defaultParams);

      var el = $("#" + params.id);
      el.css("color", params.col);
    }
  
  '
  
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'sCRM'
    ),
    
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    shinyjs::useShinyjs(),
    #shinyjs::extendShinyjs(script = "shinyjs_funcs.js", functions = c("backgroundCol")),
    shinyjs::extendShinyjs(text = jsCode, functions = c("backgroundCol", "fontCol")),
    bsplus::use_bs_tooltip(),
    bsplus::use_bs_popover(),
    
    shinyFeedback::useShinyFeedback()
    
  )
}


