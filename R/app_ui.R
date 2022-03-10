#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import fresh
#' @noRd
app_ui <- function(request) {
  
  theme_set(theme_bw())
  
  tagList(
    
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
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
              handleWidth = "80px",
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
          menuItem(
            tabName = "sbm-wf",
            text = "Step 1: Wind Farm scenarios",
            icon = icon("fan")
          ),
          menuItem(
            tabName = "sbm-spp",
            text = "Step 2: Species features",
            icon = icon("crow") #icon("swift") #icon("earlybirds")
          ),
          menuItem(
            tabName = "sbm-sim",
            text = "Step 3: Simulation & Results",
            icon = icon("laptop-code")
          )
        )
        
        # # Perhaps include the following at tge bottom of the sidebar?
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
        
        # dashboardthemes::shinyDashboardThemes(
        #   theme = "grey_light"
        # ),
        
        # br(),
        # br(),
        # br(),

        tabItems(
    
          # Windfarm Scenarios
          tabItem(
            tabName = "sbm-wf",
            #br(),
            shinydashboard::tabBox(
              id = "tbx-wf",
              width = 12,
              # title contains buttons to append additional tabPanels
              title = add_scn_btns(
                btn_add_id = "btn-add-wf", 
                drpdwn_btn_id = "btn-upld-wf-scenarios",
                dwnl_btn_id = "dt-wf-inputs-tmpl",
                file_input_id = "flinput-wf-inputs"),
              
              # TabPanel default scenario
              tabPanel(
                title = strong("Scenario 1"),
                value = "tbp-wf-1",
                wellPanel(
                  h4(strong("Wind farm scenario 1")),
                  fluidRow(mod_pnl_wf_ui("pnl-wf-1"))
                )
              )
            )
          ),
          
          # Species Features
          tabItem(
            tabName = "sbm-spp",
            box(
              title = "Species Stuff"
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
  
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'sCRM'
    ),
    
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    shinyjs::useShinyjs(),
    bsplus::use_bs_tooltip(),
    bsplus::use_bs_popover()
    
  )
}


