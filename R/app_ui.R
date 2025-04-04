#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import fresh
#' @noRd
app_ui <- function(request) {

  # ggplot scrm thete
  ggplot2::theme_set(theme_scrm())
  
  waiter::waiter_set_theme(
    color = waiter::transparent(0.70)
    )

  tagList(
    
    # Your application UI logic
    
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # loading screen -----------------------------------------------------------
    waiter::waiterShowOnLoad(
      tags$img(
        src = "www/favicon_2.svg",
        height = 200,
        id = "app_loader" # set id
      ),
      color = "#D8DEE9"
    ),
    
    # link to awesome fonts v6.0.0
    tags$style("@import url(https://use.fontawesome.com/releases/v6.0.0/css/all.css);"),

    
    shinydashboardPlus::dashboardPage(
      controlbar = NULL,
      #freshTheme = fresh::use_theme("inst/app/www/scrm-custom-theme.css"),

      # Dashboard header -------------------------------------------------------
      shinydashboardPlus::dashboardHeader(
        #fixed = TRUE,
        title = p(
          img(src = "www/hexSticker_scrm.png", height = "35px"),
          "sCRM"),
        controlbarIcon = NULL, 
        
        leftUi = tagList(
          tags$li(
            class = "dropdown",
            a(tags$b(paste0("v", packageVersion("sCRM"))),
              href='https://github.com/dmpstats/sCRM/releases',
              style = "font-size: 16px; padding-left: 5px; padding-right: 5px", 
              target='_blank') %>%
              bsplus::bs_embed_tooltip(
                "App version", 
                "bottom",
                container = "body")
            # actionLink(
            #   inputId =  "appvrsn",
            #   label = tags$b(paste0("v", packageVersion("sCRM"))),  
            #   style = "font-size: 16px; padding-left: 5px; padding-right: 5px")
          ),
          tags$li(
            class = "dropdown", 
            a(icon('github', "fa-2x"),
              href='https://github.com/dmpstats/sCRM',
              style = "padding-top: 10px; padding-bottom: 10px", 
              target='_blank') %>%
              bsplus::bs_embed_tooltip(
                "Code Repository", 
                "bottom",
                container = "body")
          ),
          tags$li(
            class = "dropdown", 
            a(icon('bug', "fa-2x"), 
              href='https://github.com/dmpstats/sCRM/issues',
              style = "padding-top: 10px; padding-bottom: 10px", 
              target='_blank') %>%
              bsplus::bs_embed_tooltip(
                "Submit issues, queries and suggestions. Thanks!", 
                placement = "bottom", 
                container = "body")
            )
        ),
        
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
        

        

        
        # 
        # # tags$li(class = "dropdown", actionLink("bookmark_btt", label = NULL, icon("bookmark", "fa-2x", lib = "font-awesome"),
        # #                                        style = "padding-top: 10px; padding-bottom: 10px")),
        # tags$li(class = "dropdown", actionLink("saveInputs_btt", label = NULL, icon("save", "fa-2x", lib = "font-awesome"),
        #                                        style = "padding-top: 10px; padding-bottom: 10px")),
        # tags$li(class = "dropdown", actionLink("restoreInputs_btt", label = NULL, icon("window-restore", "fa-2x", lib = "font-awesome"),
        #                                        style = "padding-top: 10px; padding-bottom: 10px")),
        
        
      ),

      # Dashboard sidebar ------------------------------------------------------
      shinydashboardPlus::dashboardSidebar(

        sidebarMenu(
          id = "sidebarmenu",
          style = "white-space: normal;", # required to stop long names to spill beyond sidebar
          
          br(),
          
          # Winfarms Section
          menuItem(
            tabName = "sbm-wf",
            text = strong("Step 1: Wind Farm Scenarios"),
            icon = icon("fan")
          ),
          
          br(),
          
          # Species in Windfarms Section
          menuItem(
            tabName = "sbm-spp",
            text = strong("Step 2: Species Features"),
            icon = icon("crow"),#, #icon("swift") #icon("earlybirds"), 
            menuItemOutput("subItems_spps_in_wf"),
            startExpanded = TRUE
          ),

          br(),

          # Simulation Section
          menuItem(
            tabName = "sbm-sim",
            text = strong("Step 3: Simulation and Outputs"),
            icon = icon("laptop-code")
          )
        ),
        
        # Partnership Logos
        div(
          style = "position: absolute; bottom: 20px;",
          #img(src = "www/hexSticker_scrm.png", height = "150px"),
          a(img(src = "www/MS_Logo_stacked.png", height = "45px"), 
            href='https://www.gov.scot/Topics/marine',
            style = "padding-top: 10px; padding-bottom: 10px; padding-left:20px", 
            target='_blank'),
          rep_br(2),
          
          a(img(src = "www/DMP_logo_1.png", height = "40px"), 
            href='https://www.dmpstats.com',
            style = "padding-top: 5px; padding-bottom: 10px; padding-left: 20px", 
            target='_blank'),
          
          a(img(src = "www/HiDef_Logo_2.png", height = "30px"), 
            href='https://www.hidefsurveying.co.uk/',
            style = "padding-top: 5px; padding-bottom: 10px;padding-left: 20px", 
            target='_blank'),
          
          a(img(src = "www/bioConsultSH_Logo_2.png", height = "30px"), 
            href='https://bioconsult-sh.de/en/',
            style = "padding-top: 10px; padding-bottom: 5px; padding-left:20px", 
            target='_blank')
          
        )
      ),

      
      # Dashboard body ------------------------------------------------------
      shinydashboard::dashboardBody(

        fresh::use_theme("inst/app/www/scrm-custom-theme.css"),

        # dashboardthemes::shinyDashboardThemes(
        #   theme = "grey_light"
        # ),

        #tabItems(
        div(
          class="tab-content", 
          id="tabItemsEnvelope",  # required as reference to the dynamic UI tab for each species via insertUI()
          
          # Windfarms Section
          tabItem(
            tabName = "sbm-wf",
            
            # force minimum height of DIV otherwise overlayed waiter is too small
            style = "min-height: 100vh;",
            
            fluidRow(
              shinydashboard::tabBox(
                id = "tbx-wf",
                width = 12,
                height =  "100%",
                
                # title contains buttons to append additional tabPanels for each windfarm
                title = add_wf_btns(
                  dpdn_add_wf_id = "drpdwn-add-wf",
                  sltz_wf_id = "active-wfs",
                  dpdn_close_id = "drpdwn-close",
                  dpdn_upld_wf_id = "btn-upld-wf",
                  dwnl_btn_id = "dt-wf-inputs-tmpl",
                  file_input_id = "flinput-wf-inputs"),
                
                # NOTE: new clash between {rhandsontable} and the latest version
                # of {shiny} meant rendering of the initial WF tab needs to be
                # done here.
                tabPanel(
                  value = init_wf_tp_id,
                  title = tagList(
                    strong(init_wf_label),
                    shinyWidgets::circleButton(
                      inputId = paste0("btn-rmv-wf-", init_wf_id),
                      size = "xs",
                      status = "danger",
                      icon = icon("remove", verify_fa = FALSE), #icon("minus")
                      class = "btn-rmv-tabPanel"
                    )
                  ),
                  
                  # TabPanel content
                  shiny::wellPanel(
                    style = "padding-top: 10px",
                    # module for added wf main tabPanel content - UI side
                    mod_pnl_wf_ui(
                      id = paste0('pnl-wf-', init_wf_id),
                      band_mode = FALSE,
                      is_demo = TRUE,
                      wf_label = init_wf_label
                    )
                  )
                )
              )
            ),
            shinyjs::hidden(
              div(
                id = "no_wf_fdbck",
                class = "centered-textblock",
                p("No windfarm scenario specified. To proceed, please provide at least one scenario.",
                  style = "color: #dd4b39; font-size: 16px;"
                )
              )
            )
          ),
          
          # NOTE: new clash between {rhandsontable} and the latest version of
          # {shiny} meant rendering of the species menu sub-item for the initial
          # WF tab needs to be done here UI-side.
          tabItem(
            tabName = paste0("sbsm-sppinwf-", init_wf_id),
            mod_spp_in_wf_ui(
              id = paste0("pnl-sppinwf-", init_wf_id),
              wf_label = init_wf_label
            )
          ),
          
          # Simulation and Results Section
          tabItem(
            tabName = "sbm-sim",
            mod_pnl_sim_ui(id = "pnl-sim")
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
  
  add_resource_path('www', app_sys('app/www'))
  
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
    
    # Add here other external resource
    shinyjs::useShinyjs(),
    #shinyjs::extendShinyjs(script = "shinyjs_funcs.js", functions = c("backgroundCol")),
    shinyjs::extendShinyjs(text = jsCode, functions = c("backgroundCol", "fontCol")),
    bsplus::use_bs_tooltip(),
    #bsplus::use_bs_popover(),
    
    waiter::useAttendant(),
    waiter::useWaiter(),
    #waiter::useHostess(),
    waiter::useGarcon(),
    
    sever::useSever(), 
    
    #cicerone::use_cicerone(),
    
    shinyFeedback::useShinyFeedback()
    
  )
}


