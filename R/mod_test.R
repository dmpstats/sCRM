#' test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_test_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    fluidRow(
      col_6(
        mod_prob_inputs_ui(
          id = ns("pars"), 
          pars_lkp = wf_pars_lookup %>%
            filter(par_name %in% c("bld_pitch", "rtn_speed"))
        )
      )
    )
  )
}


#' test Server Functions
#'
#' @noRd 
mod_test_server <- function(id, determ){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_prob_inputs_server(
      id = "pars", 
      pars_lkp = wf_pars_lookup %>%
        filter(par_name %in% c("bld_pitch", "rtn_speed")),
      band_mode = determ
    )
  })
}
  

## To be copied in the UI
# mod_test_ui("test_ui_1")
    
## To be copied in the server
# mod_test_server("test_ui_1")
