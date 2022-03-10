#' prob_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
mod_prob_inputs_ui <- function(id, pars_lkp){
  ns <- NS(id)
  
  tagList(
    
    tags$table(
      style = "width: 100%",
      #id = "tbl-prob-dist-inputs",
      tags$tr(
        tags$th(
          style = "width: 40%; padding-bottom: 5px; text-align: left;",
          "",
          ),
        tags$th(
          style = "width: 28%; padding-bottom: 5px;",
          "Mean"),
        tags$th(
          style = "width: 28%; padding-bottom: 5px;",
          "SD"),
        tags$th(
          style = "width: 5%; padding-bottom: 5px; text-align: left;",
          ""
          )
      ),
      # insert rows with parameters for each input, and distribution display
      tagList(
        purrr::pmap(pars_lkp, function(par_id, par_label, dflt_mean, dflt_sd, ...){
          mod_prob_inputs_row_ui(
            id = ns(par_id), 
            par_label = par_label, 
            dflt_mean = dflt_mean, 
            dflt_sd = dflt_sd
          )
        })
      )
    )
  )
}
    

#' prob_inputs Server Functions
#'
#' @noRd 
mod_prob_inputs_server <- function(id, pars_lkp, band_mode){
  
  stopifnot(is.reactive(band_mode))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    purrr::pwalk(pars_lkp, function(par_id, par_label, par_dist, par_name, ...){
      mod_prob_inputs_row_server(
        id = par_id,
        par_label = par_label,
        par_dist = par_dist,
        par_name = par_name, 
        band_mode = band_mode)
    })
    
  })
}
    
## To be copied in the UI
# mod_prob_inputs_ui("prob_inputs_ui_1")
    
## To be copied in the server
# mod_prob_inputs_server("prob_inputs_ui_1")
