#' prob_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @import shiny
mod_prob_inputs_ui <- function(id, pars_lkp, band_mode){
  
  ns <- NS(id)
  
  #browser()
  
  tagList(
    
    tags$table(
      style = "width: 100%",
      #id = "tbl-prob-dist-inputs",
      tags$tr(
        tags$th(
          style = "width: 35%; padding-bottom: 5px; text-align: left;",
          "",
          ),
        tags$th(
          style = "width: 30%; padding-bottom: 5px;",
          "Mean"),
        tags$th(
          style = "width: 30%; padding-bottom: 5px;",
          "SD"),
        tags$th(
          style = "width: 5%; padding-bottom: 5px; text-align: left;",
          ""
          )
      ),
      # insert rows with parameters for each input, and distribution display
      tagList(
        purrr::pmap(pars_lkp, function(par_id, par_label, par_dist, dflt_mean, dflt_sd, ...){
          mod_prob_inputs_row_ui(
            id = ns(par_id), 
            par_label = par_label,
            par_dist = par_dist,
            dflt_mean = dflt_mean, 
            dflt_sd = dflt_sd,
            band_mode = band_mode
          )
        })
      )
    )
  )
}


#' prob_inputs Server Functions
#'
#' @noRd 
mod_prob_inputs_server <- function(id, pars_lkp, band_mode, 
                                   plot_fill = "olivedrab"){
  
  stopifnot(is.reactive(band_mode))
  stopifnot(!is.reactive(pars_lkp))
  stopifnot(!is.reactive(plot_fill))
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    iv <- InputValidator$new()
    
    # Generate module server for each row (i.e. parameter) AND add child
    # validator to module's InputValidator object
    purrr::pwalk(pars_lkp, function(par_id, par_label, par_dist, par_name, ...){
      
      prob_input_row_result <- mod_prob_inputs_row_server(
        id = par_id,
        par_label = par_label,
        par_dist = par_dist,
        par_name = par_name, 
        band_mode = band_mode, 
        plot_fill = plot_fill)
      
      iv$add_validator(prob_input_row_result$iv)
    })
    

    # return InputValidator object
    list(
      iv = iv
    )
    
  })
}
    
## To be copied in the UI
# mod_prob_inputs_ui("prob_inputs_ui_1")
    
## To be copied in the server
# mod_prob_inputs_server("prob_inputs_ui_1")
