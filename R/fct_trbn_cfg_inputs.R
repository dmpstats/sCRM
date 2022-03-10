#' Insert inputs for turbine configuration parameters (UI function)
#'
#' @description 
#' Insert `shinydashboard::box` with the following turbine input parameters in
#' windfarm scenario panel:   
#'  - Nr of blades
#'  - Rotor radius
#'  - Air Gap
#'  - Blade width
#' 
#' @param inputs_width character string, the width of the numeric input, e.g. '400px',
#'   or '100%'
#'
#' @return No returned value.
#'
trbn_cfg_inputs <- function(ns, inputs_width = "100%"){

  tagList(
  
    # ---- Number of blades
    numericInput(
      width = inputs_width,
      inputId = ns("nblades"),
      label = "No. of Blades",
      value = NULL, #startUpValues$turbinePars$numBlades, 
      min = 1),
    
    
    
    # ---- Rotor radius
    numericInput(
      width = inputs_width,
      inputId = ns("rtradius"), 
      label =  "Rotor Radius (m)",
      value = NULL, #startUpValues$turbinePars$rotorRadius, 
      min = 1, step = 0.1),
    
    
    
    # ---- Air Gap
    numericInput(
      width = inputs_width, 
      inputId = ns("airgap"),
      label =  "Air Gap (m)",
      value = NULL, #startUpValues$turbinePars$airGap,
      min = 1, step = 0.5),
    
    
    
    # Maximum blade width
    numericInput(
      width = inputs_width, 
      inputId = ns("bladewth"), # "numInput_turbinePars_maxBladeWdth",
      label =  "Max Blade Width (m)",
      value = NULL, #startUpValues$turbinePars$maxBladeWdth, 
      min = 1, step = 0.1)
  )
}