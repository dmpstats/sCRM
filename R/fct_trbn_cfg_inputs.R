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
trbn_cfg_inputs <- function(ns, inputs_width = "100%", is_demo = FALSE){
  
  tagList(
  
    # ---- Number of blades
    numericInput(
      width = inputs_width,
      inputId = ns("nblades"),
      label = "No. of Blades",
      value = if(is_demo) 3 else NULL,
      min = 1, 
      step = 1),
    
    
    # ---- Rotor radius
    numericInput(
      width = inputs_width,
      inputId = ns("rtradius"), 
      label =  "Rotor Radius (m)",
      value = if(is_demo) 50 else NULL, 
      min = 1, 
      step = 0.1),
    
    
    # ---- Air Gap
    numericInput(
      width = inputs_width, 
      inputId = ns("airgap"),
      label =  "Air Gap (m)",
      value = if(is_demo) 25.5 else NULL,
      min = 1,
      step = 0.1),
    
    
    # --- Maximum blade width
    numericInput(
      width = inputs_width, 
      inputId = ns("bladewth"), # "numInput_turbinePars_maxBladeWdth",
      label =  "Max Blade Width (m)",
      value = if(is_demo) 5.5 else NULL,
      min = 1,
      step = 0.1)
    
  )
}