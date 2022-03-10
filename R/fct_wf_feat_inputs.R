#' Insert winfarm features inputs
#'
#' @description 
#' Insert a `shinydashboardPlus::box` in the a windfarm scenario panel with the
#' following input parameters:
#'  - Nr of turbines
#'  - Windfarm latitude
#'  - Air Gap
#'  - Blade width
#' 
#' @param inputs_width character string, the width of the numeric input, e.g. '400px',
#'   or '100%'.
#'
#' @return No returned value.
#' 

wf_feat_inputs <- function(ns = ns, inputs_width = "100%"){
  
  tagList(
    
    #splitLayout(
    # --- Number of Turbines
    numericInput(
      width = inputs_width,
      inputId = ns("nturb"),
      label = "Number of turbines",
      value = NULL, # startUpValues$windFarmPars$windfarmPars_nTurbines,
      min = 1,
      step = 1),

    # ---  Latitude
    numericInput(
      width = inputs_width,
      inputId = ns("lat"),
      label = "Latitude (deg)", #label.help("Latitude (deg)", "lbl_windfarmLatitude"),
      value = NULL, #startUpValues$windFarmPars$windfarmPars_Latitude,
      min = -90,
      max = 90,
      step = 0.01),

    # --- Width
    numericInput(
      width = inputs_width,
      inputId = ns("wfwidth"),
      label = "Width (Km)", #label.help("Width (Km)", "lbl_windfarmWidth"),
      value = NULL, #startUpValues$windFarmPars$windfarmPars_width,
      min = 1,
      step = 1),

    # --- Tidal offset
    numericInput(
      width = inputs_width,
      inputId = ns("tdloffset"),
      label = "Tidal offset (m)", #label.help("Tidal Offset (m)", "lbl_tidalOffset"),
      value = NULL, #startUpValues$windFarmPars$tidalOffset,
      min = 0, step = 0.1)
    #)
  )
}