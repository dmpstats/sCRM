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
#' @noRd

wf_feat_inputs <- function(ns = ns, inputs_width = "100%", is_demo = FALSE){

  tagList(
    
    # --- Number of Turbines
    numericInput(
      width = inputs_width,
      inputId = ns("nturb"),
      label = "Number of Turbines",
      value = if(is_demo) 100 else NULL,
      min = 1,
      step = 1),

    # ---  Latitude
    numericInput(
      width = inputs_width,
      inputId = ns("lat"),
      label = "Latitude (deg)", #label.help("Latitude (deg)", "lbl_windfarmLatitude"),
      value = if(is_demo) 56.3 else NULL, 
      min = -90,
      max = 90,
      step = 0.01),

    # --- Width
    numericInput(
      width = inputs_width,
      inputId = ns("wfwidth"),
      label = "Width (Km)", #label.help("Width (Km)", "lbl_windfarmWidth"),
      value = if(is_demo) 12 else NULL, #startup_vals$wf_width,
      min = 1,
      step = 1),

    # --- Tidal offset
    numericInput(
      width = inputs_width,
      inputId = ns("tdloffset"),
      label = "Tidal Offset (m)", #label.help("Tidal Offset (m)", "lbl_tidalOffset"),
      value = if(is_demo) 2.1 else NULL, #startup_vals$tidal_offset,
      min = 0, 
      step = 0.1),
    
  
    # --- Large array correction
    strong("Large Array Correction"), 
    tagAppendAttributes(
      shinyWidgets::switchInput(
        inputId = ns("lac"),
        inline = FALSE, 
        value = TRUE,
        size = "small",
        #onStatus = "success"
      ),
      style = "padding-top: 10px"
    )
  )
}