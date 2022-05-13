#' Remove parent `tabPanel` of a clicked button (server function)
#'
#' @description Removes tabpanel of clicked "-" button and selects latest
#'   tabPanel as active. #'
#'
#' @param btn_tags character vector, each element a tag composing the button id.
#'   IDs expected to follow the convention `btn-'action'-'section'-'section id'`.
#'   E.g. the button for removing the 3rd wind farm scenario expected to be
#'   `btn-rmv-wf-3`
#'
#' @param valid_inputs_ids character vector with input ids of active buttons
#'
#' @return No returned value
#'   
remove_tbp <- function(btn_tags, valid_inputs_ids){
  
  action <- btn_tags[2]
  section <- btn_tags[3]
  section_id <- btn_tags[4]
  
  btn_id <- paste(btn_tags, collapse = "-")
  
  # remove tabPanel
  removeTab(
    inputId = paste("tbx", section, sep = "-"), 
    target = paste("tbp", section, section_id, sep = "-")
  )
  
  # Taking button names as proxy for tabs...
  
  # pattern of button names to look
  btn_pattern <- paste("btn", action, section, sep = "-")
  
  # available valid buttons
  avlb_btn_ids <- stringr::str_subset(valid_inputs_ids, btn_pattern)
  avlb_btn_ids <- avlb_btn_ids[avlb_btn_ids != btn_id]
  avlb_btn_ids <- avlb_btn_ids[!is.null(avlb_btn_ids)]
  
  # youngest tabPanel available
  section_ids <- as.numeric(stringr::str_extract(avlb_btn_ids, "\\d+"))
  latest_section <- sort(section_ids, decreasing = TRUE)[1]
  yng_tab_id <- paste("tbp", section, latest_section, sep = "-")

  # select the youngest tabPanel
  updateTabItems(
    inputId = paste("tbx", section, sep = "-"),
    selected = yng_tab_id
  )
  
  # assign NULL for value of button removing the panel
  shinyjs::runjs(paste0("Shiny.onInputChange('", btn_id,"',null)"))

}
