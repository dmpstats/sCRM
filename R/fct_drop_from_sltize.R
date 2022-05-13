#' drop_from_sltize 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


drop_from_sltz <- function(session = session, 
                           rmv_id, 
                           c_slctd_labs, 
                           inputId,
                           drop_from_choices = FALSE){
  
  # index of removed item id in list of currently selected items
  rmv_idx <- which(rmv_id == label2id(c_slctd_labs))
  
  # variable labels to keep as selected
  upd_slct <- c_slctd_labs[-rmv_idx]
  
  
  # update selected variable labels
  if(drop_from_choices){
    
    updateSelectizeInput(
      inputId = inputId, 
      choices = upd_slct,
      selected = upd_slct)
    
  }else{
    
    updateSelectizeInput(
      inputId = inputId, 
      selected = upd_slct)
  }
  
  # return updated list of selected items
  upd_slct
}
