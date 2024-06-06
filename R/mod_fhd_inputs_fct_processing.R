#' Read FHD bootstraps from user input file
#' 
#' Read, validate and preprocess FHD bootstrap data uploaded by user
#'
#' @param name a character string, the filename, including it's extension
#' @param path a character string, the path to the file containing the data
#'
#' @import shiny
#
#' @noRd
load_fhd_boot <- function(name, path){
  
  # shinyFeedback::hideFeedback(input_id)
  
  ext <- tools::file_ext(name)
  
  if(ext != "csv"){
    
    list(
      error = TRUE,
      msg = "Invalid file extension - please upload a csv file"
    )
    
    # shinyFeedback::showFeedbackDanger(
    #   inputId = input_id, 
    #   text = "Invalid file extension - please upload a csv file"
    # )
    
  }else{
    
    fhd_boot <- vroom::vroom(file = path, delim = ",", col_names = TRUE) 
    
    det_height_col <- any(
      stringr::str_detect(
        names(fhd_boot), 
        pattern = "(H|h)eight"
      ))
    
    if(!det_height_col){
      
      list(
        error = TRUE,
        msg = "Invalid data: 'height' column not found"
      )
      
      # shinyFeedback::showFeedbackDanger(
      #   inputId = input_id, 
      #   text = "Invalid data: 'height' column not found"
      # )
      
    }else{
      
      fhd_boot <- fhd_boot %>%
        dplyr::rename(height =  matches("(H|h)eight")) %>%
        dplyr::select_if(colSums(., na.rm = TRUE) > 0)
      
      if(ncol(fhd_boot) <= 1){
        
        list(
          error = TRUE,
          msg = "Invalid data: universally zero bootstraps values"
        )
        
        # shinyFeedback::showFeedbackDanger(
        #   inputId = input_id, 
        #   text = paste0(
        #     "Invalid data: universally zero bootstraps values")
        # )
        
      }else if(fhd_boot$height[1] != 0){
        
        list(
          error = TRUE,
          msg = "Column `height` must start from 0, denoting band '0-1m'"
        )
        
      }else{
        
        list(
          error = FALSE,
          dt = fhd_boot
        )
        
      }
    }
  }
}


#' Read FHD estimates from user input file
#' 
#' Read, validate and preprocess FHD data uploaded by user
#'
#' @param name a character string, the filename, including it's extension
#' @param path a character string, the path to the file containing the data
#'
#' @import shiny
#
#' @noRd
load_fhd_est <- function(name, path){
  
  # # hide any old feedback message, if existent
  # shinyFeedback::hideFeedback(input_id)
  # 
  colname_patterns <- c("(H|h)eight", "(P|p)rop")
  col_names <- c("height", "prop")
  
  ext <- tools::file_ext(name)
  
  if(ext != "csv"){
    
    list(
      error = TRUE,
      msg = "Invalid file extension - please upload a csv file"
    )
    
    # shinyFeedback::showFeedbackDanger(
    #   inputId = input_id, 
    #   text = "Invalid file extension - please upload a csv file"
    # )
    
  }else{
    
    fhd <- vroom::vroom(file = path, delim = ",", col_names = TRUE) 

    cols_detected <- sapply(colname_patterns, function(x){
      any(
        stringr::str_detect(
          names(fhd), 
          pattern = x
        ))
    })
    
    if(!all(cols_detected)){
      
      missing_col <- col_names[which(!cols_detected)[1]]
      
      list(
        error = TRUE,
        msg = glue::glue("Invalid data: column '{missing_col}' not found")
      )
      
      # shinyFeedback::showFeedbackDanger(
      #   inputId = input_id, 
      #   text = paste0("Invalid data: column '", missing_col, "' not found")
      # )
      
    }else{
      
      fhd <- fhd %>%
        dplyr::rename(
          height =  matches("(H|h)eight"),
          prop = matches("(P|p)rop")
        )
      
      if(fhd$height[1] != 0){
        
        list(
          error = TRUE,
          msg = "Column `height` must start from 0, denoting band '0-1m'"
        )
        
      }else{
        
        # # provide warning about relative distributions not summing up to 1
        # # Not making this a strict requirement for now
        # shinyFeedback::feedbackWarning(
        #   inputId = input_id, 
        #   show = abs(1 - sum(fhd$prop)) > 0.01, 
        #   text = "Proportions at flight heights do not sum to up 1"
        # )
        
        list(
          error = FALSE,
          dt = fhd
        )
      }
    }
  }
}






file_tooltip_text <- function(band_mode){
  if(!band_mode){
    c("Dataset with first column comprising 1m height bands ",
      "and remaining columns with bootstrap replicates of the ",
      "species' FHD. Please download & use the adjacent template.")
  }else{
    c("File with the species' FHD, comprising columns 'height' ",
      "(1m bands) and 'prop' (proportion at flight height). ",
      "Please download & use the adjacent template file")
  }
}



dwnl_tooltip_text <- function(band_mode){
  if(!band_mode){
    c("Template for the FHD bootstrap dataset. Fill it in, save ",
      "locally and upload it on the adjacent field")
  }else{
    c("FHD template dataset. Fill it in, save ",
      "locally and upload it on the adjacent field")
  }
}
