#' Process monthly density data uploaded by user
#' 
#' Read, validate and preprocess monthly bird density provided by the user. Data
#' consists of either percentile estimates or random draws. For the later case, set
#' `pctl_dt = TRUE`
#'
#' @param name a character string, the filename, including it's extension
#' @param path a character string, the path to the file containing the data
#' @param pctl_dt logical, whether data consists of percentiles estimates
#'
#' @import shiny
#
#' @noRd
load_dens <- function(name, path, pctl_dt = TRUE){
  
  ext <- tools::file_ext(name)
  
  if(ext != "csv"){
    
    list(
      error = TRUE,
      msg = "Invalid file extension: pease upload a csv file"
    )
    
  }else{
    
    # Load up data
    dt <- vroom::vroom(file = path, delim = ",", col_names = TRUE)
    
    # Process column names 
    dt_names <- names(dt) %>% 
      stringr::str_to_title() %>%
      stringr::str_sub(start = 1, end = 3)
    
    # check if column names are unique
    if(any(duplicated(dt_names))){
      
      list(
        error = TRUE,
        msg = "Invalid data: column names must be unique"
      )
      
    }else{
      
      names(dt) <- dt_names
      
      # check presence of month columns
      det_months_cols <- any(
        stringr::str_detect(
          names(dt),
          pattern = paste(month.abb, collapse = "|")
        ))
      
      if(!det_months_cols){
        
        list(
          error = TRUE,
          msg = "Invalid data: no month-named columns found"
        )
        
      }else{
        
        # check for non-numeric columns
        non_numeric_cols <- dplyr::select(dt, !where(is.numeric))
        
        if(ncol(non_numeric_cols) > 0){
          
          non_num_colnames <- fPaste(names(non_numeric_cols))
          
          list(
            error = TRUE,
            msg = stringr::str_glue("Invalid non-numeric column(s): {non_num_colnames}")
          )
          
        }else{
          
          if(pctl_dt){
            
            # check presence of percentile column
            det_pctls_col <- any(
              stringr::str_detect(
                names(dt),
                pattern = "Per"
              ))
            
            if(!det_pctls_col){
              
              list(
                error = TRUE,
                msg = "Invalid data: 'Percentile' column must be provided"
              )
              
            }else{
              
              # pre-process percentile data
              dt <- dt %>%
                dplyr::rename(pctl = Per) %>%
                dplyr::select(pctl, matches(month.abb)) %>%
                dplyr::rename_with(.fn = ~month.name[match(., month.abb)], .cols = -pctl) %>%
                dplyr::arrange(pctl) %>%
                tidyr::drop_na()
              
              list(error = FALSE, dt = dt)         
            }
            
          }else{
            
            # pre-process draws data
            dt <- dt %>%
              dplyr::select(matches(month.abb)) %>%
              dplyr::rename_with(.fn = ~month.name[match(., month.abb)])
            
            list(error = FALSE, dt = dt)
          }
        }
      }
    }
  }
}

