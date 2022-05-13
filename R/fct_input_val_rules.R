#' ------------------------------------------------------------------------ #
#' ----   Functions with input validation rules for {shinyvalidate}    ------
#' ------------------------------------------------------------------------ #


#' Validation logic for SD in Beta distributions 
#'
#' @description
#' 
#' Valid SDs in Beta distributions depend on the associated mean value. This
#' function checks validity of chosen SD given the Mean. Failure will return a
#' string which will subsequently be passed to the UI via {shinyvalidate}
#'
#' @return Either NULL, if the the value is acceptable, or a character string
#'   describing why the value is problematic
#' 
#' @noRd
valid_beta_sd <- function(sd, mean){
  
  betaMeanVarCond <- sd^2 < mean*(1-mean)
  
  if(!is.na(sd) & !is.na(mean)){
    if(mean >= 0 & mean <= 1){
      if(mean == 0 | mean == 1){
        if(sd != 0){
          glue::glue("SD must be 0 when Mean = {mean}")  
        }
      }else if(!betaMeanVarCond){
        condLimit <- round(sqrt(mean*(1-mean)), digits = 3)
        mean_rounded <- round(mean, digits = 3)
        glue::glue("SD must be < {condLimit} when Mean = {mean_rounded}")
      }
    }
  }
}



#' Check if data contains incomplete columns
#' 
#' @description 
#' Check if data frame contains any column with a mix of NAs and
#' non-NA values
#' 
#' Created for application to tables comprising interdependent features in each
#' column, such as parameters mean and SD regulating the truncated normal
#' distribution used to sample monthly bird densities.
#'
#' @param dt a data frame
#' 
#' @return Boolean, indicating whether data contains uncompleted column(s)
#' 
#'@noRd
incomplete_cols <- function(dt){
  
  # local ftc to check if vector elements are a mix of NAs and non-NAs
  some_nas <- function(x){
    empty_col <- all(is.na(x))
    completed_col <- all(!is.na(x))
    if(empty_col | completed_col){
      FALSE
    }else{
      TRUE
    }
  }
  
  # Does any column contains a mix of NAs and non-NAs?
  any(apply(dt, 2, some_nas))
}




#' Check for non-numeric columns in data frame (with NAs ignored)
#' 
#' @param dt a data frame
#' 
#' @noRd 
non_numeric_cols <- function(dt){
  
  not_numeric <- function(x){
    if(!is.numeric(x)){
      TRUE
    }else if(is.na(x)){
      FALSE
    }else{
      FALSE
    }
  }
  
  any(apply(dt, c(1, 2), not_numeric))
}




#' Check if data frame contains any empty column 
#'  
#' @param dt a data frame
#' 
#' @noRd 
empty_cols <- function(dt){
  
  any(apply(dt, 2, function(x){all(is.na(x))}))
  
}



#' Check if season periods are not covered by available monthly data
#' 
#' @param seas_df a data frame with seasons definitions
#' @param avlb_mths a character vector with months for which the is available data.
#' 
#' @noRd 
seas_uncovered <- function(seas_df, avlb_mths){
  
  avlb_mths <- substr(avlb_mths, 1, 3)
  seas_df[[2]] <- substr(seas_df[[2]], 1, 3)
  seas_df[[3]] <- substr(seas_df[[3]], 1, 3)
  
  if(length(avlb_mths) == 0){
    TRUE
  }else{
    seasons_months <- purrr::pmap(seas_df, ~stochLAB::seq_months(..2, ..3))
    any(purrr::map_lgl(seasons_months, ~ any(. %not_in% avlb_mths)))
  }

}



