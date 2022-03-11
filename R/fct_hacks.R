#' ------------------------------------------------------------ #
#' -----        Hacked third party functions             ------
#' ------------------------------------------------------------ #


#' Hacked `msm::qtnorm` to return NAs instead of error if mean or sd is NA
#' 
#' @import msm
#' @noRd
qtnorm_hck <- function(p, mean=0, sd=1, lower=-Inf, upper=Inf){
  
  if(is.na(mean)|is.na(sd)){
    out <- rep(NA, length(p))
    warning("NA values for mu and/or stdev - NAs produced")
  }else{
    if(sd >= 0){
      if(sd == 0 & mean == lower){
        out <- rep(lower, length(p))
      }
      if(sd == 0 & mean < lower){
        out <- rep(NA, length(p))
        warning("mu < lower & SD = 0 - NAs produced")
      }
      if(sd == 0 & mean > lower){
        out <- rep(mean, length(p))
      }
      if(sd > 0){
        out <- qtnorm(p, mean = mean, sd = sd, lower = lower, upper = upper)
      }
    }else{
      warning("SD < 0 - NAs produced")
      out <- rep(NA, length(p))
    }
  }
  return(out)
}
