#' ------------------------------------------------------------ #
#' -----     Functions for outputing quantile summaries    ------
#' ------------------------------------------------------------ #

#' Function for Truncated Normal density quantiles of model input parameters
#' 
#' @import shiny
#' @import dplyr
#' @importFrom tibble column_to_rownames remove_rownames
#' 
#' @noRd
tnorm_qtl_tbl <- function(mean, sd, lower = -Inf, upper = Inf, varTag, decPlaces = 2){
  
  req(mean, sd)
  
  if(sd == 0 & mean <= lower){
    #NULL
    invisible()
  }else{
    if(sd >= 0){
      
      data.frame(pctile = paste0(varTag, "|"), 
                 t(round(qtnorm(p = c(0.025, 0.25, 0.5, 0.75, 0.975), 
                                mean, sd, lower, upper), 
                         decPlaces))
      ) %>%
        rename(`%tile|` = pctile, `2.5th` = X1, `25th` = X2, `50th` = X3, `75th` = X4, `97.5th` = X5) %>%
        mutate(across(.cols = `2.5th`:`97.5th`, .fns = ~sprintf(fmt = paste0("%.", decPlaces, "f"), .))) %>%
        #select(`2.5th`:`97.5th`) %>%
        mutate(dummy = "") %>%
        column_to_rownames("dummy")
    }else{
      invisible()
    }
  }
  
}





# Function for beta quantiles of model input parameters
#' 
#' @import shiny
#' @import dplyr
#' @importFrom tibble column_to_rownames remove_rownames
#' 
#' @noRd
beta_qtl_tbl <- function(p, sd, varTag, decPlaces = 3){

  req(p, sd)

  eta <- p*(1-p)/sd^2 - 1
  alpha <- eta*p
  beta <- eta*(1-p)

  betaMeanVarCond <- sd^2 < p*(1-p)

  if(p >= 0 & p <= 1){
    if(sd == 0){   # if sd == 0, fixe sampled values to chosen p/mu (default qbeta generates fixed 0.5, as alpha = beta = Inf => p = 0.5)
      data.frame(pctile = paste0(varTag, "|"), X1 = p, X2 = p, X3 = p, X4 = p, X5 = p) %>%
        rename(`%tile|` = pctile, `2.5th` = X1, `25th` = X2, `50th` = X3, `75th` = X4, `97.5th` = X5) %>%
        mutate(across(.cols = `2.5th`:`97.5th`, .fns = ~sprintf(fmt = paste0("%.", decPlaces, "f"), .))) %>%
        mutate(dummy = "") %>%
        column_to_rownames("dummy")
    }else{
      if(sd > 0){
        if(betaMeanVarCond){
          data.frame(pctile = paste0(varTag, "|"), t(round(qbeta(p = c(0.025, 0.25, 0.5, 0.75, 0.975), shape1 = alpha, shape2 = beta), decPlaces))) %>%
            rename(`%tile|` = pctile, `2.5th` = X1, `25th` = X2, `50th` = X3, `75th` = X4, `97.5th` = X5) %>%
            #mutate_at(.vars = vars(`2.5th`:`97.5th`), list(~sprintf(fmt = paste0("%.", decPlaces, "f"), .))) %>%
            mutate(across(.cols = `2.5th`:`97.5th`, .fns = ~sprintf(fmt = paste0("%.", decPlaces, "f"), .))) %>%
            mutate(dummy = "") %>%
            column_to_rownames("dummy")
        }else{
          warning("Invalid sd value: the condition sd^2 < p*(1-p) is not satisfied.",
                  " No table returned.")
          invisible()
        }
      }else{
        warning("Invalid sd value: standad deviation must be >= 0.",
                " No plot returned.")
        invisible()
      }
    }
  }else{
    warning("Invalid p value: probability value must lie within 0 and 1.", 
            " No plot returned")
    invisible()
  }
}