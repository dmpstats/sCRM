#' Lookup table for windfarm and turbine parameters
#'
#' @format A dataframe with the followig columns: 
#' \describe{
#'   \item{name}{used in coding}
#'   \item{label}{used for display parameter name in UI}
#'   \item{id}{used for inputIDs in e.g. buttons, plots, etc}
#'   \item{dist}{probability distribution assumed for the parameter}
#'   \item{dflt_mu}{default for the mean}
#'   \item{dflt_sd}{default for the sd}
#' }
"wf_pars_lookup"



#' Startup values for turbine monthly maintenance downtime
#'
#' @format A dataframe with 2 rows and 12 columns:
#' \describe{
#'   \item{January}{the Mean and SD percentage of January whilst turbines were not operating.}
#'   \item{February}{the Mean and SD percentage of February whilst turbines were not operating.}
#'   ...
#' }
"startup_trb_dwntm"




#' Startup values for monthly operational wind availability
#'
#' @format A dataframe with 1 row and 12 columns
#' 
#' \describe{
#'   \item{January}{The percentage of January during which operational wind conditions}
#'   \item{February}{The percentage of February during which wind conditions
#'   allowed for turbine operation}
#'   ...
"startup_wind_avbl"
