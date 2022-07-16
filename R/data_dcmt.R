#' Lookup table listing windfarm and turbine parameters. Used to generate UI
#' input widgets dynamically.
#'
#' @format A dataframe with the following columns: 
#' \describe{
#'   \item{par_name}{used in coding}
#'   \item{par_label}{used for display parameter name in UI}
#'   \item{par_id}{used for inputIDs in e.g. buttons, plots, etc}
#'   \item{par_dist}{probability distribution assumed for the parameter}
#'   \item{dflt_mean}{default for the mean}
#'   \item{dflt_sd}{default for the sd}
#' }
"wf_pars_lookup"



#' Lookup table listing species features treated as random variables with a
#' probability distribution. Used to generate UI input widgets dynamically.
#'
#' @format A data-frame with the following columns: 
#' \describe{
#'   \item{par_name}{used in coding}
#'   \item{par_label}{used for display parameter name in UI}
#'   \item{par_id}{used for inputIDs in e.g. buttons, plots, etc}
#'   \item{par_dist}{probability distribution assumed for the parameter}
#' }
"spp_probdist_pars"







#' Lookup table with species-specific default parameter values, for the most
#' common seabird species and a "demo species". Literature references for
#' default values are also provided.
#' 
#' @format A data-frame with the following columns: 
#' \describe{
#'   \item{spp_id}{The species name/id, to be used in coding}
#'   \item{body_lt}{A list-column of nested data frames with the mean
#'   and sd of body length (m).}
#'   \item{wing_span}{A list-column of nested data frames with the mean
#'   and sd of wing span (m).}
#'   \item{fl_speed}{A list-column of nested data frames with the mean
#'   and sd of flight speed (m/s).}
#'   \item{nct_act}{A list-column of nested data frames with the mean
#'   and sd of nocturnal activity.}
#'   \item{avoid_bsc}{A list-column of nested data frames with the mean
#'   and sd of avoidance probability for the basic models (Option 1).}
#'   \item{avoid_extended}{A list-column of nested data frames with the
#'   mean and sd of avoidance probability for the extended models (Options 2, 3
#'   and 4).}
#'   \item{fl_type}{A list-column of nested data frames with the flight
#'   type.}
#'   \item{upwind_fl}{A list-column of nested data frames with the
#'   proportion of upwind flights.}
#'   \item{prop_crh}{A list-column of nested data frames with the
#'   proportion at collision risk height.}
#'   \item{mth_dens}{A list-column of nested data frames with monthly
#'   mean and sd of in-flight density (birds/km^2) at the windfarm site. Each
#'   data frame consists of 2 rows and 12 columns:
#'    \describe{
#'       \item{January}{the Mean and SD of daytime in-flight bird density in
#'        January, in birds/km^2.}
#'        ...
#'       \item{December}{the Mean and SD of daytime in-flight bird density in
#'       December, in birds/km^2.}
#'     }
#'   }
#'   \item{fhd_boot}{A list-column comprising nested data frames with bootstrap
#'   replicates of flight height distribution, i.e. the proportion of bird
#'   flights at 1 metre height intervals. Each data frame consists of 500 rows
#'   and 201 columns, with bootstrap replicates of the distribution of bird
#'   flights at 1m height bands, up to 500m above sea level. Specifically:
#'   \describe{
#'   \item{height_m}{Height above sea level, in metres. First element represents
#'   the 0-1 meters height band, and height interval is 1 metre.}
#'   \item{bootId_1}{First bootstrap sample of the proportion of bird flights
#'   within each height interval}
#'   ...
#'   \item{bootId_200}{200th bootstrap sample of the proportion of bird flights
#'   within each height interval}
#'    }
#'   }
#'   \item{fhd_est}{A list-column comprising nested data frames with estimates
#'   of flight height profiles, expressed as the proportion of birds in 1 metre
#'   height intervals. Each data frame contains estimates of the proportion of
#'   flights at 1-metre height intervals above sea level, with columns:
#'   \describe{
#'     \item{height}{Height above sea level, in metres. First element represents
#'     the 0-1 meters height band, and height interval is 1 metre. Highest
#'     height band is currently 499-500 metres.}
#'     \item{est}{Maximum Likelihood estimates of the proportion of flights within
#'      height intervals.}
#'     \item{lcl}{Lower limit of the 95% CI for the proportion of bird flights
#'      within height intervals.}
#'     \item{ucl}{Upper limit of the 95% CI for the proportion of bird flights
#'      within height intervals.}
#'     \item{med}{Median of the proportion of bird flights within height intervals.}
#'   }
#'  }
#' }
#' 
#' @source Flight Height Distribution data from [Johnston et al
#'   (2014)](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2664.12191)
#'
"spp_dflts"






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
#' @format A dataframe with 1 row and 12 columns.
#' 
#' \describe{
#'   \item{January}{The percentage of January during which operational wind conditions}
#'   \item{February}{The percentage of February during which wind conditions
#'   allowed for turbine operation}
#'   ...
#'   }
"startup_wind_avbl"




#' Template data for FHD bootstrap replicates
#'
#' Bootstrap samples of FHD, i.e. the proportion of bird flights at 1 metre
#' height intervals.
#'
#' @format data frame, with 500 rows and 201 columns, containing 200 bootstrap
#'   replicates of the distribution of bird flights at 1m height bands, up to
#'   500m above sea level. 
#'   
#' \describe{
#'   \item{height_m}{Height above sea level, in metres. First element represents the
#'   0-1 meters height band, and height interval is 1 metre.}
#'   \item{bootId_1}{First bootstrap sample of the proportion of bird flights
#'   within each height interval}
#'   ...
#'  \item{bootId_200}{200th bootstrap sample of the proportion of bird flights
#'   within each height interval}
#' }
"FHD_bootstrap_template"




#' Template data for FHD estimates
#'
#' FHD estimates, i.e. the proportion of birds flights at 1 metre height intervals.
#'
#' @format data frame, with 500 rows and 2 columns the distribution of bird
#'   flights at 1m height bands, up to 500m above sea level.   
#' \describe{
#'   \item{height_m}{Height above sea level, in metres. First element represents
#'   the 0-1 meters height band, and height interval is 1 metre.}
#'   \item{prop}{the proportion of bird flights within each height interval}
#' }
"FHD_template"




#' Template data for monthly in-flight density reference point estimates
#'
#' @format data frame, with 13 columns:
#' \describe{
#'   \item{Percentiles}{numeric values between 0 and 100 (e.g. 25 represents the
#'   25th percentile)}
#'   \item{January}{Point estimates of in-flight bird densities in January.}
#'   ...
#'   \item{December}{Point estimates of in-flight bird densities in December}
#' }
"monthDens_pctls_template"



#' Template data for monthly density random samples
#'
#' @format data frame, with 12 columns:  
#' \describe{
#'   \item{January}{Random draws of in-flight bird densities in January.}
#'   ...
#'   \item{December}{Random draws of in-flight bird densities in December.}
#' }
"monthDens_draws_template"







