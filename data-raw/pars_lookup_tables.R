## code to prepare look-up tables for input parameters


### ------------------------------------ ###
####   Wind farm and turbine features   ####
### ------------------------------------ ###

# ------ Parameter specifications
wf_pars_lookup <- tibble::tribble(
  ~par_name,              ~par_label,   ~par_id,    ~par_dist,   ~dflt_mean,   ~dflt_sd,
  "bld_pitch",     "Blade Pitch (deg)",    "ptch",     "tnorm",           2,        0.1,
  "rtn_speed",  "Rotation Speed (rpm)",   "rtspd",     "tnorm",          10,        0.5,
  "wind_spd",      "Wind Speed (m/s)",   "wndspd",     "tnorm",        7.74,        3.2
)


usethis::use_data(wf_pars_lookup, overwrite = TRUE)



# ---- Start-up values for turbine downtime
startup_trb_dwntm <- data.frame(
  month = month.name,
  Mean = round(runif(12, 3, 8), 2),
  SD =  round(runif(12, 1, 2), 2)) %>%
  tidyr::pivot_longer(cols = Mean:SD) %>%
  tidyr::pivot_wider(values_from = value, names_from = month) %>%
  tibble::column_to_rownames("name")
  
usethis::use_data(startup_trb_dwntm, overwrite = TRUE)



# ---- Start-up values for wind availability
startup_wind_avbl <- data.frame(
  month = month.name,
  avlb = c(96.28, 96.53, 95.83, 92.78, 90.86, 92.22, 89.11, 89.92, 93.71, 96.14, 97.14, 96.41)
) %>%
  tidyr::pivot_wider(values_from = avlb, names_from = month)

rownames(startup_wind_avbl) <- "avlb"

usethis::use_data(startup_wind_avbl, overwrite = TRUE)





### ------------------------------------ ###
####         Species Features           ####
### ------------------------------------ ###


# Probability Distribution parameters lookup table -----------------------------

spp_probdist_pars <- tibble::tribble(
   ~par_name,                  ~par_label,     ~par_id,   ~par_dist,    #~dflt_mean,    ~dflt_sd,
   "body_lt",           "Body Length (m)",      "bdlt",     "tnorm",    #         1,         0.1,
 "wing_span",             "Wing Span (m)",    "wngspn",     "tnorm",    #         1,         0.1,
  "fl_speed",        "Flight Speed (m/s)",     "flspd",     "tnorm",    #         1,         0.1,
   "nct_act",   "Nocturnal Activity",    "nctact",      "beta",    #       0.5,         0.1,
 "avoid_bsc",      "Basic Avoidance",  "avoidbsc",      "beta",    #       0.5,         0.1,
 "avoid_ext",   "Extended Avoidance",  "avoidext",      "beta",    #       0.5,         0.1
  "prop_crh",         "Proportion at CRH",      "pcrh",      "beta"
)

usethis::use_data(spp_probdist_pars, overwrite = TRUE)




# Parameter Defaults  ----------------------------------------------------------

# ---- Johnston FHD **estimates** for band option

# reshape data from {stochLAB}
Johnston_fhd_est <- stochLAB::Johnston_Flight_heights_SOSS %>% 
  tidyr::separate(col = variable, sep = "\\.", into = c("spp", "estimate")) %>%
  tidyr::pivot_wider(id_cols = c(height, spp), names_from = estimate, values_from = prop)

# find correspondence of spp labels between bootstrap data a estimates data
spp_label_est <- stringr::str_to_lower(unique(Johnston_fhd_est$spp)) ; spp_label_est

spp_label_boots <- names(stochLAB::generic_fhd_bootstraps) %>%
  stringr::str_replace_all("_", "") %>%
  stringr::str_to_lower() ; spp_label_boots

spp_label_corresp <- sapply(
  spp_label_est, 
  function(x){
    stringr::str_which(spp_label_boots, pattern = x)
  },
  USE.NAMES = FALSE
)

length(spp_label_est) == length(spp_label_boots)
length(spp_label_est) == length(spp_label_corresp)


# Table with label correspondence
spp_labels <- data.frame(
  label_boot = names(stochLAB::generic_fhd_bootstraps)[spp_label_corresp],
  label_est = unique(Johnston_fhd_est$spp)
)

Johnston_fhd_est <- dplyr::left_join(Johnston_fhd_est, spp_labels, by = c("spp" = "label_est"))

# extend to 500m above sea level and pad with zeros
Johnston_fhd_est <- Johnston_fhd_est %>%
  split(.$label_boot) %>%
  purrr::map(function(dt){
    
    max_height <- max(dt$height)
    
    dt %>%
      dplyr::select(height, est:med) %>%
      tibble::add_row(
        data.frame(
          height = (max_height+1):499, 
          est = 0,
          lcl = 0,
          ucl = 0,
          med = 0
        )
      )
  })

#usethis::use_data(Johnston_fhd_est, overwrite = TRUE)



# ---- Species-specific default values and combine with FHD data

spp_dt <- readr::read_csv("data-raw/default species dimensions and flightspeeds.csv") |>
  dplyr::mutate(spp_id = stringr::str_replace_all(Species, pattern = "\\s|-", replacement = "_")) |>
  dplyr::select(spp_id, contains("midpoint"), contains("SD"), contains("flightspeed"), contains("ref")) |>
  dplyr::rename(
    body_lt.mean = length_midpoint_m,
    body_lt.sd = length_SD_m,
    wing_span.mean = wingspan_midpoint_m,
    wing_span.sd = wingspan_SD_m,
    fl_speed.mean = flightspeed_minpersec,
    fl_speed.ref = flightspeed_ref,
    wing_span.ref = wingspan_ref, 
    body_lt.ref = lenght_ref
  ) |>
  dplyr::mutate(dplyr::across(everything(), as.character)) |>
  tidyr::pivot_longer(cols = -spp_id, names_to = "pars") |>
  tidyr::separate(col = pars, into = c("par_name", "stat"), sep = "\\.") |>
  tidyr::pivot_wider(names_from = stat, values_from = value) |>
  dplyr::rename(dflt_mean = mean, dflt_sd = sd) |>
  dplyr::group_by(spp_id, par_name) |>
  tidyr::nest(data = c(dflt_mean:ref)) |>
  tidyr::pivot_wider(names_from = par_name, values_from = data) 


# Species present in fhd but not in default data
missing_spp <- setdiff(names(Johnston_fhd_est), spp_dt$spp_id)

# Add outstanding species to default data
spp_dt <- spp_dt %>%
  dplyr::ungroup() %>%
  dplyr::add_row(
    spp_id = missing_spp
  ) 

# Add FHD data
spp_dt <- spp_dt %>%
  dplyr::mutate(
    fhd_boot = purrr::map(spp_id, ~(stochLAB::generic_fhd_bootstraps[[.]])),
    fhd_est = purrr::map(spp_id, ~Johnston_fhd_est[[.]])
  )

spp_dt



# ---- Demo species start-up values

# demo_spp_dflts <- list()
# 
# demo_spp_dflts$body_lt <- data.frame(dflt_mean = 0.5, dflt_sd = 0.05, ref = NA)
# demo_spp_dflts$wing_span <- data.frame(dflt_mean = 1, dflt_sd = 0.05, ref = NA)
# demo_spp_dflts$fl_speed <- data.frame(dflt_mean = 14, dflt_sd = 1.2, ref = NA)
# demo_spp_dflts$nct_act <- data.frame(dflt_mean = 0.04, dflt_sd = 0.01, ref = NA)
# demo_spp_dflts$avoid_bsc <- data.frame(dflt_mean = 0.98, dflt_sd = 0.001, ref = NA)
# demo_spp_dflts$avoid_ext <- data.frame(dflt_mean = 0.96, dflt_sd = 0.002, ref = NA)
# demo_spp_dflts$fl_type <- data.frame(dflt = "Flapping", ref = NA)
# demo_spp_dflts$upwind_fl <- data.frame(dflt_mean = 0.5, ref = NA)
# demo_spp_dflts$prop_crh <- data.frame(dflt_mean = 0.08, dflt_sd = 0.01, ref = NA)
# 
# demo_spp_dflts$mth_dens <- data.frame(
#   month = month.name,
#   Mean = round(runif(12, 0.8, 4), 2),
#   SD =  round(runif(12, 0.2, 0.8), 2)) |>
#   tidyr::pivot_longer(cols = Mean:SD) |>
#   tidyr::pivot_wider(values_from = value, names_from = month) |>
#   tibble::column_to_rownames("name")
# 
# demo_spp_dflts$fhd_boot <- tibble::as_tibble(stochLAB::generic_fhd_bootstraps$Black_legged_Kittiwake)
# 
# demo_spp_dflts$fhd_est <- Johnston_fhd_est$Black_legged_Kittiwake
# 
# demo_spp_dflts$seasons <-  data.frame(
#   period_name = c("Breeding", "Non-breeding"),
#   start_month = c("April", "September"),
#   end_month = c("August", "March")
# )
# 
# 
# demo_spp_dflts <- tibble::enframe(demo_spp_dflts) |>
#   dplyr::mutate(spp_id = "Demo_Species") |>
#   tidyr::pivot_wider(names_from = name, values_from = value)
# 
# 
# demo_spp_dflts



generate_dflt_spp <- function(spp_id){
  
  fhd_spp <- sample(names(stochLAB::generic_fhd_bootstraps), 1)
  
  list(
    body_lt = data.frame(
      dflt_mean = runif(1, 0.3, 1),  
      dflt_sd = runif(1, 0.05, 0.1), 
      ref = NA),
    wing_span = data.frame(
      dflt_mean = runif(1, 0.6, 1.5), 
      dflt_sd = runif(1, 0.05, 0.1), 
      ref = NA),
    fl_speed = data.frame(
      dflt_mean = runif(1, 20, 25), 
      dflt_sd = runif(1, 1.2, 2), 
      ref = NA),
    nct_act = data.frame(
      dflt_mean = runif(1, 0.06, 0.2),
      dflt_sd = runif(1, 0.02, 0.1), 
      ref = NA),
    avoid_bsc = data.frame(
      dflt_mean = runif(1, 0.90, 0.98), 
      dflt_sd = runif(1, 0.001, 0.01), 
      ref = NA),
    avoid_ext = data.frame(
      dflt_mean = runif(1, 0.90, 0.98), 
      dflt_sd = runif(1, 0.002, 0.01), 
      ref = NA),
    fl_type = data.frame(
      dflt = sample(c("Gliding", "Flapping"), size = 1), 
      ref = NA),
    upwind_fl = data.frame(
      dflt_mean = runif(1, 0.2, 0.9), 
      ref = NA),
    prop_crh = data.frame(
      dflt_mean = runif(1, 0.05, 0.2),
      dflt_sd = runif(1, 0.07, 0.1), 
      ref = NA),
    mth_dens = data.frame(
      month = month.name,
      Mean = round(runif(12, 0.8, 7), 2),
      SD =  round(runif(12, 0.2, 0.8), 2)) |>
      tidyr::pivot_longer(cols = Mean:SD) |>
      tidyr::pivot_wider(values_from = value, names_from = month) |>
      tibble::column_to_rownames("name"),
    fhd_boot = tibble::as_tibble(stochLAB::generic_fhd_bootstraps[[fhd_spp]]),
    fhd_est = Johnston_fhd_est[[fhd_spp]],
    seasons = data.frame(
      period_name = c("Breeding", "Non-breeding"),
      start_month = c("April", "September"),
      end_month = c("August", "March")
    )
  ) |>
    tibble::enframe() |>
    dplyr::mutate(spp_id = spp_id) |>
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    dplyr::mutate(
      dplyr::across(c(body_lt:avoid_ext, upwind_fl, prop_crh), 
                    .fns = ~purrr::map(., round, 3)))
}



demo_spp_dflts <- purrr::map_dfr(
  c("Demo_Species", "Demo_Species_2", "Demo_Species_3"), 
  generate_dflt_spp)





# ---- Combine Demo species with other defaults into a single dataset 

spp_dflts <- dplyr::bind_rows(demo_spp_dflts, spp_dt)
spp_dflts
dim(spp_dflts)

usethis::use_data(spp_dflts, overwrite = TRUE)
























