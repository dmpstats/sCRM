## code to prepare look-up tables for input parameters

# Wind farm and turbine features ---------------------------------------------


wf_pars_lookup <- tibble::tribble(
    ~par_name,              ~par_label,   ~par_id,   ~par_dist,    ~dflt_mean,    ~dflt_sd,
  "bld_pitch",     "Blade Pitch (deg)",    "ptch",     "tnorm",             2,         0.1,
  "rtn_speed",  "Rotation Speed (rpm)",   "rtspd",     "tnorm",            10,         0.5,
  "wind_spd",       "Wind Speed (m/s)",  "wndspd",     "tnorm",          7.74,         3.2
)

wf_pars_lookup

usethis::use_data(wf_pars_lookup, overwrite = TRUE)


# start-up values for turbine downtime
startup_trb_dwntm <- data.frame(
  month = month.name,
  Mean = round(runif(12, 3, 8), 2),
  SD =  round(runif(12, 1, 2), 2)) %>%
  tidyr::pivot_longer(cols = Mean:SD) %>%
  tidyr::pivot_wider(values_from = value, names_from = month) %>%
  tibble::column_to_rownames("name")
  
usethis::use_data(startup_trb_dwntm, overwrite = TRUE)


# start-up values for wind availability
startup_wind_avbl <- data.frame(
  month = month.name,
  avlb = c(96.28, 96.53, 95.83, 92.78, 90.86, 92.22, 89.11, 89.92, 93.71, 96.14, 97.14, 96.41)
) %>%
  tidyr::pivot_wider(values_from = avlb, names_from = month)

rownames(startup_wind_avbl) <- "avlb"

usethis::use_data(startup_wind_avbl, overwrite = TRUE)
