## code to prepare `global_obj` dataset goes here

wf_colour <- "olivedrab"
spp_colour <- "#d39a2d"
error_colour <- "#dd4b39"


# initial Windfarm IDs
init_wf_label <- "Demo Windfarm"
init_wf_id <- label2id(init_wf_label)
init_wf_tp_id <- paste0("tbp-wf-", init_wf_id)

# initial Species IDs
init_spp_label <- "Demo Species"
init_spp_id <- label2id(init_spp_label)
init_spp_tp_id <- paste0("tbp-", init_spp_id)

# save as internal data (i.e. file 'sysdata.rda')
usethis::use_data(
  wf_colour, 
  spp_colour,
  error_colour,
  init_wf_label, 
  init_wf_id,
  init_wf_tp_id,
  init_spp_label,
  init_spp_id, 
  init_spp_tp_id,
  internal = TRUE, 
  overwrite = TRUE)
