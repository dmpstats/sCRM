#' Export sCRM inputs to external folder
#' 
#' @param scrm_inputs list containing all the inputs of the current scrm run
#' @param inputs_dir string, path to directory where to store exporting files
#' 
#' @import zeallot
#' 
#' @noRd
export_inputs <- function(scrm_inputs, inputs_dir, band_mode){
  
  # loop over wfs
  for(wf_id in scrm_inputs$active_scens){   # wf_id <- "demolition";  wf_id <- "Demo_Windfarm"
    
    scen_inputs <- scrm_inputs$wf_scens[[wf_id]]
    
    # unpack wf inputs for current wf
    c(..., wf_ftrs, trb_ftr, wf_oper) %<-% scen_inputs$wf_inputs
    c(nturb, lat, wfwidth, tdloffset, lac) %<-% wf_ftrs
    c(nblades, rtradius, airgap, bladewth, bld_ptch_rtn) %<-% trb_ftr
    
    # list with current wf inputs
    wf_inputs <- 
      list(
        wf_features = tibble::tibble(
          wf_label = scen_inputs$wf_label,
          wf_n_trbs = nturb,
          wf_lat = lat,
          wf_width = wfwidth,	
          tidal_offset = tdloffset,	
          lrg_arr_corr = lac
        ),
        trbn_features = tibble::tibble(
          trb_n_blades = nblades,	
          rtr_radius = rtradius,	
          air_gap = airgap,	
          bld_width = bladewth,	
          rtn_pitch_opt = bld_ptch_rtn$opt,
          bld_pitch_mean = bld_ptch_rtn$bld$mean,
          bld_pitch_sd = bld_ptch_rtn$bld$sd,
          rtn_speed_mean = bld_ptch_rtn$rtn_speed$mean,
          rtn_speed_sd = bld_ptch_rtn$rtn_speed$sd,
          windspd_mean = bld_ptch_rtn$wind_spd$mean,
          windspd_sd = bld_ptch_rtn$wind_spd$sd
        ),
        windspd_to_rtnspd_ptch = bld_ptch_rtn$windspd_to_rtnspd_ptch,
        trb_downtime = wf_oper$trbdwnt %>% 
          dplyr::rename_with(stringr::str_to_lower) %>%
          select(month, mean, sd),
        wind_availability = wf_oper$wndavlb %>%
          dplyr::rename_with(stringr::str_to_lower) %>%
          select(month, wndavlb)
      ) %>%
      purrr::compact()
    
    
#    inputs_wf_dir <- fs::dir_create(fs::path(inputs_dir, wf_id))
    
    # export current wf inputs
    writexl::write_xlsx(
      x = wf_inputs,
      path = fs::path(inputs_dir, glue::glue("{wf_id}_wf_inputs.xlsx"))
    )
    
    # Loop over spps in wf
    for(spp_id in scen_inputs$active_spps){  # spp_id <- "Demo_Species"
      
      # unpack current spp inputs for current wf
      c(., spp_label, ..., biom_ftrs, fhd, inflight_ftrs, bird_dens, c(out_opt, seas_dt)) %<-% 
        scen_inputs$spp_in_wf[[spp_id]]
      
      if(bird_dens$dens_type == "tnorm"){
        bird_dens$dens_dt <- bird_dens$dens_dt %>% 
          dplyr::rename_with(stringr::str_to_lower) %>%
          dplyr::select(month, mean, sd)
      }
      
      if(band_mode == TRUE){
        fhd$gen_fhd <- fhd$gen_fhd %>%
          select(height, prop)
      }
      
      spp_in_wf_inputs <- 
        list(
          input_options = tibble::tibble(
            spp_label = spp_label,
            generic_fhd = ifelse("gen_fhd" %in% fhd$fhd_type, "Yes", "No"),
            site_specific_fhd = ifelse("site_fhd" %in% fhd$fhd_type, "Yes", "No"),
            density_input_type = bird_dens$dens_type,
            collisions_aggr_level = out_opt
          ),
          body_dimensions = dplyr::bind_rows(biom_ftrs, .id = "id") %>%
            tidyr::pivot_wider(
              names_from = id, 
              values_from = c(mean, sd), 
              names_glue = "{id}_{.value}",
              names_vary = "slowest"),
          inflight_features = tibble::tibble(
            flight_type = inflight_ftrs$fltype,
            prop_upwind = inflight_ftrs$upwind,
            fl_speed_mean = inflight_ftrs$fl_speed$mean,
            fl_speed_sd = inflight_ftrs$fl_speed$sd,
            avoid_bsc_mean = inflight_ftrs$avoid_bsc$mean,
            avoid_bsc_sd = inflight_ftrs$avoid_bsc$sd,
            avoid_ext_mean = inflight_ftrs$avoid_ext$mean,
            avoid_ext_sd = inflight_ftrs$avoid_ext$sd,
            nct_act_mean = inflight_ftrs$nct_act$mean,
            nct_act_sd = inflight_ftrs$nct_act$sd,
            prop_crh_mean = inflight_ftrs$colrisk$mean,
            prop_crh_sd = inflight_ftrs$colrisk$sd
          ),
          generic_fhd = fhd$gen_fhd,
          site_specific_fhd = fhd$site_fhd,
          density_estimates = bird_dens$dens_dt,
          seasonal_definitions = seas_dt
        ) %>%
        purrr::compact()
      
      # export current spp-in-wf inputs
      writexl::write_xlsx(
        x = spp_in_wf_inputs,
        path = fs::path(inputs_dir, glue::glue("{wf_id}_{spp_id}_inputs.xlsx"))
      )
    } # end of loop over spps in wf
  } # end of loop over wfs
} 
