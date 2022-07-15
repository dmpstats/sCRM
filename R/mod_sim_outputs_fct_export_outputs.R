#' Export sCRM outputs to external folder
#' 
#' @param scrm_outputs list containing the model outputs of the current scrm run
#' @param scrm_results list containing the summarized outputs of the current scrm run
#' @param outputs_dir string, path to directory where to store exporting files
#' @param band_mode logical, whether the model was run in stochastic (`FALSE`)
#'   or deterministic (`TRUE`) mode
#' 
#' 
#' @noRd
export_outputs <- function(scrm_outputs, scrm_results, outputs_dir, band_mode){
  
  # convert list with outputs from all scenarios into one single data frame
  simulation_outputs <- scrm_outputs %>%
    purrr::map_dfr(function(x){
      purrr::map_dfr(x, function(y){
        y$colls %>%
          dplyr::mutate(
            wind_farm = y$wf_label,
            species = y$spp_label,
            aggr_opt = y$out_opt,
            .before = aggr_label
          ) 
      })
    })
  
  # convert list with summary stats from all scenarios into one single data frame
  summary_stats <- purrr::imap_dfr(scrm_results, function(wf, wf_id){
    purrr::imap_dfr(wf, function(spp_results, spp_id){
      
      out_opt <- scrm_outputs[[wf_id]][[spp_id]]$out_opt
      
      out <- spp_results$summ_df %>%
        tibble::add_column(
          wind_farm = scrm_outputs[[wf_id]][[spp_id]]$wf_label,
          species = scrm_outputs[[wf_id]][[spp_id]]$spp_label,
          aggr_opt = out_opt,
          .before = 1
        )
      
      if(out_opt == "annum"){
        out <- out %>%
          tibble::add_column(
            aggr_label = "Annum", 
            time_period = "January - December",
            .after = "aggr_opt"
          )
      }else if(out_opt == "months"){
        out <- out %>%
          tibble::add_column(aggr_label = "Month", .after = "aggr_opt")
      }
      out
    })
  })
  
  # write outputs to xlsx
  if(band_mode == FALSE){
    writexl::write_xlsx(
      x = list(
        simulation_outputs = simulation_outputs, 
        summary_statistics = summary_stats),
      path = fs::path(outputs_dir, "scrm_collision_estimates.xlsx")
    )
  }else{ # identical simulation and summary datasets, so only exporting one of them
    writexl::write_xlsx(
      x = simulation_outputs, 
      path = fs::path(outputs_dir, "scrm_collision_estimates.xlsx")
    )
  }
}
