#' summ_spp_collisions 
#'
#' @description A fct function
#' @return The return value, if any, from executing the function.
#' 
#' @noRd
summ_spp_collisions <- function(spp_out, band_mode){
  
  flextable::set_flextable_defaults(font.size = 10, font.family = "Arial")
  
  colls_df <- spp_out$colls %>%
    dplyr::mutate(crm_option = stringr::str_replace(crm_option, "opt", "Option "))
  
  if(spp_out$out_opt == "months"){
    colls_df <- colls_df %>%
      dplyr::mutate(
        time_period = month.name[match(time_period, month.abb)],
        time_period = factor(time_period, levels = month.name)
      ) %>%
      dplyr::arrange(time_period)
  }
  
  # Generate df for exporting + flextable for display in UI and automated report
  if(band_mode == FALSE){   # stochastic mode
    
    # Summary stats for draws of collision estimates
    summ_df <- colls_df %>%
      dplyr::group_by(aggr_label, time_period, crm_option) %>%
      dplyr::summarise(
        mean = mean(collisions),
        median = median(collisions),
        sd = sd(collisions),
        cv = 100*sd/mean,
        pctl_2.5 = quantile(collisions, probs = 0.025),
        pctl_97.5 = quantile(collisions, probs = 0.975)) %>%
      ungroup()
    
    # output type-specific formatting
    if(spp_out$out_opt == "annum"){
      summ_df <- dplyr::select(summ_df, -c(aggr_label, time_period))
    } else if(spp_out$out_opt == "seasons"){
      summ_df$time_period <- tp_frmt(summ_df$time_period)
    } else{
      summ_df <- dplyr::select(summ_df, -c(aggr_label))
    }
    
    # generate base flextable
    summ_ft <- summ_df %>%
      flextable::flextable() %>%
      flextable::set_header_labels(
        aggr_label = "Season",
        time_period = "Time Period",
        crm_option = "CRM Option",
        mean = "Mean",
        median = "Median",
        sd = "SD",
        cv = "CV",
        pctl_2.5 = "2.5%",
        pctl_97.5 = "97.5%"
      )
    
  }else{    # deterministic mode
    
    # format for exporting
    summ_df <- colls_df %>%
      #dplyr::relocate(time_period, .before = 1) %>%
      dplyr::select(-iter)
    
    if(spp_out$out_opt == "annum"){
      summ_df <- dplyr::select(summ_df, -c(aggr_label, time_period))
    }else if(spp_out$out_opt == "seasons"){
      summ_df$time_period <- tp_frmt(summ_df$time_period)
    }else{
      summ_df <- dplyr::select(summ_df, -c(aggr_label))
    }
    
    # generate base flextable
    summ_ft <- summ_df %>%
      flextable::flextable() %>%
      flextable::set_header_labels(
        aggr_label = "Season",
        time_period = "Time Period",
        crm_option = "CRM Option",
        collisions = "No. Collisions"
      )
  }
  
  # define period label for caption
  period_label <- switch(
    spp_out$out_opt,
    annum = "per Annum",
    months = "by Month", 
    seasons = "by Season"
  )
  
  
  # Table caption, depending on simulation mode
  if(band_mode == FALSE){
    tab_caption <-  glue::glue(
      "Summary statistics of collision estimates for {spp_out$spp_label} at",
      " {spp_out$wf_label}, {period_label}")
  }else{
    tab_caption <-  glue::glue(
      "Collision estimates for {spp_out$spp_label} at {spp_out$wf_label},",
      " {period_label}")
  }
  
  
  # flextable - specify overall formatting
  summ_ft <- summ_ft %>%
    flextable::colformat_double(digits = 3, big.mark = " ") %>%
    flextable::autofit() %>%
    flextable::merge_v() %>%
    flextable::fix_border_issues() %>%
    flextable::theme_vanilla() %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::bg(
      i = rep_len(c(FALSE, TRUE), length.out = nrow(summ_df)), 
      #j = -1, 
      bg = "#edf2f7"
    ) %>%
    flextable::set_caption(
      caption = tab_caption
    )
  
  list(summ_df = summ_df, summ_ft = summ_ft)
}






tp_frmt <- function(time_period){
  stringr::str_split(time_period, " - ") %>%
    purrr::map_chr(., function(x){
      glue::glue_collapse(month.name[match(x, month.abb)], sep = " - ")
    })
}
  



# season_format <- function(collsum_df, season_df){
#   
#   seas <- season_df %>%
#     dplyr::mutate(
#       across(!season_id, 
#              .fns = ~ month.name[match(., month.abb)], 
#              .names = "{.col}_long")
#     ) %>%
#     tidyr::unite(time_period, start_month, end_month, sep = "_") %>%
#     tidyr::unite(time_period_long, start_month_long, end_month_long, sep = " - ")
#   
#   
#   collsum_df %>%
#     dplyr::left_join(., seas, by = "time_period") %>%
#     dplyr::mutate(time_period = time_period_long, .keep = "unused") %>%
#     dplyr::relocate(season_id, .before = 1)
# }
