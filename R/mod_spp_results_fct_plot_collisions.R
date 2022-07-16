#' plot_collisions 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' 
#' @import ggplot2
#' @import ggdist
#'
#' @noRd

plot_spp_collisions <- function(spp_out, band_mode){
  
  # set up colour pallete for output plots
  #clr <- c(MetBrewer::met.brewer("Lakota", 4))
  clr <- c(MetBrewer::met.brewer("Juarez", 5, direction = -1))  # "Austria # Juarez
  crm_opt_pal <- c("opt1" = clr[1], "opt2" = clr[2], "opt3" = clr[3], "opt4" = clr[4])
  
  colls_df <- spp_out$colls
  
  # if outputs by month, order months chronologically
  if(spp_out$out_opt == "months"){
    colls_df <- colls_df %>%
      dplyr::mutate(time_period = factor(time_period, levels = month.abb))
  }
  
  # for seasonal outputs, plot by season label (by overwriting time-period)
  if(spp_out$out_opt == "seasons"){
    colls_df <- dplyr::mutate(colls_df, time_period = aggr_label)
  }
  
  
  # active model options
  model_opt <- unique(colls_df$crm_option)
  

  if(band_mode == FALSE){ # plot for stochastic outputs
    
    # base plot
    p <- colls_df %>%
      ggplot(aes(y = collisions, x = crm_option, fill = crm_option)) +
      stat_slab(aes(color = crm_option), slab_alpha = 0.6, scale = 0.5) +
      stat_dotsinterval(
        quantiles = 50, 
        side = "left", 
        slab_color = NA, 
        scale = 0.5,  
        shape = 21
      )
    
    # add faceting for monthly or seasonal outputs
    if(spp_out$out_opt != "annum"){
      p <- p + facet_wrap(~time_period)
    }
    
    # customise x-axis and legend position
    p <- p + 
      theme(
        strip.text = element_text(size = 12),
        strip.background = element_rect(fill = "#D8DEE9"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom"
      ) +
    labs(subtitle = glue::glue(
      "Density distribution, median, 66% and 95% quantile intervals and quantile",
      " dotplots (each dot represents ~2% chance outcome) of simulated values"))
    
  }else{   # plot for deterministic outputs
    
    # variable mapping and legend 
    if(spp_out$out_opt == "annum"){ # for per annum outputs
      
      p <- colls_df %>%
        ggplot(
          aes(
            x = crm_option, 
            y = collisions, 
            color = crm_option, 
            fill = crm_option
          )
        ) +
        theme(
          axis.title.x = element_blank(),
          legend.position = "none"
        ) +
        scale_x_discrete(
          labels = stringr::str_replace(model_opt, "opt", "CRM Option ")
        )
      
    }else{ # for seasonal and monthly outputs
      
      p <- colls_df %>%
        ggplot(aes(
          x = time_period, y = collisions, 
          color = crm_option, fill = crm_option
        )) +
        theme(
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom"
        )
    }
    
    # Add lines and dots
    p <- p +
      geom_linerange(
        aes(ymin = 0, ymax = collisions),
        position = position_dodge(0.3),
        #col = "gray",
        size = 1
      ) +
      geom_point(
        alpha = 0.5,
        shape = 21,
        size = 3,
        stroke = 2,
        position = position_dodge(width = 0.3))
  }

  
  # define period label for title
  period_label <- switch(
    spp_out$out_opt,
    annum = "per Annum",
    months = "by Month", 
    seasons = "by Season"
  )
  
  # # Beggining of title label, depending on band_mode
  # if(band_mode == FALSE){
  #   title_fore <- "Collision risk distributions"
  # }else{
  #   title_fore <- "Collision risk estimates"
  # }
  
  
  # Add labs and set colour pallete
  p +
    labs(
      title = glue::glue(
        "Collision risk estimates for {spp_out$spp_label} at {spp_out$wf_label},",
        " {period_label}"),
      y = "Number of Collisions",
      fill = "", 
      color = "") +
    scale_color_manual(
      aesthetics = c("colour", "fill"),
      values = crm_opt_pal[model_opt], 
      labels = stringr::str_replace(model_opt, "opt", "CRM Option ")
    ) +
    theme(
      title = ggplot2::element_text(
        size = 12, 
        colour = "#5f5e5e", 
        margin = margin(1, 1, 20, 1)
      ),
      plot.subtitle = element_text(size = 12)
    )
}
