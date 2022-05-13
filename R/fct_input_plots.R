#' ------------------------------------------------------------------------ #
#' ----   Functions for building plot charts for input parameters      ------
#' ------------------------------------------------------------------------ #


#' Truncated Normal density plot
#'
#' @param mean numeric, the mean
#' @param sd numeric, the standard deviation
#' @param lower numeric, lower truncation point
#' @param upper numeric, upper truncation point
#' @param fill character string, colour of area below density curve
#' @param xlab character string, the x-axis label
#' 
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @importFrom msm dtnorm qtnorm
#' 
#' @return a ggplot object, a density plot
#' 
#' @noRd
tnorm_dplot <- function(mean, sd, lower = -Inf, upper = Inf, fill="olivedrab", xlab){
  
  req(mean, sd)
  
  #theme_set(theme_bw())
  
  if(sd == 0){
    if(mean <= lower){
      NULL
    }else{
      data.frame(x = seq(mean-500, mean+500, by = 1), y = 0) %>%
        ggplot(aes(x=x, y=y)) +
        geom_path() +
        geom_segment(aes(x = mean, xend = mean, y = 0, yend = Inf), size = 1) +
        coord_cartesian(xlim = c(lower, mean*1.65), ylim = c(0, 0.5)) +
        labs(y="Density", x = xlab)  
    }
  }else{
    if(sd > 0){
      
      # distTails <- qtnorm(c(0.00001, 0.999999), mean, sd, lower, upper)
      # xaxisUpLim <- distTails[2]*1.1
      # xaxisLowLim <- ifelse(distTails[1]*0.5 > mean/2.5, distTails[1]*0.7, 0)
      
      yaxisUpLimBands <- c(0.05, 0.1, 0.25, 1, 4, 6, 10, 15, 20)
      yaxisMax <- max(dtnorm(seq(mean-500, mean+500, by = 1), mean, sd, lower, upper))
      
      if(yaxisMax > last(yaxisUpLimBands)){
        yaxisUpLim <- yaxisMax
      }else{
        yaxisUpLim <- yaxisUpLimBands[findInterval(yaxisMax, yaxisUpLimBands)+1]  
      }
      
      data.frame(qtls = qtnorm(c(0.0001, 0.9999), mean, sd, lower, upper))  %>%
        ggplot(aes(qtls)) +
        stat_function(fun = dtnorm,
                      args = list(mean, sd, lower, upper),
                      geom = "area",
                      fill = fill,
                      col = "black",
                      #size = 1,
                      alpha = 0.5,
                      outline.type = "full"
                      ) +
        labs(y="Probability Density", x = xlab) +
        coord_cartesian(
          #xlim = c(xaxisLowLim, max(0, xaxisUpLim, na.rm=TRUE)), 
          ylim = c(0, yaxisUpLim)
          )
    }else{
      NULL
    }
  }
}





#' Beta density plot
#'
#' @param p numeric, expected probability
#' @param sd numeric, the standard deviation
#' @param fill character string, colour of area below density curve
#' @param xlab character string, the x-axis label
#' 
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @importFrom msm dtnorm qtnorm
#' 
#' @return a ggplot object, a density plot
#' 
#' @noRd
beta_dplot <- function(p, sd, fill = "olivedrab", xlab){
  
  req(p, sd)

  betaMeanVarCond <- sd^2 < p*(1-p)
  
  # Update variable label for x-axis legend
  xlab <- stringr::str_replace(xlab, "pb", replacement = "probability")
  xlab <- stringr::str_replace(xlab, "pp", replacement = "proportion")
  
  if(p >= 0 & p <= 1){
    if(sd == 0){
      
      data.frame(x = seq(0, 1, length.out = 100), y = 0) %>%
        ggplot(aes(x=x, y=y)) +
        geom_path() +
        geom_segment(aes(x = p, xend = p, y = 0, yend = Inf), size = 1) +
        coord_cartesian(xlim = c(0,1), ylim = c(0, 0.5)) +
        labs(y="Density", x = xlab)  
      
    }else{
      
      if(sd > 0){
        
        if(betaMeanVarCond){
          
          eta <- p*(1-p)/sd^2 - 1
          alpha <- eta*p
          beta <- eta*(1-p)
          
          # map x-axis limits to a discrete sequence of values, to
          # help visualize shifts in chosen input parameter
          distTails <- qbeta(c(0.00001, 0.999999), shape1 = alpha, shape2 = beta)
          xUpLimBands <- c(0.3, 0.6, 1)
          xaxisUpLim <- xUpLimBands[findInterval(distTails[2], xUpLimBands) + 1]
          xLowLimBands <- c(0, 0.5, 0.8, 1)
          xaxisLowLim <- xLowLimBands[findInterval(distTails[1], xLowLimBands)]
          
          
          # set y-axis upper limit
          yaxisUpLimBands <- c(0.5, 2, 4, 6, 10, 15, 20)
          yaxisMax <- max(dbeta(seq(0, 1, by = 0.001), shape1 = alpha, shape2 = beta))
          
          if(yaxisMax > last(yaxisUpLimBands)){
            yaxisUpLim <- yaxisMax
          }else{
            yaxisUpLim <- yaxisUpLimBands[findInterval(yaxisMax, yaxisUpLimBands)+1]  
          }
          
          p1 <- data.frame(qtls = qbeta(c(0.0001, 0.9999), shape1 = alpha, shape2 = beta))  %>%
            ggplot(aes(qtls)) +
            stat_function(
              fun = dbeta, 
              args = list(shape1 = alpha, shape2 = beta), 
              geom = "area",
              fill = fill, 
              #size = 1,
              col = "black", 
              alpha = 0.5,
              outline.type = "full") +
            labs(y="Probability Density", x = xlab)
          
          if(is.infinite(yaxisUpLim)){
            p1
          }else{
            p1 + coord_cartesian(xlim = c(xaxisLowLim, xaxisUpLim), ylim = c(0, yaxisUpLim))
          }
        }else{
          warning("Invalid sd value: the condition sd^2 < p*(1-p) is not satisfied.",
                  " No plot returned.")
          NULL
        }
      }else{
        warning("Invalid sd value: standad deviation must be >= 0.",
                " No plot returned.")
        NULL
      }
    }
  }else{
    warning("Invalid p value: probability value must lie within 0 and 1.", 
            " No plot returned")
    NULL
  }
}




#' Area chart 
#' 
#' @param data data frame to use for the plot
#' @param x column to be used as the independent variable
#' @param y column to be used as the dependent variable
#' @param xlab character string, the x-axis label
#' @param ylab character string, the y-axis label
#' @param point_col character string, the color name (or hex code) for points
#' @param line_col character string, the color name (or hex code) for lines
#' @param area_col character string, the color name (or hex code) of the are beneath the line
#'  
#' @import dplyr
#' @import ggplot2
#'  
#' @noRd
area_plot <- function(data, 
                       x, y, 
                       xlab = NULL, ylab = NULL, 
                       point_col = "black", line_col = "black", area_col = "blue"){
  
  data %>%
    ggplot(aes(x = {{x}}, y = {{y}})) +
    geom_line(col = line_col, size = 1) +
    geom_point(color = "black", size = 2, alpha = 0.8, shape = 21, fill = point_col) +
    geom_area( fill=area_col, alpha=0.4) +
    labs(x = xlab, y = ylab)
}




#' lollipop chart
#' 
#' @import dplyr
#' @import ggplot2
#' 
#' @noRd
lolli_plot <- function(data, 
                      x, y, 
                      xlab = NULL, 
                      ylab = NULL, 
                      title = NULL,
                      point_col = "black", 
                      line_col = "black", 
                      point_size = 4){
  
  data %>%
    ggplot(aes(x = {{x}}, y = {{y}})) +
    geom_segment(aes(x = {{x}}, xend = {{x}}, y = 0, yend = {{y}}),
                  color = line_col, size = 1) +
    geom_point(color = "black", size = point_size, alpha = 0.8, shape = 21, fill = point_col) +
    labs(x = xlab, y = ylab, title = title)
}




#' Point-range plot summarizing FHD bootstrap data
#' 
#' @param data a `data.frame` with bootstrap replicates of a species' FHD
#' @param height `<data-masking>` the column containing height intervals above sea level
#' @param spp_label a character string, providing the species name/label
#' 
#' @return a ggplot object, a point-range plot with median and 95% CI of the
#'   proportion of bird flights at height intervals
#' 
#' @noRd
fhd_boots_pntrng_plot <- function(data, height, spp_label, col = "darkorange"){
  
  data %>%
    tidyr::pivot_longer(
      cols = -c({{height}}), 
      names_to = "bootId", 
      values_to = "prop") %>%
    dplyr::group_by({{height}}) %>%
    dplyr::filter(!is.na(prop)) %>%
    dplyr::summarise(
      perc2.5 = quantile(prop, probs = 0.025),
      perc50 = quantile(prop, probs = 0.5),
      perc97.5 = quantile(prop, probs = 0.975)) %>%
    #dplyr::filter(perc97.5 > 0.0001) %>%
    dplyr::filter({{height}} < 200) %>%
    ggplot2::ggplot(aes(y = perc50, x = {{height}})) +
    ggplot2::geom_pointrange(aes(ymin = perc2.5, ymax = perc97.5), col = col, size = 0.3) +
    ggplot2::labs(
      title = paste0("Proportion of ", spp_label, " flying at 1m height bands"),
      subtitle = "Estimates derived from bootstrap replicates (only 0-200m displayed)", 
      y = "Proportion (median and 95% CI)", 
      x = "Height above sea level (m)") +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12)
    )
}


#' Lolli plot for FHD estimates
#' @noRd
fhd_lolli_plt <- function(data, 
                          height, 
                          prop, 
                          spp_label, 
                          point_col = "orange", 
                          line_col="gray"){
  
  data %>%
    dplyr::filter({{height}} < 200) %>%
    lolli_plot(
      x = {{height}},
      y = {{prop}},
      xlab = "Height above sea level (m)",
      ylab = "Proportion", 
      title = paste0("Proportion of ", spp_label, " flying at 1m height bands"),
      point_col = point_col,
      line_col = line_col,
      point_size = 2) +
    ggplot2::theme(
      plot.subtitle = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12)
    )
}



#' Heatmap plot for FHD bootstrap data
#' 
#' @param data a `data.frame` with bootstrap replicates of a species' FHD
#' @param height `<data-masking>` the column containing height intervals above sea level
#' @param spp_label a character string, providing the species name/label
#' 
#' @return a ggplot tile plot with bootstrap replicates of the proportion of
#'   bird flights at height intervals
#' 
#' @noRd
fhd_boots_heatmap <- function(data, height, spp_label){

  #pal <- MetBrewer::met.brewer("Veronese", n = 100, direction = -1)
  pal <- MetBrewer::met.brewer("Tiepolo", n = 100, direction = -1)
  
  data <- data %>%
    rename(height = {{ height }}) %>%
    tidyr::pivot_longer(
      cols = -c(height), 
      names_to = "boot_id", 
      values_to = "prop")
  
  height_max <- 100
  boot_max <- 200
  height_breaks <- seq(0, height_max, by = 10)
  boot_breaks <- seq(0, boot_max, by = 25)
  
  p1 <- data %>%
    dplyr::mutate(
      boot_id = as.numeric(stringr::str_extract(boot_id, pattern = "\\d+"))
    ) %>%
    dplyr::filter(
      height < height_max,
      boot_id < boot_max,
      !is.na(prop)
    ) %>%
    dplyr::arrange(height) %>%
    dplyr::mutate(
      height = factor(height),
      boot_id = factor(boot_id)
      ) %>%
    ggplot2::ggplot(ggplot2::aes(x = boot_id, y = height, fill = prop)) +
    ggplot2::geom_tile() +
    #ggplot2::coord_fixed() +
    ggplot2::scale_fill_gradientn(colours = pal) + 
    ggplot2::scale_y_discrete(breaks = height_breaks) + 
    ggplot2::scale_x_discrete(breaks = boot_breaks) +
    ggplot2::labs(
      y = "Height above sea level (m)",
      x = "Bootstrap ID"
    ) +
    ggplot2::theme(
      legend.position="bottom",
      plot.margin = ggplot2::margin(t = 5, r = 1, l = 0, b = 5)
      ) +
    ggplot2::guides(
      fill = ggplot2::guide_colourbar(
        title = "Proportion",
        barwidth = 20,
        barheight = 1,
        title.vjust = .9
      )
    ) 
   
  
  p2 <- data %>%
    dplyr::group_by(height) %>%
    dplyr::filter(!is.na(prop)) %>%
    dplyr::summarise(
      lcl = quantile(prop, probs = 0.025),
      med = quantile(prop, probs = 0.5),
      ucl = quantile(prop, probs = 0.975)) %>%
    dplyr::filter(height < height_max) %>%
    ggplot2::ggplot(ggplot2::aes(x = height)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lcl, ymax = ucl), alpha = 0.3, fill = "orange") +
    ggplot2::geom_line(ggplot2::aes(y = med), col = "darkorange") +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 5, r = 0, l = 0, b = 5)
    ) +
    ggplot2::scale_y_continuous(n.breaks = 3) +
    #ggplot2::labs(y = "Proportion") +
    ggplot2::labs(y = "Bootstrap median\nand 95% CIs") +
    ggplot2::coord_flip()
  
  
  patchwork::wrap_plots(p1, p2) +
    patchwork::plot_layout(widths = c(5,1)) +
    patchwork::plot_annotation(
      title = paste0(
        "Bootstraps of proportion of ", spp_label, 
        " flying at 1m height bands (only 0-100m displayed)"
      )
    )
}



#' Plot percentile estimates of monthly bird densities
#' 
#' @param data a `data.frame` with percentile estimates of bird densities per
#'   monthly. It must contain a column specifying the percentiles and assign to
#'   argument `pctl`. Each od remaining columns comprise percentile values for a
#'   given month.
#' @param pctl `<data-masking>` the column containing the percentiles, with
#'   values ranging between 0 and 100
#'  
#' @noRd
densbird_pctl_plot <- function(data, pctl, spp_label){
  
  dt <- data %>%
    dplyr::rename(pctl = {{ pctl }}) %>%
    dplyr::mutate(pctl = dplyr::case_when(
      pctl == 1 ~ "1st",
      pctl == 2 ~ "2nd",
      pctl == 3 ~ "3nd",
      TRUE ~ paste0(pctl, "th")
    )) %>%
    dplyr::mutate(dplyr::across(everything(), .fns = dplyr::lead, .names= "{.col}_end")) %>%
    dplyr::slice(1:(n()-1)) %>%
    tidyr::drop_na() %>%
    tidyr::pivot_longer(-c(pctl, pctl_end), names_to = "month", values_to = "dens") %>%
    dplyr::mutate(
      pos = if_else(stringr::str_detect(month, "end"), "end", "start"), 
      month = stringr::str_replace(month, pattern = "_end", replacement = "")
    ) %>%
    tidyr::pivot_wider(names_from = pos, values_from = dens) %>%
    dplyr::mutate( 
      month = factor(month, levels = month.name),
      #across(c(pctl, pctl_end), ~./100)
    ) %>%
    tidyr::unite(col = "pctl_range", c(pctl, pctl_end), sep = " to ") %>%
    dplyr::mutate(
      pctl_range = factor(pctl_range, levels = unique(pctl_range)),
      c = (end+start)/2,
      h = end-start
    )
  
  n_pctls <- length(unique(dt$pctl_range))
  col_pal <- MetBrewer::met.brewer("Peru2", n = n_pctls)
  
  dt %>%
    ggplot(aes(x = month, y = c)) +
    geom_tile(aes(width = 0.2, height = h, fill = pctl_range), col = "black") +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        title = "Percentile interval", 
        reverse = TRUE)
    ) +
    ggplot2::scale_fill_manual(values = col_pal) +
    ggplot2::labs(
      x = "",
      y = bquote('Number of birds per' ~km^2),
      title = paste0("Percentile estimates of monthly density of ", spp_label)
    )
}

# densbird_pctl_plot <- function(data, pctl, spp_label){
#   
#   dt <- data %>%
#     dplyr::rename(pctl = {{ pctl }}) %>%
#     dplyr::arrange(pctl) %>%
#     tidyr::pivot_longer(
#       cols = -pctl, 
#       names_to = "month", 
#       values_to = "dens") %>%
#     dplyr::mutate( month = factor(month, levels = month.name))
#   
#   edge_pctls <- dt %>%
#     dplyr::filter(pctl %in% c(min(pctl), max( pctl))) %>%
#     tidyr::pivot_wider(names_from = pctl, id_cols = month, values_from = dens)
#   
#   names(edge_pctls) <- c("month", "min_pctl", "max_pctl")
#   
#   pctiles <- unique(dt$pctl)
#   col_pal <- MetBrewer::met.brewer("Peru2", n = length(pctiles))
#   
#   dt %>%
#     dplyr::mutate(
#       shape_size = ifelse(pctl <= 50, pctl, 100 - pctl),
#       pctl = factor(pctl)
#     ) %>%
#     ggplot2::ggplot(aes(x = month, y = dens)) +
#     ggplot2::geom_segment(
#       aes(x = month, xend = month, y = min_pctl, yend = max_pctl), 
#       linetype = "dashed", 
#       col = "gray45", data = edge_pctls) +
#     #ggplot2::geom_point(aes(fill = pctl, size = shape_size), shape = 22) +
#     #ggplot2::geom_point(aes(fill = pctl, size = as.numeric(pctl)), shape = 22) +
#     ggplot2::geom_point(aes(fill = pctl), shape = 22, size = 4) +
#     ggplot2::guides(
#       fill = ggplot2::guide_legend(
#         title = "Percentile", 
#         reverse = TRUE, 
#         override.aes = list(size = 6)
#       ),
#       size = "none"
#     ) +
#     ggplot2::scale_fill_manual(values = col_pal) +
#     #ggplot2::scale_size(range = c(2, 7)) +
#     ggplot2::labs(
#       x = "",
#       y = bquote('Number of birds per' ~km^2),
#       title = paste0("Percentile estimates of monthly density of ", spp_label)
#     )
#   
# }









#' Ridge plot for random draws of monthly bird densities
#' 
#' @param data a `data.frame`, each column containing random draws of bird
#'   densities in a given month. Columns should be named as English month names
#' @param fill a character string, the histograms filling color
#' 
#' @noRd
# densbird_draws_plot <- function(data, spp_label, fill = "#d39a2d"){
#   
#   data %>%
#     tidyr::pivot_longer(
#       cols = everything(), 
#       names_to = "month", 
#       values_to = "dens") %>%
#     dplyr::mutate( month = factor(month, levels = month.name)) %>%
#     ggplot2::ggplot(aes(x = dens, y = month, height = stat(density))) + 
#     ggridges::geom_density_ridges(
#       fill = fill,
#       alpha = 0.5,
#       stat = "binline", 
#       bins = 50, 
#       scale = 1,#0.95,   
#       draw_baseline = TRUE
#     ) +
#     #ggridges::theme_ridges(center_axis_labels = TRUE) +
#     #ggridges::theme_ridges(grid = FALSE, center_axis_labels = TRUE) +
#     ggplot2::coord_flip() +
#     labs(
#       x = bquote('Number of birds per' ~km^2),
#       y = "", 
#       title = paste0("Histograms for randm draws of monthly density of ", spp_label)
#     )
# }


densbird_draws_plot <- function(data, spp_label, fill = "#d39a2d"){
  
  data %>%
    tidyr::pivot_longer(
      cols = everything(), 
      names_to = "month", 
      values_to = "dens") %>%
    dplyr::mutate( month = factor(month, levels = month.name)) %>%
    ggplot2::ggplot(aes(x = month, y = dens)) +
    ggdist::stat_histinterval(
      slab_fill = fill, 
      slab_alpha = 0.5,
      slab_colour = "black",
      slab_size = 0.5,
      shape = 21,
      point_fill = "white",
      stroke = 1.5,
      outline_bars = TRUE) +
    labs(
      x = bquote('Number of birds per' ~km^2),
      y = "", 
      title = paste0("Histograms for randm draws of monthly density of ", spp_label)
    )
}

