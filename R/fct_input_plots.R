#' ------------------------------------------------------------ #
#' ----      Functions for building plot charts            ------
#' ------------------------------------------------------------ #



#' Normal density plots
#' 
normDens_ParsPlots <- function(mu, stdev, fill="olivedrab", xlab, refValue_E = mu, refValue_SD = stdev){
  
  req(mu, stdev)
  data.frame(qtls = qnorm(c(0.0001, 0.9999), mean = mu, sd=stdev))  %>%
    ggplot(aes(qtls)) +
    stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), col = "black", size =1) +
    stat_function(fun=dnorm, args = list(mean = mu, sd = stdev), geom="area", fill = fill, col = "black", alpha = 0.3) +
    labs(y="Density", x = xlab) + 
    #geom_vline(aes(xintercept = 0)) +
    coord_cartesian(xlim = qnorm(c(0.000001, 0.999999), mean = refValue_E, sd = refValue_SD))
}




#' Truncated Normal density plots
#'
#' @import shiny
#' @import ggplot2
#' @import dplyr
#' @importFrom msm dtnorm qtnorm
#' 
#' @return a ggplot object, a density plot
#' 
#' @noRd
tnorm_dplots <- function(mean, sd, lower = -Inf, upper = Inf, fill="olivedrab", xlab){
  
  req(mean, sd)
  
  theme_set(theme_bw())
  
  if(sd == 0){
    if(mean <= lower){
      NULL
    }else{
      data.frame(x = seq(mean-500, mean+500, by = 1), y = 0) %>%
        ggplot(aes(x=x, y=y)) +
        geom_path() +
        geom_segment(aes(x = mean, xend = mean, y = 0, yend = Inf), size = 1) +
        coord_cartesian(xlim = c(0, mean*1.65), ylim = c(0, 0.5)) +
        labs(y="Density", x = xlab)  
    }
  }else{
    if(sd>0){
      
      yaxisUpLimBands <- c(0.05, 0.1, 0.25, 0.5, 1, 2, 4, 6, 10, 15, 20)
      
      distTails <- qtnorm(c(0.00001, 0.999999), mean, sd, lower, upper)
      xaxisUpLim <- distTails[2]*1.1
      xaxisLowLim <- ifelse(distTails[1]*0.5 > mean/2.5, distTails[1]*0.7, 0)
      
      yaxisMax <- max(dtnorm(seq(mean-500, mean+500, by = 1), mean, sd, lower, upper))
      
      if(yaxisMax > last(yaxisUpLimBands)){
        yaxisUpLim <- yaxisMax
      }else{
        yaxisUpLim <- yaxisUpLimBands[findInterval(yaxisMax, yaxisUpLimBands)+1]  
      }
      
      data.frame(qtls = qtnorm(c(0.0001, 0.9999), mean, sd, lower, upper))  %>%
        ggplot(aes(qtls)) +
        # two layers required for having lines delimiting the curve in all its sides
        stat_function(fun=dtnorm,
                      args = list(mean, sd, lower, upper),
                      geom="area",
                      fill = fill,
                      col = "black",
                      #size = 1,
                      alpha = 0.3) +
        labs(y="Density", x = xlab) +
        coord_cartesian(xlim = c(xaxisLowLim, max(0, xaxisUpLim, na.rm=TRUE)), 
                        ylim = c(0, yaxisUpLim))
    }else{
      NULL
    }
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
                      xlab = NULL, ylab = NULL, 
                      point_col = "black", line_col = "black"){
  
  data %>%
    ggplot(aes(x = {{x}}, y = {{y}})) +
    geom_segment(aes(x = {{x}}, xend = {{x}}, y = 0, yend = {{y}}),
                  color = line_col, size = 1) +
    geom_point(color = "black", size = 4, alpha = 0.8, shape = 21, fill = point_col) +
    labs(x = xlab, y = ylab)
}

