## Functions to get quick plots
#' track_plot()
#'
#' This function uses xy data to create a plot of the track traveled by
#' the individual.
#'
#' @param XY coord data from a single individual
#' @param zones A dataframe with the number of zones and the coordinates of the zones.
#'              e.g. zone, xmin, xmax, ymin, ymax, group
#'              Group could be light/dark or inner and outer in nested
#' @param nested Logical vector is Zone1 within Zone2?
#' @return A plot showing the complete path travelled by individuals
#' @export
track_plot <- function(XY, zones, nested){
  if(missing(nested)){
    nested <- FALSE
  }

  if(isFALSE(nested)){
  plot <- ggplot() + geom_rect(data=zones, aes(xmin=xmin, xmax=xmax,
                                       ymin=ymin, ymax=ymax, fill=group))+
    geom_path(data=XY, aes(x=X, y=Y), colour="#141F52", alpha=0.5) +
    scale_fill_manual(values=c("#E1DFD0", "#F5F4EF")) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme_void()+
    theme(legend.position = "none",
          panel.border = element_rect(colour = "#333333", fill=NA, linewidth=1))
  } else {
    inner <- zones %>% filter(group == "inner")
    outer <- zones %>% filter(group == "outer")
    ggplot() +
      geom_rect(data=outer, aes(xmin=xmin, xmax=xmax,
                               ymin=ymin, ymax=ymax, colour="#F5F4EF"))+
      geom_rect(data=inner, aes(xmin=xmin, xmax=xmax,
                                         ymin=ymin, ymax=ymax, colour="#E1DFD0"))+
      geom_path(data=XY, aes(x=X, y=Y), colour="#141F52", alpha=0.5) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme_void()+
      theme(legend.position = "none",
            panel.border = element_rect(colour = "#333333", fill=NA, linewidth=1))
  }
return(plot)}

## Functions to get quick plots
#' density_plot()
#'
#' This function uses xy data to create a plot showing how used each area of 
#' the area was used by an individual.
#'
#' @param XY coord data from a single individual
#' @param track Logical vector to show the track on the plot or not
#' @return A plot showing the regions with the highest density
#' @export
density_plot <- function(XY, track){
  if(missing(track)){
    track <- TRUE
  }
  
  if(isFALSE(track)){
    plot <- ggplot(XY, aes(x=X, y=Y)) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
      scale_fill_gradient(high="#3ce3df", low="#2683b5") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_void() +
      theme(legend.position="none")
  } else{
    plot <- ggplot(XY, aes(x=X, y=Y)) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
      geom_path(colour="white", alpha=0.6, linewidth=0.2) +
      scale_fill_gradient(high="#3ce3df", low="#2683b5") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_void() +
      theme(legend.position="none")
  }
return(plot)}
