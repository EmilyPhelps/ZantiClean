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
#' @return An object containing a longer transposed version of the data frame
#' @export
track_plot <- function(XY, zones){
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
