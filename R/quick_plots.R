## Functions to get quick plots
#' track_plot()
#'
#' This function uses xy data to create a plot of the track traveled by
#' the individual.
#'
#' @param XY coord data from a single individual
#' @param zones A dataframe with the number of zones and the coordinates of the zones.
#'              e.g. zone, xmin, xmax, ymin, ymax, colour
#' @return An object containing a longer transposed version of the data frame
#' @export
track_plot <- function(XY, zones){
  plot <- ggplot() + geom_rect(data=zones, aes(xmin=xmin, xmax=xmax,
                                       ymin=ymin, ymax=ymax, fill=colour))+
    geom_path(data=XY, aes(x=X, y=Y), colour="#141F52", alpha=0.5) +
    scale_fill_manual(values=c("#E1DFD0", "#F5F4EF")) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme_void()+
    theme(legend.position = "none",
          panel.border = element_rect(colour = "#333333", fill=NA, linewidth=1))

return(plot)}

