## Function to reallocate zones within data
#' reallocate_zones
#'
#' This function is used to change the zone allocation and recalculate time spent in each zone.
#'
#' @param XY coord data from a single individual
#' @param zones A dataframe with the number of zones and the coordinates of the zones.
#'              e.g. zone, xmin, xmax, ymin, ymax, colour
#' @return An object containing a longer transposed version of the data frame
#' @export

old_zones <- zones %>% dplyr::select(!colour)

#New zones
zone <- c("inner", "outer")

width <- 360
height <- 270

total_area <- width*height

inner_area <-total_area/2
margin_x <- sqrt(inner_area / height)  # margin in the x direction
margin_y <- sqrt(inner_area / width)

# New dimensions of the inner rectangle
inner_width <- width - 2 * margin_x
inner_height <- height - 2 * margin_y

# Print the results
cat("Outer rectangle dimensions: ", width, "x", height, "\n")
cat("Inner rectangle dimensions: ", inner_width, "x", inner_height, "\n")
cat("Margin (x direction):", margin_x, "\n")
cat("Margin (y direction):", margin_y, "\n")
ggplot() + geom_rect(data=zones, aes(xmin=xmin, xmax=xmax,
                                             ymin=ymin, ymax=ymax, fill=colour))+
  scale_fill_manual(values=c("#E1DFD0", "#F5F4EF")) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void()+
  theme(legend.position = "none",
        panel.border = element_rect(colour = "#333333", fill=NA, linewidth=1))

