#Area measure development
#need to read in XY coords
test <- read_zancoord("../data/cherryshrimp2024/coord.csv/", 
                      "LightDarktestCS_final-20241104T143100-XY_data.csv") %>%
        transform_xy()
xy <- test 


#arena= 
calc_area <- function(xy, arena.df){
  width <- arena.df[1,]$xmax- arena.df[1,]$xmin
  height <- arena.df[1,]$ymax-arena.df[1,]$ymin
  
  rel.xy <- left_join(xy, arena.df) %>% #Calculate the relative coordinates due to arena structure
    mutate(rel.X=X-xmin, rel.Y=Y-ymin)
  
  left_join(xy, arena.df,by=c("arena"))
  df_roundwalk <- data_frame(Move = 1:nrow(rel.xy),
                             arena = rel.xy$arena,
                             file.timestamp=rel.xy$file.timestamp,
                             x_round = floor(rel.xy$rel.X),
                             y_round = floor(rel.xy$rel.Y)) |>  
    group_by(arena, file.timestamp, x_round, y_round) |> 
    summarise(score = n())
  
  area_cov <- df_roundwalk %>%
                left_join(., arena.df) %>%
                group_by(arena, file.timestamp) %>%
                summarize(area=round((n()/(width*height))*100,1))
  
  return(area_cov)
}


area_cov
