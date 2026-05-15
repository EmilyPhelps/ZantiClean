## Functions for getting behavioural traits from Zantiks data.
#' stopping_duration()
#'
#'This function estimates the time stopped from the Zantiks csv files.
#' @param xy Zantiks transformed coordinate data (XY.csv)
#' @param thres (optional) The speed under which the animal is assumed to be stopped. 
#'              This is needed for animals that are swimming, so if you are doing a land animal set to 0.
#'              Default is 4mm/s based on the guppy threshold. 
#' @returns A dataframe containing duration of each stopping event.
stopping_duration <- function(xy, thres){
  if(missing(thres)){
    thres <- 4
    print("default speed set to 4mm/s- the threshold can be set to if animal is not likely to drift/float")
  }
  
  #Estimate speed between two points
  zero_grouped <- xy %>%
    group_by(file.timestamp, arena) %>%
    mutate(x_move = X - lag(X), #Estimate the distance using Pythagoras
           y_move = Y - lag(Y),
           distance = sqrt(x_move^2 + y_move^2),
           time=RUNTIME- lag(RUNTIME), #Estimate the time
           speed=distance/time,
           # Assign group number only to rows where speed < thres
           below_thres = speed <= thres & !is.na(speed),  # TRUE/FALSE flag
           zero_group = cumsum(below_thres & !lag(below_thres, default = FALSE)),  # increment at each new run start
           zero_group = ifelse(below_thres, zero_group, NA))
  
  output <- zero_grouped %>%
    group_by(file.timestamp, arena, zero_group) %>% 
    summarize(duration=(max(RUNTIME)-min(RUNTIME))) %>%
    ungroup()
  
  return(output)
}

#' freezings()
#' This function calculates the number of freezing events and the average time spent frozen.
#' 
#' @param xy Zantiks transformed coordinate data (XY.csv)
#' @param thres (optional) The speed under which the animal is assumed to be stopped. 
#'              This is needed for animals that are swimming, so if you are doing a land animal set to 0.
#'              Default is 4mm/s based on the guppy threshold. 
#' @returns A dataframe containing the number of freezing events, the mean duration of the freezing events, 
#'          the sd of the freezing events and the total duration spent frozen.

freezings <- function(xy, thres){
  if(missing(thres)){
    thres <- 4
    print("default speed set to 4mm/s- the threshold can be set to if animal is not likely to drift/float")
  }
  stops.df <- stopping_duration(xy, thres) %>%
    group_by(file.timestamp, arena) %>% summarise(
      n_freezing = n_distinct(zero_group, na.rm = TRUE),
      mean_freezetime = mean(duration, na.rm=TRUE),
      sd_freezetime=sd(duration, na.rm=TRUE),
      .groups = "drop"
    )
  
  return(stops.df)
}

#' calc_area()
#'
#' This function estimates the proportion of the arena that covered
#' using the xy coordinates provided by Zantiks. Either a dataframe containing
#' arena information should be included or individual height/width of the arena. 
#'
#' @param xy Transformed Zantiks coordinates.
#' @param arena.df (optional) A small dataframe containing the coordinates of the arenas within the Zantiks tank/enclosure.
#'         This should include xmin, xmax, ymin, ymax for each arena.
#' @param width (optional) The width of the trial arena. 
#' @param height (optional) The height of the trial arena.
#' @return A dataframe containing area measurements for each individual.
#' @export
#' 
calc_area <- function(xy, arena.df, width, height){
  if(missing(arena.df) & (missing(width) | missing(height))){
    print("Need to include either arena.df or height and width of trial arena")
  }
  
  if(missing(arena.df)){
  print("Arena info missing so using explicit width and height")
    width <- width
    height <- height
    df_roundwalk <- data_frame(Move = 1:nrow(xy),
               arena = xy$arena,
               file.timestamp=xy$file.timestamp,
               x_round = floor(xy$X),
               y_round = floor(xy$Y)) |>  
      group_by(arena, file.timestamp, x_round, y_round) %>%
      summarise(score = n())
    
    area_cov <- df_roundwalk %>%
      group_by(arena, file.timestamp) %>%
      summarize(area=round((n()/(width*height))*100,1))
    
  }else{
  print("Arena info input so estimating width. Give explicit width and remove arena.df if you dont want this")
  width <- arena.df[1,]$xmax- arena.df[1,]$xmin
  height <- arena.df[1,]$ymax-arena.df[1,]$ymin
  
  rel.xy <- left_join(xy, arena.df) %>% #Calculate the relative coordinates due to arena structure
    mutate(rel.X=X-xmin, rel.Y=Y-ymin)
  
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
  }
  return(area_cov)
}
#' summary_behaviour()
#'
#' This function estimates the summary behaviour data from the
#' Zantiks csv. The variables included are time in each zone,
#' overall velocity, track length, freezings (calculated as no
#' movement for three seconds)
#'
#' @param data Zantiks transformed csv
#' @param xy Zantiks transformed xy coords
#' @param arena.df A small dataframe containing the coordinates of the arenas within the Zantiks tank/enclosure.
#'         This should include xmin, xmax, ymin, ymax for each arena.
#' @param ID (optional) A logical vector. If true, the ID will be carried forward.
#' @param thres (optional) A threshold speed (mm/s) value after which individuals should be considered to be exhibiting a freeze response. 
#' @return A dataframe containing summary behavioural variables
#' @export

summary_behaviour <- function(data, xy, arena.df, ID, thres){
  if(missing(ID)){
    ID <- FALSE
  }
  
  if(missing(thres)){
    thres <- 4
    print("default speed set to 4mm/s- the threshold can be set to if animal is not likely to drift/float")
  }
  
  free <- freezings(xy, thres = thres) #Calculate the freezing information
  
  ts <- data %>% dplyr::select(file.timestamp) %>% distinct() 
  
  area <- calc_area(xy, arena.df) %>%
    dplyr::rename(xy.timestamp=file.timestamp) %>%
    rowwise() %>%
    mutate(xy.time = ymd_hms(xy.timestamp, tz = "UTC"),  # parse as datetime
           plus = xy.time + seconds(10),
           minus = xy.time - seconds(10),
           file.timestamp = as.character({
             ts_times <- ymd_hms(ts$file.timestamp, tz = "UTC")       # Convert all ts timestamps to datetime
             match_vals <- ts$file.timestamp[ts_times >= minus & ts_times <= plus]
             if (length(match_vals) > 0) match_vals[1] else NA_character_})) %>% #Keep as original
    dplyr::select(!c(minus, plus, xy.time, xy.timestamp))
  
  if (ID == TRUE) {
    dis <- data %>%
      filter(type == "D") %>%
      group_by(file.timestamp, arena, ID, unit) %>%
      mutate(time=max(TIME_BIN)) %>%
      reframe(track_length=sum(total_distance),
              velocity= sum(total_distance)/time) %>%
      distinct()
    
    
    tim <- data %>%
      filter(type == "T") %>%
      pivot_longer(cols=contains("Z"), names_to="Zone", values_to = "TIZ") %>%
      group_by(Zone, file.timestamp, arena, ID, unit) %>%
      summarise(Time.in.Zone=sum(TIZ)) %>%
      mutate(Zone=paste0("time_", Zone)) %>%
      pivot_wider(names_from=Zone, values_from = "Time.in.Zone") %>%
      ungroup()
    
  } else {
    dis <- data %>%
      filter(type == "D") %>%
      group_by(file.timestamp, arena, unit) %>%
      mutate(time=max(TIME_BIN)) %>%
      reframe(track_length=sum(total_distance),
              velocity= sum(total_distance)/time) %>%
      distinct
    
    time <- max(data$TIME_BIN)
    
    tim <- data %>%
      filter(type == "T") %>%
      pivot_longer(cols=contains("Z"), names_to="Zone", values_to = "TIZ") %>%
      group_by(Zone, file.timestamp, arena, unit) %>%
      summarise(Time.in.Zone=sum(TIZ)) %>%
      mutate(Zone=paste0("time_", Zone),) %>%
      pivot_wider(names_from=Zone, values_from = "Time.in.Zone") %>%
      ungroup()
    
  }
  output <- left_join(dis, tim) %>%
    left_join(., free) %>%
    left_join(., area)
  return(output)
}

#' split_behaviour()
#'
#' This function estimates the behaviour variables accounting
#' for differences in assay that can be divided by time. E.g.
#' light for the first X seconds, dark for next X seconds.
#'
#' @param data Zantiks transformed csv.
#' @param time Time in seconds, in which to divide data by.
#' @param ID (optional) A logical vector. If true, the ID will be carried forward.
#' @param thres (optional) A threshold speed (mm/s) value after which individuals should be considered to be exhibiting a freeze response. 
#' @return A dataframe containing summary behavioral variables
#' @export
split_behaviour <- function(data, time, ID, thres){
  if(missing(ID)){
    ID <- FALSE
  }
  
  if(missing(thres)){
   thres <- 3
  }
  
  if (ID == TRUE) {
    pre <- data %>%
      filter(TIME_BIN<=time) %>%
      summary_behaviour(., ID=TRUE, thres=thres) %>%
      mutate(timesplit=paste0("pre", time)) %>%
      rename_with(~paste0("pre", time, .),
                  !c(file.timestamp, arena, ID, unit))

    post <- data %>%
      filter(TIME_BIN>=time) %>%
      summary_behaviour(., ID=TRUE, frz=frz) %>%
      mutate(timesplit=paste0("post", time)) %>%
      rename_with(~paste0("post", time, .),
                  !c(file.timestamp, arena, ID, unit))
  } else {
  pre <- data %>%
    filter(TIME_BIN<=time) %>%
    summary_behaviour(., frz=frz) %>%
    mutate(timesplit=paste0("pre", time)) %>%
    rename_with(~paste0("post", time, .),
                !c(file.timestamp, arena, unit))

  post <- data %>%
    filter(TIME_BIN>=time) %>%
    summary_behaviour(., frz=frz) %>%
    mutate(timesplit=paste0("post", time))%>%
    rename_with(~paste0("post", time, .),
                !c(file.timestamp, arena, unit))

  }
  output <- rbind(pre, post)
  return(output)
}

