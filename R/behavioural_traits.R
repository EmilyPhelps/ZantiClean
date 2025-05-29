## Functions for getting behavioural traits from Zantiks data.
#' stopping_duration()
#'
#'This function estimates the time stopped from the Zantiks csv files.
#' @param data Zantiks transformed csv
#' @param ID (optional) A logical vector. If true, the ID will be carried forward.
#' @returns A dataframe containing duration of each stopping event, e.g. when total distance is 0.
stopping_duration <- function(data, ID){
  if(missing(ID)){
    ID <- FALSE
  }

  if (ID == TRUE) {
    output <- data %>% 
      group_by(file.timestamp, arena, ID, unit) %>%
      mutate(
        # Assign group number only to rows where total_distance == 0
        zero_group = ifelse(total_distance == 0,
                            cumsum(c(TRUE, diff(total_distance == 0) != 0)) * (total_distance == 0),
                            NA)
      ) %>%
      filter(!is.na(zero_group)) %>%
      group_by(file.timestamp, arena, ID, zero_group, unit) %>%
      summarize(duration=(max(TIME_BIN)-min(TIME_BIN))+1) %>%
      ungroup()

  } else {
    output <- data %>% 
      group_by(file.timestamp, arena, unit) %>%
      mutate(
        # Assign group number only to rows where total_distance == 0
        zero_group = ifelse(total_distance == 0,
                            cumsum(c(TRUE, diff(total_distance == 0) != 0)) * (total_distance == 0),
                            NA)
      ) %>%
      filter(!is.na(zero_group)) %>%
      group_by(file.timestamp, arena, zero_group, unit) %>%
      summarize(duration=(max(TIME_BIN)-min(TIME_BIN))+1) %>%
      ungroup()
  }
  
  missing_arenas <- setdiff(unique(data$arena), unique(output$arena))
  
  if(length(missing_arenas) > 0) {
    for(arena in missing_arenas) {
      file.timestamp <- output[1,1]
      ID <- output[1,3]
      unit <- output[1,5]
      
      new_row <- data.frame(arena = arena, 
                            file.timestamp= file.timestamp, 
                            ID=ID, zero_group=NA,
                            unit=unit, duration = 0)
      # Append the new row to output
      output <- rbind(output, new_row)
    }
  }
  
  return(output)
}
#'freezings()
#'
#' This function estimates whether a stopping is actually a freezing event and
#' calculates the number of freezing events and the average time spent frozen.
#' @param data Zantiks transformed csv
#' @param ID (optional) A logical vector. If true, the ID will be carried forward.
#' @param frz The number of seconds after which an animal can be considered to give the freezing response.
#' @returns Summary of the freezing including total number of freezing and average number of freezing.
freezings <- function(data, ID, frz){
  if(missing(ID)){
    ID <- FALSE
  }
  
  if(missing(frz)){
    frz <- 3
  }

  if (ID == TRUE) {
    output <- stopping_duration(data, ID=TRUE) %>%
      ungroup() %>%
      group_by(file.timestamp, arena, ID, unit) %>%
      mutate(freeze.event=ifelse(duration >= frz, 1, 0)) %>% #if stopping time is over 3 seconds consider it a freezing event
      summarize(freeze.count=sum(freeze.event),
                freeze.time = mean(duration[freeze.event == 1], na.rm = TRUE)) %>%
      mutate(freeze.time=ifelse(is.na(freeze.time), 0, freeze.time))%>% #Replace the na of those with no freezing time with 0
      ungroup()

  } else {
    output <- stopping_duration(data) %>%
      ungroup() %>%
      group_by(file.timestamp, arena, unit) %>%
      mutate(freeze.event=ifelse(duration>= frz, 1, 0)) %>% #if stopping time is over 3 seconds consider it a freezing event
      summarize(freeze.count=sum(freeze.event),
                freeze.time = mean(duration[freeze.event == 1], na.rm = TRUE)) %>%
      mutate(freeze.time=ifelse(is.na(freeze.time), 0, freeze.time)) %>% #Replace the na of those with no freezing time with 0
      ungroup()
  }
  return(output)
}
#' calc_area()
#'
#' This function estimates the proportion of the arena that covered
#' using the xy coordinates provided by Zantiks. 
#'
#' @param xy Transformed Zantiks coordinates.
#' @param ID (optional) A logical vector. If true, the ID will be carried forward.
#' @param arena.df (optional) A small dataframe containing the coordinates of the arenas within the Zantiks tank/enclosure.
#'         This should include xmin, xmax, ymin, ymax for each arena.
#' @return A dataframe containing arena measurements for each individual.
#' @export
#' 
calc_area <- function(xy, arena.df){
  if(missing(arena.df)){
    area_cov <- data_frame(Move = 1:nrow(xy),
               x_round = floor(xy$X),
               y_round = floor(xy$Y)) |>  
      group_by(x_round, y_round) |> 
      group_by(x_round, y_round) %>%
      summarise(score = n(), .groups = "drop") %>%
      summarise(area = round((n() / (width * height)) * 100, 1))
    
    
  }else{
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
#' @param frz (optional) A threshold value after which individuals should be considered to be exhibiting a freeze response. 
#' @return A dataframe containing summary behavioural variables
#' @export

summary_behaviour <- function(data, xy, arena.df, ID, frz){
 if(missing(ID)){
    ID <- FALSE
 }

if(missing(frz)){
    frz <- 3
}
  
 if (ID == TRUE) {
   free <- freezings(data, ID=TRUE, frz=frz) #Calculate the freezing information
   
   area <- calc_area(xy, arena.df)
   
   dis <- data %>%
     filter(type == "D") %>%
     group_by(file.timestamp, arena, ID, unit) %>%
     mutate(time=max(TIME_BIN)) %>%
     reframe(track_length=sum(total_distance),
             velocity= sum(total_distance)/time) %>%
     distinct

   time <- max(data$TIME_BIN)

   tim <- data %>%
     filter(type == "T") %>%
     pivot_longer(cols=contains("Z"), names_to="Zone", values_to = "TIZ") %>%
     group_by(Zone, file.timestamp, arena, ID, unit) %>%
     summarise(Time.in.Zone=sum(TIZ)) %>%
     mutate(Zone=paste0("time_", Zone)) %>%
     pivot_wider(names_from=Zone, values_from = "Time.in.Zone") %>%
     ungroup()

   output <- left_join(dis, tim) %>%
     left_join(., free) %>%
     left_join(., area)

 } else {
 free <- freezings(data, frz=frz) #Calculate the freezing information
 
 area <- calc_area(xy, arena.df)
 
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
    mutate(Zone=paste0("time_", Zone)) %>%
    pivot_wider(names_from=Zone, values_from = "Time.in.Zone") %>%
    ungroup()

 output <- left_join(dis, tim) %>%
   left_join(., free) %>%
   left_join(., area)
 }
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
#' @return A dataframe containing summary behavioural variables
#' @export
split_behaviour <- function(data, time, ID, frz){
  if(missing(ID)){
    ID <- FALSE
  }
  
  if(missing(frz)){
    frz <- 3
  }
  
  if (ID == TRUE) {
    pre <- data %>%
      filter(TIME_BIN<=time) %>%
      summary_behaviour(., ID=TRUE, frz=frz) %>%
      mutate(timesplit=paste0("pre", time)) %>%
      rename_with(~paste0("pre", time, .),
                  c(track_length, velocity, freeze.count, freeze.time,
                    time_Z1, time_Z2, time_Z3, time_Z4))

    post <- data %>%
      filter(TIME_BIN>=time) %>%
      summary_behaviour(., ID=TRUE, frz=frz) %>%
      mutate(timesplit=paste0("post", time)) %>%
      rename_with(~paste0("post", time, .),
                  c(track_length, velocity, freeze.count, freeze.time,
                    time_Z1, time_Z2, time_Z3, time_Z4))
  } else {
  pre <- data %>%
    filter(TIME_BIN<=time) %>%
    summary_behaviour(., frz=frz) %>%
    mutate(timesplit=paste0("pre", time)) 

  post <- data %>%
    filter(TIME_BIN>=time) %>%
    summary_behaviour(., frz=frz) %>%
    mutate(timesplit=paste0("post", time))

  }
  output <- rbind(pre, post)
  return(output)
}

