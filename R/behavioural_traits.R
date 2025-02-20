## Functions for getting behavioural traits from Zanticks data.
#' summary_behaviour()
#'
#' This function estimates the summary behaviour data from the
#' zanticks csv. The variables included are time in each zone,
#' overall velocity, track length, freezings (calculated as no
#' movement for three seconds)
#'
#' @param data Zanticks transformed csv
#' @param ID a logical vector. If true, an ID will be assigned from the Service part of the file.
#' @return A dataframe containing summary behavioural variables
#' @export
summary_behaviour <- function(data, ID){
 if(missing(ID)){
    ID <- FALSE
 }

 if (ID == TRUE) {
   dis.traits <- data %>%
     filter(type == "D") %>%
     group_by(file.timestamp, arena, ID) %>%
     mutate(freezing_event = ifelse(total_distance == 0 &
                                      lag(total_distance, default = NA) == 0 &
                                      lead(total_distance, default = NA) == 0, 1, 0),
            time=max(TIME_BIN)) %>%
     summarise(track_length=sum(total_distance),
               velocity= sum(total_distance)/time,
               freezing=sum(freezing_event)) %>%
     ungroup()

   time.trait <- data %>%
     filter(type == "T") %>%
     pivot_longer(cols=contains("Z"), names_to="Zone", values_to = "TIZ") %>%
     group_by(Zone, file.timestamp, arena, ID) %>%
     summarise(Time.in.Zone=sum(TIZ)) %>%
     mutate(Zone=paste0("time_", Zone)) %>%
     pivot_wider(names_from=Zone, values_from = "Time.in.Zone") %>%
     ungroup()

   output <- left_join(dis.traits, time.trait)
   return(output)
 } else {
 time <- max(data$TIME_BIN)

 dis.traits <- data %>%
    filter(type == "D") %>%
    group_by(file.timestamp, arena) %>%
    mutate(freezing_event = ifelse(total_distance == 0 &
                                     lag(total_distance, default = NA) == 0 &
                                     lead(total_distance, default = NA) == 0, 1, 0),
           time=max(TIME_BIN)) %>%
    summarise(track_length=sum(total_distance),
              velocity= sum(total_distance)/time,
              freezing=sum(freezing_event)) %>%
    ungroup()

 time.trait <- data %>%
    filter(type == "T") %>%
    pivot_longer(cols=contains("Z"), names_to="Zone", values_to = "TIZ") %>%
    group_by(Zone, file.timestamp, arena) %>%
    summarise(Time.in.Zone=sum(TIZ)) %>%
    mutate(Zone=paste0("time_", Zone)) %>%
    pivot_wider(names_from=Zone, values_from = "Time.in.Zone") %>%
    ungroup()

 output <- left_join(dis.traits, time.trait)
 return(output)
 }
}
#' split_behaviour()
#'
#' This function estimates the behaviour variables accounting
#' for differences in assay that can be divided by time. E.g.
#' light for the first X seconds, dark for next X seconds.
#'
#' @param data Zanticks transformed csv.
#' @param time Time in seconds, in which to divide data by.
#' @return A dataframe containing summary behavioral variables
#' @export
split_behaviour <- function(data, time){
  pre <- csv %>%
    filter(TIME_BIN<=time) %>%
    summary_behaviour() %>%
    mutate(timesplit=paste0("pre", time)) %>%
    rename_with(~paste0("pre", time, .),
                c(track_length, velocity, freezing,
                  time_Z1, time_Z2, time_Z3, time_Z4))

  post <- csv %>%
    filter(TIME_BIN>=time) %>%
    summary_behaviour() %>%
    mutate(timesplit=paste0("post", time)) %>%
    rename_with(~paste0("post", time, .),
                c(track_length, velocity, freezing,
                  time_Z1, time_Z2, time_Z3, time_Z4))

  output <- left_join(pre, post)
  return(output)
}
