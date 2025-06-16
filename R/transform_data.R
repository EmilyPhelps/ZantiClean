## Functions to get behavioural traits from Zantiks data
#' transform_csv()
#'
#' This function takes the wide dataframe and makes it longer so each row
#' then reflects what is occuring in one arena at that time point.
#'
#' @param data Dataframe to transform
#' @param ID a logical vector. If true, ID will be retained in the output
#' @return An object containing a longer transposed version of the data frame
#' @export
transform_csv <- function(data, ID){
  if(missing(ID)){
    ID <- FALSE
  }

  if (ID == TRUE){

    output <- data %>% dplyr::select(!contains("TOTAL_DISTANCE")) %>%
      melt(., id=c("TIME", "TIME_BIN", "file", "file.timestamp", "file.date", "ID", "unit")) %>%
      mutate(arena=substr(variable, 1, 2),
             zone=substr(variable, 4, 5),
             type=substr(variable, 7,7)) %>%
      dplyr::select(TIME, TIME_BIN, arena, zone, type, value, file.timestamp, file.date, file, ID, unit) %>%
      distinct() %>%
      pivot_wider(names_from = zone, values_from = value) %>%
      mutate(total_distance=ifelse(type == "D", rowSums(across(contains("Z"))), NA))

 } else {

   output<- data %>% dplyr::select(!contains("TOTAL_DISTANCE")) %>%
      melt(., id=c("TIME", "TIME_BIN", "file","file.timestamp", "file.date", "unit")) %>%
      mutate(arena=substr(variable, 1, 2),
             zone=substr(variable, 4, 5),
             type=substr(variable, 7,7)) %>%
      dplyr::select(TIME, TIME_BIN, arena, zone, type, value, file.timestamp, file.date, file, unit) %>%
      distinct() %>%
      pivot_wider(names_from = zone, values_from = value) %>%
      mutate(total_distance=ifelse(type == "D", rowSums(across(contains("Z"))), NA))

 }
  return(output)
}
#'transform_xy()
#'
#'This function takes the wide xy coordinate file and makes it longer so
#'each row reflects what is occurring in one arena.
#'
#' @param data Dataframe to transform
#' @return An object containing a longer transposed version of the data frame
#' @export
transform_xy <- function(data){
  X <- data %>%
    dplyr::select(RUNTIME, file.timestamp, unit, contains("X")) %>%
    pivot_longer(cols=contains("X"), values_to="X", names_to="arena") %>%
    mutate(arena=gsub("X_", "", arena))

  Y <- data %>%
    dplyr::select(RUNTIME, file.timestamp, unit, contains("Y")) %>%
    pivot_longer(cols=contains("Y"), values_to="Y", names_to="arena") %>%
    mutate(arena=gsub("Y_", "", arena))

  output<- left_join(X, Y)
  return(output)
}

