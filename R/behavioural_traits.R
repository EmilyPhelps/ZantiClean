## Functions to get behavioural traits from zanticks data
#' transform_zan()
#'
#' This function loads in a single .csv file output from Zanticks.
#' It identifies the header line and only reads in from there
#' skipping the preamble.
#'
#' @param data Dataframe to transform
#' @param ID a locgical vector. If true, ID will be retained in the output
#' @return An object containing a longer transposed version of the data frame, ready for behavioural traits
#' @export
transform_zan <- function(data, ID){
  if(missing(ID)){
    ID <- FALSE
  }

  if (ID == TRUE){

    output <- data %>% dplyr::select(!contains("TOTAL_DISTANCE")) %>%
      melt(., id=c("TIME", "TIME_BIN", "file", "ID")) %>%
      mutate(arena=substr(variable, 1, 2),
             zone=substr(variable, 4, 5),
             type=substr(variable, 7,7)) %>%
      dplyr::select(TIME, TIME_BIN, arena, zone, type, value, file, ID) %>%
      distinct() %>%
      pivot_wider(names_from = zone, values_from = value) %>%
      mutate(total_distance=ifelse(type == "D", rowSums(across(contains("Z"))), NA))

 } else {

   output<- data %>% dplyr::select(!contains("TOTAL_DISTANCE")) %>%
      melt(., id=c("TIME", "TIME_BIN", "file")) %>%
      mutate(arena=substr(variable, 1, 2),
             zone=substr(variable, 4, 5),
             type=substr(variable, 7,7)) %>%
      dplyr::select(TIME, TIME_BIN, arena, zone, type, value, file) %>%
      distinct() %>%
      pivot_wider(names_from = zone, values_from = value) %>%
      mutate(total_distance=ifelse(type == "D", rowSums(across(contains("Z"))), NA))

 }
  return(output)
}
#' split_calcs()
#'
#' This function splits your data by the experimental design.
#' e.g. light-dark cycles
#'
#' @param data Dataframe to work with
#' @return An object containing your divied up data.
#' @export

split_calcs <- function(data)
