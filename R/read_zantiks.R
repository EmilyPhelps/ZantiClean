## Functions for reading zantiks files into R.
#' read_zancsv()
#'
#' This function loads in a single .csv file output from Zantiks.
#' It identifies the header line and only reads in from there
#' skipping the preamble.
#'
#' @param file Path to the file to be imported
#' @param ID (optional) The location to look at to find identifying information.
#'           Either "Source" or "Subject".
#' @return A dataframe containing the csv data
#' @export
read_zancsv <- function(file, ID){
  lines <- readLines(file)
  start_line <- which(grepl("TIME", lines))[1]
  date <- strsplit(lines[1], ",")[[1]][3] %>%
          gsub("\"", "", .)
  zunit <- lines[grepl("Apparatus", lines)] %>% 
              gsub('.*\"Apparatus\",\"', '', .)  %>%
              gsub('[\\",]', '', .)
  data <- read.csv(file, skip = start_line - 1)
  data <- data[-nrow(data), ] %>%
            mutate_all(as.numeric) %>%
            mutate(file=paste0(file),
                   file.date=as.Date(date),
                   file.timestamp=str_replace_all(date, "[^[:alnum:]]", ""),
                   Z_unit=zunit)

  if (!missing(ID)) {  # Only execute this block if ID is not missing
    if (ID == "Service") {
    id <- lines[grepl("Service", lines)][1] %>%
      sub(".*'(.*?)'.*", "\\1", .)
    } else if (ID == "Subject") {
    id <- lines[grepl("Subject", lines)][1] %>%
      gsub('.*\"Subject Identification\",\"', '', .)  %>%
      gsub('[\\",]', '', .)
    }
    data <- mutate(data, ID = paste0(id))  # Add ID column to data
  }
  rm(ID)
  return(data)

}
#' read_manyzancsv()
#' 
#' This function loads in all Zanticks output .csv files stored in a single directory.
#' It identifies the header line and only reads in from there
#' skipping the preamble.
#'
#' @param dir Path to the directory where csv to be imported are stored
#' @return A dataframe containing the csv data
#' @param ID (optional) The location to look at to find identifying information.
#'           Either "Source" or "Subject".
#' @export
read_manyzancsv <- function(dir, ID){
  files <- list.files(dir, full.names=TRUE)
  data.list <- lapply(files, function(file){
    read_zancsv(file, ID=ID)
  })
  do.call(rbind, data.list)

}
#' read_zancoord()
#'
#' This function loads in a single .coord file output from Zanticks.
#'
#' @param file Name of file to the coordinate file to be imported
#' @param dir Directory containing the coordinate file to be imported
#' @return A dataframe containing the coord data
#' @export
read_zancoord <- function(dir, file){
  timestamp <- str_split(file, "-")[[1]][2] %>% gsub("T", "", .)

  data <- read_csv(paste0(dir, file)) %>%
    mutate(file.timestamp=timestamp)
  return(data)

}
#' read_manyzancoord()
#'
#' This function loads in a many .coord output files from Zanticks.
#'
#' @param dir Directory containing the coordinate files to be imported
#' @return A dataframe containing the coord data
#' @export
read_manyzancoord <- function(dir){
  files <- list.files(dir, full.names=FALSE)
  data.list <- lapply(files, function(file){
    read_zancoord(dir, file)
  })
  do.call(rbind, data.list)

}


