## Functions for reading zanticks files into R.
#' read_zancsv()
#'
#' This function loads in a single .csv file output from Zanticks.
#' It identifies the header line and only reads in from there
#' skipping the preamble.
#'
#' @param file Path to the file to be imported
#' @param ID a locgical vector. If true, an ID will be assigned from the Service part of the file.
#' @return A dataframe containing the csv data
#' @export
read_zancsv <- function(file, ID){
  lines <- readLines(file)
  start_line <- which(grepl("TIME", lines))[1]
  data <- read.csv(file, skip = start_line - 1)

  if(missing(ID)){
    ID <- FALSE
  }

  if (ID == TRUE) {  # Only execute this block if ID is TRUE
    id <- lines[grepl("Service", lines)][1] %>%
      sub(".*'(.*?)'.*", "\\1", .)
    data <- mutate(data, ID = paste0(id))  # Add ID column to data
  }
  rm(ID)
  return(data)

}
#' read_
#' This function loads in all Zanticks output .csv files stored in a single directory.
#' It identifies the header line and only reads in from there
#' skipping the preamble.
#'
#' @param dir Path to the directory where csv to be imported are stored
#' @return A dataframe containing the csv data
#' @export

read_manyzancsv <- function(dir){
  files <- list.files(dir, full.names=TRUE)
  data.list <- lapply(files, function(file){
    read_zancsv(file, ID=TRUE)
  })
  do.call(rbind, data.list)
}
