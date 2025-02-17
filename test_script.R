#Test script
library(tidyverse)
library(reshape)
library(lubridate)
library(stringr)
#Single output file
data <- read_zancsv("data/cherryshrimp2024/csv/LightDarktestCS_final-20241104T155414.csv") %>%
        dplyr::rename(A3_Z4_T=A3_Z2_T.1)

trans <-transform_zan(data)

#Multiple output files
data <- read_manyzancsv("data/cherryshrimp2024/csv/") %>% dplyr::rename(A3_Z4_T=A3_Z2_T.1)
trans <- transform_zan(data, ID=TRUE)

file <- "LightDarktestCS_final-20241104T155414-XY_data.csv"
data <- read_zancoord("data/cherryshrimp2024/coord.csv/", file = file)

X <- data %>%
  dplyr::select(RUNTIME, file.timestamp, contains("X")) %>%
  pivot_longer(cols=contains("X"), values_to="X", names_to="arena") %>%
  mutate(arena=gsub("X_", "", arena))

Y <- data %>%
  dplyr::select(RUNTIME, file.timestamp, contains("Y")) %>%
  pivot_longer(cols=contains("Y"), values_to="Y", names_to="arena") %>%
  mutate(arena=gsub("Y_", "", arena))

left_join(X, Y)

csv <- read_zancsv("data/cherryshrimp2024/csv/LightDarktestCS_final-20241104T155414.csv") %>%
  dplyr::rename(A3_Z4_T=A3_Z2_T.1) %>% transform_zan(., ID=FALSE) %>% mutate(RUNTIME=round(TIME, 2))
file <- "LightDarktestCS_final-20241104T155414-XY_data.csv"
xy <- read_zancoord("data/cherryshrimp2024/coord.csv/", file = file) %>% transform_xy(.)
xy %>% filter(RUNTIME >=50) %>% mutate(RUNTIME=round(RUNTIME, 2)) %>% left_join(., csv)
