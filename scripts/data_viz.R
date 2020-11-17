# Alfredo Rojas
# Date: 11/01/2020
# Description: Visualize NDVI and other enviro data
# file: data_viz.R

library(tidyverse)
#library(foreign)
library(haven)
library(stringr)
# require(ndvi_data_mgmt.R)

# set up directory and files
path_name <- file.path("Dropbox", "Articles", "HAZ_paper", "NDVI_data")
setwd(path_name) # specify path name above


mean_ndvi_times <- get_ndvi_time("ndvi_mean_bf.dta") %>%
  rename(ndvi_mean = ndvi) %>%
  filter(
    timeOfMonth == "a", 
    as.numeric(year) <= 1994, 
    # ndvi_mean != -1, 
    DHSID == "BF199300000001") %>%
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-"), "%Y-%m-%d")
  )

max_ndvi_times <- get_ndvi_time("ndvi_max_bf.dta") %>%
  rename(ndvi_max = ndvi) %>%
  filter(
    timeOfMonth == "a", 
    as.numeric(year) <= 1994, 
    # ndvi_max != -1, 
    DHSID == "BF199300000001") %>%
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-"), "%Y-%m-%d")
  )

min_ndvi_times <- get_ndvi_time("ndvi_min_bf.dta") %>%
  rename(ndvi_min = ndvi) %>%
  filter(
    timeOfMonth == "a", 
    as.numeric(year) <= 1994, 
    # ndvi_min != -1, 
    DHSID == "BF199300000001") %>%
  mutate(
    date = as.Date(paste(year, month, "01", sep = "-"), "%Y-%m-%d")
  )

# this works, it just makes my computer crash. . . 
all_data <- left_join(
  mean_ndvi_times,
  max_ndvi_times, 
  by = c("DHSID", "LATNUM", "LONGNUM", "year", "month", "timeOfMonth", "date")) %>%
  left_join(
    min_ndvi_times,
    by = c("DHSID", "LATNUM", "LONGNUM", "year", "month", "timeOfMonth", "date")
  )
  

# first_ndvi <- data %>%
#   filter(
#     timeOfMonth == "a", 
#     as.numeric(year) <= 1994, 
#     ndvi != -1, 
#     DHSID == "BF199300000001") %>%
#   mutate(
#     date = as.Date(paste(year, month, "01", sep = "-"), "%Y-%m-%d")
#       )


long_data <- all_data %>%
  pivot_longer(
    cols = contains("ndvi"),
    names_to = "type_measure",
    values_to = "ndvi_measure"
  )


long_data %>%
  ggplot(aes(x = date, y = ndvi_measure, color = type_measure)) +
  geom_line() +
  xlab("")


