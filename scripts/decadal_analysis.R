# Alfredo Rojas
# 12/17/2020
# description: re-extracted ndvi on decadal scale, this script stacks and datasets
# so trends and anomolies can be detected. 
# filename: decadal_analysis.R

# TODO: create function to create csv of this data. . . too much copy & paste before

library(tidyverse)
library(foreign)
library(haven)
library(lubridate)
source("scripts/functions.R")

path93 <- c("data/dhs1993_ndvi_stats") # is the c() necessary?
path98 <- c("data/dhs1998_ndvi_stats")
path03 <- c("data/dhs2003_ndvi_stats")
path10 <- c("data/dhs2010_ndvi_stats")

#### Function for getting Decadal Data stacked ####
get_decadal <- function(path_string, subset_string) {
  
  files <- list.files(path = path_string, recursive = TRUE)
  files_subset <- file.path(path_string, str_subset(files, subset_string))
  long_data <- create_ndvi_long(files_subset)
  stack_data <- stack_ndvi(long_data)
  return(stack_data)
}

# 1993 data, get stacked data and then get date objects, write to csv
mean93_stack <- get_decadal(path93, "mean")
write_csv(mean93_stack, "data/dhs1993_ndvi_stats/decadal_1993_stack.csv")

mean93_final <- get_ndvi_time("data/dhs1993_ndvi_stats/decadal_1993_stack.csv") %>%
  unite(date, c("year", "month", "day"), sep = "-")  %>% # removes input columns from data.frame
  mutate(date = ymd(date))
write_csv(mean93_final, "data/dhs1993_ndvi_stats/1993_decadal_ndvi.csv")

# 1998
mean98_stack <- get_decadal(path98, "mean")
write_csv(mean98_stack, "data/dhs1998_ndvi_stats/decadal_1998_stack.csv")

mean98_final <- get_ndvi_time("data/dhs1998_ndvi_stats/decadal_1998_stack.csv") %>%
  unite(date, c("year", "month", "day"), sep = "-")  %>% # removes input columns from data.frame
  mutate(date = ymd(date))
write_csv(mean98_final, "data/dhs1998_ndvi_stats/1998_decadal_ndvi.csv")

# 2003
mean03_stack <- get_decadal(path03, "mean")
write_csv(mean03_stack, "data/dhs2003_ndvi_stats/decadal_2003_stack.csv")

mean03_final <- get_ndvi_time("data/dhs2003_ndvi_stats/decadal_2003_stack.csv") %>%
  unite(date, c("year", "month", "day"), sep = "-")  %>% # removes input columns from data.frame
  mutate(date = ymd(date))
write_csv(mean03_final, "data/dhs2003_ndvi_stats/2003_decadal_ndvi.csv")

# 2010

mean10_stack <- get_decadal(path10, "mean")
write_csv(mean10_stack, "data/dhs2010_ndvi_stats/decadal_2010_stack.csv")

mean10_final <- get_ndvi_time(paste0(path10, "/decadal_2010_stack.csv")) %>%
  unite(date, c("year", "month", "day"), sep = "-")  %>% # removes input columns from data.frame
  mutate(date = ymd(date))
write_csv(mean10_final, paste0(path10, "/2010_decadal_ndvi.csv"))


