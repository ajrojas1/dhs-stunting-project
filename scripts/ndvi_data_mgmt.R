# Alfredo Rojas
# 10/28/2020
# description: converts ndvi output from GEE from wide to long format
# then stacks observations
# filename: ndvi_data_mgmt.R

library(tidyverse)
library(foreign)
library(haven)

# TODO: set up directory and files in a GitHub ready folder. 

# path_name <- file.path("Dropbox", "Articles", "HAZ_paper", "data")
# setwd(path_name) # specify path name above
# files <- list.files(path = ".", pattern = "\\.csv$", recursive = TRUE)
# 
# files_mean <- str_subset(files, "mean")
# files_max <- str_subset(files, "max")
# files_min <- str_subset(files, "min")

# reads in data from AVHRR ndvi and turns it from wide to long
create_ndvi_long <- function(files) {
  
  # create list for data.frames for efficiency
  list_dfs <- list()
  for(i in 1:NROW(files)) {
    # change data from wide to long for DHS data
    data <- read_csv(files[i]) %>%
      pivot_longer(
        cols = ends_with("ndvi"),
        names_to = "time",
        values_to = "ndvi",
        values_drop_na = FALSE
      )
    
    # add dfs to list
    list_dfs[[i]] <- data
  }
  return(list_dfs)
}


stack_data <- function(list_dfs) {

  # stack data: rows on rows
  stack_df <- list_dfs %>%
    do.call(rbind, .) %>%
    as_tibble() %>%
    rename(
      system_index = `system:index` 
    )
  return(stack_df)
}  

# select relevant variables, lat/long just for geographical reference
get_ndvi_time <- function(file_name) {
  
  x <- read_dta(file_name) %>%
    select(DHSID, LATNUM, LONGNUM, time, ndvi) %>%
    separate(
      col = time, 
      into = c("date", "extra"),
      sep = "_")  %>%
    select(-extra) %>%
    separate(
      col = "date",
      into = c("year", "month"),
      sep = 4
    ) %>%
    separate(
      col = "month", 
      into = c("month", "timeOfMonth"),
      sep = 2
    ) %>%
    mutate(
      day = ifelse(timeOfMonth == "a", 1, 16) # correspond to periods of AVHRR a or b
    )
  return(x)
}

# mean_ndvi <- create_ndvi_long(files_mean)
# mean_stack <- stack_data(mean_ndvi)
# 
# max_ndvi <- create_ndvi_long(files_max)
# max_stack <- stack_data(max_ndvi)
# 
# min_ndvi <- create_ndvi_long(files_min)
# min_stack <- stack_data(min_ndvi)
# 
# # export as DTA if needed
# write_dta(mean_stack, "ndvi_mean_bf.dta")
# write_dta(max_stack, "ndvi_max_bf.dta")
# write_dta(min_stack, "ndvi_min_bf.dta")
