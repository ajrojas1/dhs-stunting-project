# Alfredo Rojas
# 11/10/2020
# description: explores and creates pre-natal/seasonal ndvi data using DHS child data
# filename: dhs_child_data.R

# TODO: I need to create a directory template to deal with all this stuff in a 
# data science-y way. scripts, results, data, etc. When you get updated values from
# NDVI, set up this diretory the right way.

library(tidyverse)
library(foreign)
library(haven)
library(lubridate)
source("scripts/functions.R")

# set up directory and files
path_name <- file.path("data", "idhs_00011.dta")

bf_data <- read_dta(path_name) %>%
  select(idhspid, dhsid, kidbirthmo, kidbirthyr) %>%
  unite(date, c("kidbirthyr", "kidbirthmo"), sep = "-") %>%
  mutate(birthmo = ymd(paste(date, "01", sep="-"))) %>%
  rowid_to_column(var = "rowid")

# get mean ndvi--export to csv (relies on functions.R)
files <- list.files(path = ".", pattern = "\\.csv$", recursive = TRUE)
files_mean <- str_subset(files, "mean")
mean_ndvi <- create_ndvi_long(files_mean)
mean_stack <- stack_ndvi(mean_ndvi)
write_csv(mean_stack, "data/ndvi_avgs_87to11.csv")
  
# separate date values and convert to date object
mean_bf_ndvi <- get_ndvi_time("data/ndvi_avgs_87to11.csv") %>%
  unite(date, c("year", "month", "day"), sep = "-")  %>% # removes input columns from data.frame
  mutate(date = ymd(date)) # format date (from lubridate pkg) -- maybe add inside function

# rm(mean_stack)
# rm(mean_ndvi)

# Task: capture pre-natal (period of ~40 weeks) exposure to enviro data
# Method: take child birth month-year, add 1 month to it, then create 40 
# week interval from that new value

get_ndvi_pn <- function(start, end, dhsid, data) {
  #####
  # this function subsets prenatal values of longitudinal environmental
  # data, like NDVI, temp, precip using dates using DHSID codes
  # start = birth date - 9 months; end = birth date in DHS data
  #####
  data_subset <- data %>%
    filter(date >= start & date <= end, DHSID == dhsid) 
  return(data_subset)
  
}

# test
# end = bf_data$birthmo[1]
# start = as_date(end - dmonths(9))
# dhsid = "BF199300000001"
# test <- get_ndvi_pn(start, end, dhsid, mean_bf_ndvi) # works


# get prenatal NDVI values for every child -- there's probably a way to use purrr!!!
list_dfs <- list()
for(i in 1:nrow(bf_data)) { #nrow(bf_data)
  
  # prenatal is birthday minus duration of 9 mo.
  birthday <- bf_data[i, ]$birthmo
  prenatal <- as_date(birthday - dmonths(9)) 
  dhsid = bf_data[i, ]$dhsid
  dhspid = bf_data[i, ]$idhspid
  rowid = bf_data[i, ]$rowid
  
  # inputs: start = prenatal, end = birthday, dhsid = dhsid, data = mean_bf_ndvi 
  list_dfs[[i]] <- get_ndvi_pn(prenatal, birthday, dhsid, mean_bf_ndvi) %>%
    mutate(
      idhspid = rep(dhspid, nrow(.)), # adds idhspid
      rowid = rep(rowid, nrow(.)) # adds rowid to each child obs. 
      ) # (some children have same pid (mom))
  print(i) # print out progress
}

# stack data: rows on rows
stack_df <- list_dfs %>%
  do.call(rbind, .) %>%
  as_tibble()

write_csv(stack_df, "data/stacked_prenatal_ndvi.csv")

# summarizes ndvi across obs. -- it works!
prenatal_ndvi <- stack_df %>%
  group_by(rowid, idhspid, DHSID) %>% # idhspid, DHSID
  summarise(
    mean_pn = mean(ndvi),
    num = n()
    )

# see if everything was captured--do all kids have 9-10 months of data?
table(prenatal_ndvi$num) # some have 18 mo, others have 19. . . does this matter? verify

# change directory. 
write_csv(prenatal_ndvi, "data/prenatal_ndvi_93to10.csv")

