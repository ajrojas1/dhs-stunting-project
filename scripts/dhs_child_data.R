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
# source(ndvi_data_mgmt.R)

# set up directory and files
path_name <- file.path("Dropbox", "Articles", "HAZ_paper", "data")
setwd(path_name) # specify path name above

bf_data <- read_dta("DHS_Data_CI_BF/BF_data_child/idhs_0009.dta") %>%
  select(idhspid, dhsid, kidbirthmo, kidbirthyr) %>%
  unite(date, c("kidbirthyr", "kidbirthmo"), sep = "-") %>%
  mutate(birthmo = ymd(paste(date, "01", sep="-"))) %>%
  rowid_to_column(var = "rowid")
  

mean_bf_ndvi <- get_ndvi_time("../../NDVI_data/ndvi_mean_bf.dta") %>%
  unite(date, c("year", "month", "day"), sep = "-")  %>% # removes input columns from data.frame
  mutate(date = ymd(date)) # format date (from lubridate pkg)


# Task: capture pre-natal (period of ~40 weeks) exposure to enviro data
# Method: take child birth month-year, add 1 month to it, then create 40 
# week interval from that new value

get_avg_pn <- function(start, end, dhsid, data) {
  
  ###
  # this function subsets prenatal values of longitudinal environmental
  # data, like NDVI, temp, precip using dates using DHSID codes
  # start = birth date - 9 months; end = birth date in DHS data
  ###
  
  data_subset <- data %>%
    filter(date >= start & date <= end, DHSID == dhsid) 
  
  return(data_subset)
}

# test
end = bf_data$birthmo[1]
start = as_date(end - dmonths(9))
dhsid = "BF199300000001"
test <- get_avg_pn(start, end, dhsid, mean_bf_ndvi) # works


# get prenatal NDVI values for every child
list_dfs <- list()
for(i in 1:nrow(bf_data)) { #nrow(bf_data)
  
  # prenatal is birthday minus duration of 9 mo.
  birthday <- bf_data[i, ]$birthmo
  prenatal <- as_date(birthday - dmonths(9)) 
  dhsid = bf_data[i, ]$dhsid
  dhspid = bf_data[i, ]$idhspid
  rowid = bf_data[i, ]$rowid
  
  # inputs: start = prenatal, end = birthday, dhsid = dhsid, data = mean_bf_ndvi 
  list_dfs[[i]] <- get_avg_pn(prenatal, birthday, dhsid, mean_bf_ndvi) %>%
    mutate(
      idhspid = rep(dhspid, nrow(.)), # adds idhspid
      rowid = rep(rowid, nrow(.)) # adds rowid to each child obs. 
      ) # (some children have same pid (mom))
  
  # print(i) # keep track of progress?
}

# stack data: rows on rows
stack_df <- list_dfs %>%
  do.call(rbind, .) %>%
  as_tibble()

# summarizes ndvi across obs. -- it works!
prenatal_ndvi <- stack_df %>%
  group_by(rowid, idhspid, DHSID) %>%
  summarise(
    mean_pn = mean(ndvi),
    num = n()
    )

# ndvi doesn't go far back enough. . . re-do
table(prenatal_ndvi$num)

# change directory. 
write_csv(prenatal_ndvi, "prenatal_ndvi_93to10.csv")

