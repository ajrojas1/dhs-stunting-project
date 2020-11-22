# Alfredo Rojas
# 11/17/2020
# description:  computes rowMeans for each year leading up to survey date and one year 
# after survey, as well as create functions to transform DHS enviro data from wide to long
# filename: wide_long_env_dhs.R

# TODO: create a function for the rowMeans stuff, like accept a string and concatenate
# the numbers so you don't ahve to copy and past this thing over and over. cleaner code
# Think about how you can get the pre-natal stuff using stuff from purrr package. . . 
# Also, the pre-natal function grabs missing values (agemo = 99) but when we stack
# rows they get dropped. . . figure out how to solve this. . . maybe convert to NA.

# TODO: look at kids with haz scores < 9995, significantly reduces dataset. . . 
# but Idk what to do in this case. . . talk to Clark, Mostly not in universe

# Include some of the missing data stuff as part of EDA for final paper. . . visualize.

library(tidyverse)
library(haven)
library(lubridate)
library(foreign)
source("scripts/functions.R")

# set working directory
file_path <- file.path("data", "idhs_00011.dta")
data_bf <- read_dta(file_path) %>%
  zap_label() %>%
  zap_labels() %>%
  zap_formats() %>%
  rowid_to_column("ID")
  

# get all precipitation data and pivot to long, <= survey date
precip_bf <- data_bf %>%
  zap_labels() %>% # this seems necessary for pivot_longer()
  select(ID, idhspid, dhsid, year, starts_with("precip_")) %>%
  pivot_longer(
    cols = starts_with("precip"),
    names_to = "time",
    values_to = "precip",
    values_drop_na = FALSE) %>%
  separate(
    col = "time",
    into = c("type", "month"),
    sep = "_"
  ) %>%
  filter(!grepl("a", month)) %>%
  mutate(month = as.numeric(month)) # month has to be numeric for get_enviro_pn()

# TODO: can do same wide to long procedure for tempmin and tempmax

# use current temp/precip to capture some pre-natal values. May not work for all kids
get_enviro_pn <- function(enviro_data, age_mon, id) {
  pre_natal <- enviro_data %>%
    filter(ID == id, month > age_mon & month <= (age_mon + 9)) 
  return(pre_natal)
}

# get prenatal for enviro data
list_dfs <- list()
for(i in 1:nrow(bf_data)) { #nrow(bf_data)
  
  # prenatal is birthday minus duration of 9 mo.
  age_mon <- data_bf[i, ]$kidagemo
  id <- data_bf[i, ]$ID 
  
  # inputs: start = prenatal, end = birthday, dhsid = dhsid, data = mean_bf_ndvi 
  list_dfs[[i]] <- get_enviro_pn(precip_b4srvy, age_mon, id) 
  print(i) # keep track of progress?
}

# stack data: rows on rows
stack_df <- list_dfs %>%
  do.call(rbind, .) %>%
  as_tibble()

prenatal_precip <- stack_df %>%
  group_by(ID, idhspid, dhsid) %>%
  summarise(
    mean_pn = mean(precip),
    num = n()
  )
