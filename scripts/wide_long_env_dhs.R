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
  
# privot precipitation values to long format
precip_long <- get_context_long(data_bf, "precip")%>%
  filter(!grepl("a", month)) %>%
  mutate(month = as.numeric(month)) # month has to be numeric for get_enviro_pn()

# pivot max temp to long format
tempmax_long <- get_context_long(data_bf, "tempmax")%>%
  filter(!grepl("a", month)) %>%
  mutate(month = as.numeric(month)) # month has to be numeric for get_enviro_pn()

# function to get prenatal for each kid. 
# TODO: maybe make this function more general for other uses. 
get_dhs_prenatal <- function(data, env_long) { 
  
  # Required: data assumes format specified at beginning and long data above
  
  list_dfs <- list()
  for(i in 1:nrow(bf_data)) { #nrow(bf_data)
    
    # prenatal is birthday minus duration of 9 mo.
    age_mon <- data[i, ]$kidagemo
    id <- data[i, ]$ID 
    # inputs: start = prenatal, end = birthday, dhsid = dhsid, data = mean_bf_ndvi 
    list_dfs[[i]] <- get_enviro_prenatal(env_long, age_mon, id) 
    print(i) # keep track of progress?
  }
  
  # stack data: rows on rows
  stack_df <- list_dfs %>%
    do.call(rbind, .) %>%
    as_tibble()
  
  return(stack_df)
}

precip_stack <- get_dhs_prenatal(data_bf, precip_long)

# summarize data to create mean for prenatal
prenatal_precip <- precip_stack %>%
  group_by(ID, idhspid, dhsid) %>%
  summarise(
    mean_pn = mean(precip),
    num = n()
  )
write_csv(prenatal_precip, "prenatal_precip.csv")

tempmax_stack <- get_dhs_prenatal(data_bf, tempmax_long)

# tempmax prenatal values
prenatal_tempmax <- tempmax_stack %>%
  group_by(ID, idhspid, dhsid) %>%
  summarise(
    mean_pn = mean(tempmax),
    num = n()
  )

write_csv(prenatal_tempmax, "prenatal_tempmax.csv")


