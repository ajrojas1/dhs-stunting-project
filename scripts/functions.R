# Alfredo Rojas
# 10/28/2020
# description: functions to handle AVHRR ndvi data, as well as dhs context data
# filename: functions.R

library(tidyverse)
library(foreign)
library(haven)

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

# function to stack AVHRR ndvi, rows on rows
stack_ndvi <- function(list_dfs) {

  stack_df <- list_dfs %>%
    do.call(rbind, .) %>%
    as_tibble() %>%
    rename(
      system_index = `system:index` 
    )
  return(stack_df)
}  

# select AVHRR ndvi year, month, day variables, lat/long just for geographical reference
get_ndvi_time <- function(file_name) {
  
  x <- read_csv(file_name) %>% # note read_csv here, handle other file types? 
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

# function to get annual (12 month) averages of DHS context data
annual_context_avg <- function(string, data) {
  
  if(str_detect(string, "_") == FALSE){
    stop("string must include '_' (e.g. 'precip_') to extract annual contextual means from DHS")
  }
  
  # Gets annual rowMeans for context data
  # string = precip_, tempmin_, etc. 
  annual_means <- data %>%
    select(idhspid, dhsid, intyear, monthint, intday, starts_with(string)) %>%
    unite(date, c("intyear", "monthint", "intday"), sep = "-") %>%
    mutate(date = ymd(date)) %>%
    mutate(
      avg5b = rowMeans(select(., paste0(string, "60"):paste0(string, "49"))), # 5 years before survey
      avg4b = rowMeans(select(., paste0(string, "48"):paste0(string, "37"))),
      avg3b = rowMeans(select(., paste0(string, "36"):paste0(string, "25"))),
      avg2b = rowMeans(select(., paste0(string, "24"):paste0(string, "13"))),
      avg1b = rowMeans(select(., paste0(string, "12"):paste0(string, "01"))),
      avg1a = rowMeans(select(., paste0(string, "00"):paste0(string, "a11"))) # 1 year after survey
    ) %>%
    select(-starts_with(string))
  
  return(annual_means)
  
}

# convert DHS contextual data into long format if needed
get_context_long <- function(data, string) {
  
  # get all precipitation data and pivot to long, <= survey date
  dhs_context <- data %>%
    zap_labels() %>% # this seems necessary for pivot_longer()
    select(names(data)[1], idhspid, dhsid, year, starts_with(paste0(string, "_"))) %>%
    pivot_longer(
      cols = starts_with(paste0(string, "_")),
      names_to = "time",
      values_to = string,
      values_drop_na = FALSE) %>%
    separate(
      col = "time",
      into = c("type", "month"),
      sep = "_"
    ) 
  return(dhs_context)
}

# function to extract prenatal months for NDVI
get_ndvi_pn <- function(start, end, dhsid, data) {
  #####
  # this function subsets prenatal values of longitudinal environmental
  # data, like AVHRR NDVI, using DHSID codes. Relies on Date objects.
  # start = birth date - 9 months; end = birth date in DHS data
  #####
  data_subset <- data %>%
    filter(date >= start & date <= end, DHSID == dhsid) 
  return(data_subset)
  
}

# use current temp/precip to capture some pre-natal values. May not work for all kids
get_enviro_prenatal <- function(enviro_data, age_mon, id) {
  
  ## this function does same as get_ndvi_pn(), but uses month values from DHS
  pre_natal <- enviro_data %>%
    filter(ID == id, month > age_mon & month <= (age_mon + 9)) 
  return(pre_natal)
}


# TODO: set up directory and files in a GitHub ready folder. 

# Below code tests to see if all this stuff works
# path_name <- file.path("Dropbox", "Articles", "HAZ_paper", "data")
# setwd(path_name) # specify path name above
# files <- list.files(path = ".", pattern = "\\.csv$", recursive = TRUE)
# 
# files_mean <- str_subset(files, "mean")
# files_max <- str_subset(files, "max")
# files_min <- str_subset(files, "min")



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
