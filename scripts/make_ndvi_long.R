# Alfredo Rojas
# 12/17/2020
# description: re-extracted ndvi on decadal scale, this script stacks and datasets
# so trends and anomolies can be detected. 
# filename: make_ndvi_long.R

library(tidyverse)
library(foreign)
library(haven)
library(lubridate)
library(Hmisc)
library(purrr)
library(rlang)
source("scripts/functions.R")

path93 <- c("data/dhs1993_ndvi_stats") # is the c() necessary?
path98 <- c("data/dhs1998_ndvi_stats")
path03 <- c("data/dhs2003_ndvi_stats")
path10 <- c("data/dhs2010_ndvi_stats")

#### Function for getting Decadal Data and stacking them ####

get_decadal <- function(path_string, subset_string) {
  
  files <- list.files(path = path_string, recursive = TRUE)
  files_subset <- file.path(path_string, str_subset(files, subset_string))
  long_data <- create_ndvi_long(files_subset)
  stack_data <- stack_ndvi(long_data)
  return(stack_data)
}

# 1993 data, get stacked data and then get date objects, write to csv
max93_stack <- get_decadal(path93, "max")
write_csv(max93_stack, "data/dhs1993_ndvi_stats/decadal_1993_stack.csv")

max93_final <- get_ndvi_time("data/dhs1993_ndvi_stats/decadal_1993_stack.csv") %>%
  unite(date, c("year", "month", "day"), sep = "-")  %>% # removes input columns from data.frame
  mutate(date = ymd(date))

write_csv(max93_final, "data/dhs1993_ndvi_stats/decadal_1993_ndvi_ymd.csv")

# 1998
max98_stack <- get_decadal(path98, "max")
write_csv(max98_stack, "data/dhs1998_ndvi_stats/decadal_1998_stack.csv")

max98_final <- get_ndvi_time("data/dhs1998_ndvi_stats/decadal_1998_stack.csv") %>%
  unite(date, c("year", "month", "day"), sep = "-")  %>% # removes input columns from data.frame
  mutate(date = ymd(date))
write_csv(max98_final, "data/dhs1998_ndvi_stats/decadal_1998_ndvi_ymd.csv")

# 2003
max03_stack <- get_decadal(path03, "max")
write_csv(max03_stack, "data/dhs2003_ndvi_stats/decadal_2003_stack.csv")

max03_final <- get_ndvi_time("data/dhs2003_ndvi_stats/decadal_2003_stack.csv") %>%
  unite(date, c("year", "month", "day"), sep = "-")  %>% # removes input columns from data.frame
  mutate(date = ymd(date))
write_csv(max03_final, "data/dhs2003_ndvi_stats/decadal_2003_ndvi_ymd.csv")

# 2010
max10_stack <- get_decadal(path10, "max")
write_csv(max10_stack, "data/dhs2010_ndvi_stats/decadal_2010_stack.csv")

## TODO: clean up other lines with path93, etc., like this one:
max10_final <- get_ndvi_time(paste0(path10, "/decadal_2010_stack.csv")) %>%
  unite(date, c("year", "month", "day"), sep = "-")  %>% # removes input columns from data.frame
  mutate(date = ymd(date))
write_csv(max10_final, paste0(path10, "/decadal_2010_ndvi_ymd.csv"))

############### Read in already-created data ################
paths <- c(path93, path98, path03, path10)
years <- c("1993", "1998", "2003", "2010")

list_dfs <- list()
for(i in 1:length(paths)) {
  
  list_dfs[[i]] <- read_csv(file.path(paths[i], paste0(years[i],"_decadal_ndvi.csv"))) %>%
    rowid_to_column("rowid") %>%
    separate(date, into = c("Year", "Month", "Day"), sep = "-") %>%
    group_by(DHSID, Year, Month) %>%
    summarise(
      days_per_mo = n(),
      avgmaxndvi = mean(ndvi) # average MAX ndvi
    )

}

# drop 1981 cuz it's not a full year with 12 months (important for lags)
total_ndvi <- do.call("rbind", list_dfs) %>%
  as_tibble() %>%
  filter(Year != "1981", Year != "2011",
         Year != "2012", Year != "2013")

table(total_ndvi$Month) # equal numbers of months all around
write_csv(total_ndvi, "BF_DHS_93to10_maxNDVI.csv")

# TODO test to see if this works
total_ndvi <- total_ndvi %>%
  rename(dhsid=DHSID,
         year=Year,
         month=Month)
write_dta(total_ndvi, "bf_ndvi.dta")


     