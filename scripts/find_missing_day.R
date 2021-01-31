# Alfredo Rojas
# Date: 1/31/2021
# description: find missing day out of date sequence
# filename: find_missing_day.R

library(lubridate)
library(tidyverse)

setwd("C:/Users/AJ Rojas/Desktop/Alfredo/CHIRTS_reclass")

files <- list.files(pattern = "*.tif$") %>%
  as_tibble()

dates <- files %>%
  separate(value, sep = "_", into = c("garbage1", "Year", 
                                      "Day", "Month", "garbage2")) %>%
  select(-garbage1, -garbage2) %>%
  filter(Year >= 2000) %>%
  mutate(Date = paste(Year, Day, Month, sep = "/"))

dates_clean <- dates %>%
  mutate(Date_v2 = ymd(Date)) %>%
  select(Date_v2) %>%
  as_tibble()

first_day <- first(dates_clean$Date_v2)
last_day <- last(dates_clean$Date_v2)

# Great solution to find missing day using lubridate:
# https://community.rstudio.com/t/how-to-check-missing-days-in-datasets-in-r/45509/3


time_span <- first_day + days(0:4016) 
time_df <- data.frame(Date = time_span) %>%
  as_tibble() 
  

time_df %>% anti_join(dates_clean, by = c("Date" = "Date_v2"))

# seems like 2001-12-17 is missing from second. . . 
# checking the original files, seems like two are missing and there seems
# to be a double

dates_full <- files %>%
  separate(value, sep = "_", into = c("garbage1", "Year", 
                                      "Day", "Month", "garbage2")) %>%
  select(-garbage1, -garbage2) %>%
  mutate(Date = paste(Year, Day, Month, sep = "/"))

dates_full %>%
  filter(!unique(Date))
