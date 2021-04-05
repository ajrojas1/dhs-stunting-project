# Alfredo Rojas
# 04/01/2021
# description: Replicate the climate, precip, and NDVI anomalies
# and test to see if they are similar to Stata output
# filename: enviro_anoms.R

library(timetk)
library(tidyverse)

# choose appropriate working directory
getwd()

# AVHRR ndvi data
ndvi <- read_csv("BF_DHS_93to10_maxNDVI.csv") %>%
  filter(avgmaxndvi != -1)
summary(ndvi$avgmaxndvi)

# CHIRPS
precip <- read_csv("data/CHIRPS/CHIRPS_DHS_1981_2010.csv")
summary(precip$PRECIP)

# CHIRTS
temp <- read_csv("data/CHIRTS/CHIRTS_DHS_1983_2010.csv")

# get monthly averages and modify date columns
precip <- precip %>%
  filter(!is.na(PRECIP)) %>%
  separate(col = DATE,
           into = c("year", "month", "day"),
           sep = "_"
  ) %>%
  group_by(DHSID, year, month) %>%
  summarise(
    avgprecip = mean(PRECIP)
  )

# create precip lags
precip_lags <- precip %>%
  arrange(DHSID, year, month) %>%
  group_by(DHSID) %>%
  tk_augment_lags(contains("precip"), .lags = 1:12) 

rm(precip)

## TODO: add temp lags

# create NDVI lags
ndvi_lags <- ndvi %>%
  arrange(DHSID, Year, Month) %>%
  group_by(DHSID) %>%
  tk_augment_lags(contains("ndvi"), .lags = 1:12)

# TODO: create z-scores, wet days and hot days, etc.
