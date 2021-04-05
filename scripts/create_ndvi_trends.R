# 2/25/2021
# Alfredo
# filename: create_ndvi_trends.R
# description: replicating the time lags functionality done in Stata for
# the DHS HAZ project

library(timetk)
library(tidyverse)

## timetk package provides a great way to do lags!

# AVHRR ndvi data
ndvi <- read_csv("BF_DHS_93to10_maxNDVI.csv") %>%
  filter(avgmaxndvi != -1)
summary(ndvi$avgmaxndvi)

# CHIRPS
climate <- read_csv("data/CHIRPS/CHIRPS_DHS_1981_2010.csv")
summary(climate$PRECIP)

# modify data so year, month, day are in separate columns, get monthly mean
climate <- climate %>%
  filter(!is.na(PRECIP)) %>%
  separate(col = DATE,
           into = c("year", "month", "day"),
           sep = "_"
  ) %>%
  group_by(DHSID, year, month) %>%
  summarise(
    avgprecip = mean(PRECIP)
  )

# TODO: clean up this code

################# TEST lead/lags - make 3-monthly accumulation ##########

# this creates two month lags and then take mean for 3 months including current
climate_lags <- climate %>%
  arrange(DHSID, year, month) %>%
  group_by(DHSID) %>%
  tk_augment_lags(contains("precip"), .lags = 1:2) 

rm(climate)

# Sum across rows
# NOTE: this lag contains the month on which observation lies
climate_lags$mean3mo <- rowSums(climate_lags[ ,4:6]) # VERIFY <--- rowSums

climate_lags <- climate_lags %>%
  select(-contains("_lag"))

# create ndvi lags -- 60 will be a 5-year trend --NOTE: lags lead up to
# the month in question. . . 
ndvi_lags <- ndvi %>%
  arrange(DHSID, Year, Month) %>%
  group_by(DHSID) %>%
  tk_augment_lags(contains("ndvi"), .lags = 1:60) # change to 1:60

# create climate lags of 3mo means. . . corresponds to ndvi lags
# Thes lags lead up into the month in question
# Interpretation is a little confusing becasue the first lag for rowSums
# crate two NA values, so remember it still incudes all 12 months even if
# it looks like it doesn't because of th NAs
climate_lags2 <- climate_lags %>%
  arrange(DHSID, year, month) %>%
  group_by(DHSID) %>%
  tk_augment_lags(contains("mean"), .lags = 1:60)

# remove values that have NA - dataset used for regressions
ndvi <- ndvi_lags %>%
  filter(Year > 1986) %>% # Will need to remove NAs from 5 year trends
  select(-days_per_mo) 

# left_join requires same type for columns
ndvi$Month <- as.numeric(ndvi$Month)

# dataset used for regressions
climate <- climate_lags2 %>%
  filter(year > 1986, year < 2011) 

# left_join requires same type for columns
climate$year <- as.numeric(climate$year)
climate$month <- as.numeric(climate$month)

# same no of obs -- GOOD
nrow(ndvi) == nrow(climate)

# join datasets
# ndvi_clim <- left_join(ndvi, climate, by = c("DHSID", 
#                                              "Year" = "year", 
#                                              "Month" = "month"))


#################### Regressions NDVI v Precip ######################
slopes <- list()
p_values <- list()
id_year_mo <- list()
for(i in 1:nrow(ndvi)) {
  
  # look at first row and do pivot_longer
  first_ndvirow <- ndvi[i, ] 
  first_preciprow <- climate[i,]
  id_year_mo[[i]] <- first_ndvirow %>%
    select(DHSID, Year, Month)
  
  ndvi_long <- first_ndvirow %>%
    pivot_longer(cols = avgmaxndvi_lag1:avgmaxndvi_lag60,
                 names_to = c(".value", "lag"), # .value is assigned to (.+g)
                 names_pattern = "(.+g)(\\d{1,2})") 
  
  clim_long <- first_preciprow %>%
    pivot_longer(cols = mean3mo_lag1:mean3mo_lag60,
                 names_to = c(".value", "lag"),
                 names_pattern = "(.+g)(\\d{1,2})")
  
  # join data.frames for regression
  ndvi_clim <- left_join(ndvi_long, clim_long, by = c("DHSID",
                                                      "Year" = "year",
                                                      "Month" = "month",
                                                      "lag"))
  
  
  # plot(ndvi_clim$avgmaxndvi_lag, ndvi_clim$mean3mo_lag)
  
  # correlation test, highly significant
  # cor.test(ndvi_clim$avgmaxndvi_lag, ndvi_clim$mean3mo_lag)
  
  reg <- lm(avgmaxndvi_lag ~ mean3mo_lag, data = ndvi_clim)
  # plot(reg)
  
  time <- c(1:60)
  residuals <- reg$residuals
  
  res_mod <- lm(residuals ~ time)
  slopes[[i]] <- summary(res_mod)$coefficients[2] # coefficient on time (slope)
  p_values[[i]] <- summary(res_mod)$coefficients[2,4] # p-value for time
  
  # sd(residuals)
  print(i)
  
}
rm(climate, climate_lags, climate_lags2)
rm(ndvi, ndvi_lags)

ids <- do.call("rbind", id_year_mo)
slope_df <- do.call("rbind", slopes)
p_values_df <- do.call("rbind", p_values)

total_df <- do.call("cbind", list(ids, slope_df, p_values_df))

total_df <- read_csv("../data/ndvi_trends.csv")

# calculate trend anomalies as we did with other climate vars
anomalies <- total_df %>%
  arrange(DHSID, Year, Month) %>%
  group_by(DHSID) %>%
  mutate(trndanom5yr = (slopes - mean(slopes))/sd(slopes))

write_csv(anomalies, "../data/ndvi_trends.csv")


## TODO: so you have the regression of ndvi on rainfall. . . 
# next step is to compare residual slopes to sigma errors. . . 
# maybe take local DHSID means (like clark did and do something similar)

# Also figure out a way to store slopes and p-values to attach to file

# TODO: Also note, what is the 3-period accumulation? 


# seems to have workd
# for(i in 1:10000) {
#   
#   print(slopes[[i]] == total_df$slopes[i])
# }

####### Read in Lags created in Stata and export as CSV #######
data <- read_dta("ndvi_anom_BF.dta") %>%
  zap_label() %>%
  zap_labels() %>%
  zap_formats() %>%
  as_tibble()
write_csv(data, "ndvi_anom_BF.csv")
