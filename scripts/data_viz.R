# Alfredo Rojas
# Date: 11/01/2020
# Description: Visualize NDVI and other enviro data
# file: data_viz.R

library(tidyverse)
#library(foreign)
library(haven)
library(stringr)
library(ggplot2)
library(directlabels)
# require(ndvi_data_mgmt.R)

path_name_DHS <- file.path("data", "idhs_00013.dta")
dhs <- read_dta(path_name_DHS)

# set up directory and files
path_name93 <- file.path("data", "dhs1993_ndvi_stats", "1993_decadal_ndvi.csv")

## Visualize geo data
library(sf)
library(leaflet)
bf_1993 <- st_read("data/BF_1993_DHS_04032020_1547_134297/BFGE23FL/BFGE23FL.shp")%>%
  st_transform(4326)
bf_1998 <- st_read("data/BF_1998-99_DHS_04032020_1546_134297/BFGE32FL/BFGE32FL.shp")%>%
  st_transform(4326)
bf_2003 <- st_read( "data/BF_2003_DHS_04032020_1546_134297/BFGE43FL/BFGE43FL.shp")%>%
  st_transform(4326)
bf_2010 <- st_read("data/BF_2010_DHS_04032020_1546_134297/BFGE61FL/BFGE61FL.shp") %>%
  st_transform(4326) 

bf_2010_test <- bf_2010 %>%
  group_by(URBAN_RURA, DHSREGNA) %>%
  summarise(DHSID = first(DHSID)) %>%
  filter(URBAN_RURA != "U")

# quickie map
leaflet() %>% 
  # addCircles(data = bf_1993) %>%
  # addCircles(data = bf_1998) %>%
  # addCircles(data = bf_2003) %>%
  addCircles(data = bf_2010_test) %>%
  addTiles()

path_name10 <- file.path("data", "dhs2010_ndvi_stats", "2010_decadal_ndvi.csv")

data10_means <- read_csv(path_name10) %>%
  rename(ndvi_mean = ndvi) %>%
  filter(
    date <= as.Date("2013-12-31"), 
    ndvi_mean != -1, 
    DHSID == bf_2010_test$DHSID) %>%
  left_join(bf_2010_test, by = "DHSID")

data10_means %>%
  ggplot(aes(x = date, y = ndvi_mean)) +
  geom_smooth(aes(colour = DHSREGNA), se = FALSE, show.legend = FALSE) +
  geom_dl(aes(label = DHSREGNA), method = list("first.points", rot = 45), cex = 0.0001) +
  xlab("")


# TODO: Maybe look at households within certain ecoregions/livelihood/soil types to compare

# BF201000000025 - Sahel
# BF201000000071 - Centre Sud
# BF201000000123 - Hatus Basins