# Alfredo Rojas
# 2/25/2021
# filename: merge_provinces.R
# description: takes IPUMS geo provinnce data and merges them to DHS
# clusters so they can be attached to DHS survey data for Fixed Effects

library(sf)
library(tidyverse)

path_pr <- file.path("data", "bf_provinces", "geo2_bf2006.shp")
gdb <- file.path("data", "data.gdb")

provinces <- st_read(path_pr)
clusters <- st_read(dsn = gdb, layer = "BF_10km_buffer")
centroids <- st_centroid(clusters)

st_crs(centroids) == st_crs(provinces)

plot(st_geometry(centroids))
plot(st_geometry(provinces))

intersection <- st_intersection(centroids, provinces) 
plot(st_geometry(intersection))

intsctn_plain <- intersection %>%
  st_set_geometry(NULL)

write_csv(intsctn_plain, "data/provinces_dhsids.csv")
