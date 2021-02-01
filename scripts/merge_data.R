# Alfredo Rojas
# 01/27/2021
# description: merge data so OBJECTID and DHSID match
# filename: merge_data.R

library(tidyverse)
# set working directories
# setwd()
x = read_csv("CHIRTS_CSV/merged_tables/merged_tables_CHIRTS_BF.csv")
y = read_csv("BF_10km_buffer.csv")
merge_xy <- left_join(x, y, by = c("OBJECTID" = "OID_")) %>%
  select(OBJECTID, LOCATIONID, X, Y, TEMP, DATE, DHSID, DHSYEAR)
rm(x, y)
write_csv(merge_xy, "CHIRTS_DHS_1983_2010.csv")
rm(merge_xy)

