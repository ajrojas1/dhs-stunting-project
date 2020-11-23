# Alfredo Rojas
# 11/22/2020
# description: using dhs and avhrr ndvi data to do some analysis
# filename: analysis.R

library(tidyverse)
library(haven)

tempmax <- read_csv("prenatal_tempmax.csv") %>%
  rename(tempmax_pnavg = mean_pn)

precip <- read_csv("prenatal_precip.csv") %>%
  rename(precip_pnavg = mean_pn)

# get preliminary predictors of interest
data_bf <- read_dta("data/idhs_00011.dta") %>%
  rowid_to_column("rowid") %>%
  select("rowid", "dhsid", "idhspid", "year", "urban", "wealthq", 
         "hwhazwho", "ecoregion", "electrc", "educlvl") %>%
  filter(hwhazwho < 9995)

# merge and handle missing
data_bl <- data_bf %>%
  left_join(precip, by = c("rowid" = "ID")) %>%
  left_join(tempmax, by = c("rowid" = "ID")) %>%
  select(urban, wealthq, hwhazwho, ecoregion, electrc, 
         educlvl, tempmax_pnavg, precip_pnavg) %>%
  mutate(
    ecoregion = na_if(ecoregion, -998),
    electrc = na_if(electrc, 8)
  )

# convert to factor if necessary
col_names <- c("urban", "wealthq", "educlvl", "ecoregion", "electrc", "educlvl")
combined[col_names] <- map(combined[col_names], as_factor)
# str(combined)

# look at relationship between dep and continuous ind var
numeric_vars <- combined %>%
  select(hwhazwho, precip_pnavg, tempmax_pnavg)
