# Alfredo Rojas
# 11/22/2020
# description: using dhs and avhrr ndvi data to do some analysis
# filename: analysis.R

library(tidyverse)
library(haven)

tempmax <- read_csv("data/prenatal_tempmax.csv") %>%
  rename(tempmax_pnavg = mean_pn) %>%
  select(ID, tempmax_pnavg)

precip <- read_csv("data/prenatal_precip.csv") %>%
  rename(precip_pnavg = mean_pn) %>%
  select(ID, precip_pnavg)

ndvi <- read_csv("data/prenatal_ndvi_93to10.csv") %>%
  rename(ndvi_pnavg = mean_pn) %>%
  select(rowid, ndvi_pnavg)

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
  left_join(ndvi, by = "rowid") %>%
  select(urban, wealthq, hwhazwho, ecoregion, electrc, 
         educlvl, ndvi_pnavg, tempmax_pnavg, precip_pnavg) %>%
  mutate(
    hwhazwho = ifelse(hwhazwho < -200, 1, 0), # 1 = stunted, 0 = otherwise
    ecoregion = na_if(ecoregion, -998),
    electrc = na_if(electrc, 8)
  ) 

# convert to factor if necessary
col_names <- c("urban", "wealthq", "ecoregion", "electrc", "educlvl")
data_bl[col_names] <- map(data_bl[col_names], as_factor)
# str(combined)

# code below is based on: https://rpubs.com/guptadeepak/logit-assumptions
model <- glm(hwhazwho ~ ., data = data_bl, family = binomial) # something happened w/ tempmax
probabilities <- predict(model, type = "response")

# look at relationship between dep and continuous ind var
numeric_vars <- combined %>%
  select(hwhazwho, ndvi_pnavg, precip_pnavg, tempmax_pnavg)
