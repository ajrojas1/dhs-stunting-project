# Deal with Missing Values

library(tidyverse)
library(haven)
library(foreign)

# set up directory and files
path_name <- file.path("Dropbox", "Articles", "HAZ_paper", "data")
setwd(path_name)

# compare gps observations quickly
data <- read_dta("DHS_Data_CI_BF/BF_data_child/idhs_00011.dta")

test <- data %>%
  select(starts_with("temp"), starts_with("precip")) %>%
  filter_all(any_vars(.== -998)) # 1024 obs

prenatal_data <- read_csv("prenatal_ndvi_93to10.csv")

missing_ndvi <- prenatal_data %>%
  filter(mean_pn == -1) # 1024 obs. -- same

# Identify other missing values
