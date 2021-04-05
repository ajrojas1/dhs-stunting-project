# Alfredo Rojas
# 11/22/2020
# description: using dhs and avhrr ndvi data to do some analysis
# filename: analysis.R

library(tidyverse)
library(haven)
library(stargazer)
library(R.utils)
# library(MASS)

gunzip("data/idhs_00014.dta.gz", remove=FALSE)

# get preliminary predictors of interest
dhs_bf <- read_dta("data/idhs_00014.dta") %>%
  rowid_to_column("rowid") %>%
  select(rowid, dhsid, idhspid, year, urban, wealthq, hhmemtotal, 
         kidcurage, kidsex, kidbirthmo, kidbord, chebalive,
         hwhazwho, educlvl, heightfem) %>%
  mutate(missing = ifelse(hwhazwho > 9000, 0, 1))

# create tabke if missing values
table(dhs_bf$year, dhs_bf$missing)

## TODO: drop missing values, and create a Stunted vs. not-stunted category

dhs_bf_cat <- dhs_bf %>%
  filter(hwhazwho < 9000) %>%
  mutate(isStunted = ifelse(hwhazwho <= -200, "stunted", "not_stunted"))

dhs_bf_cat$isStunted <- as.factor(dhs_bf_cat$isStunted)

table(dhs_bf_cat$year, dhs_bf_cat$isStunted)

dhsid_93 <- dhs_bf %>%
  select(dhsid)

dhsid_93 <- unique(dhsid_93$dhsid)

chirps <- read_csv("data/CHIRPS/CHIRPS_DHS_1981_2010.csv") %>%
  filter(DHSYEAR == 1993) %>%
  select(PRECIP, DATE, DHSID)

chirts <- read_csv("data/CHIRTS/CHIRTS_DHS_1981_2010.csv") %>%
  filter(DHSYEAR == 1993) %>%
  select(PRECIP, DATE, DHSID)

ndvi <- read_csv("data/dhs1993_ndvi_stats/1993_decadal_ndvi.csv")

test <- ndvi %>%
  filter(DHSID == dhsid_93[1]) %>%
  mutate(zscore = (ndvi - mean(ndvi))/sd(ndvi))

# NDVI prenatal data
ndvi_pn <- read_csv("data/prenatal_ndvi_93to10_v2.csv") %>%
  rename(ndvi_pnavg = mean_pn) %>%
  select(rowid, ndvi_pnavg)

# merge and handle missing
data_ready <- dhs_bf %>%
  left_join(precip, by = c("rowid" = "ID")) %>%
  left_join(tempmax, by = c("rowid" = "ID")) %>%
  left_join(ndvi, by = "rowid") %>%
  select(urban, hhmemtotal, wealthq, educlvl,  electrc, heightfem, kidbord, hwhazwho, 
         ecoregion, ndvi_pnavg, tempmax_pnavg, precip_pnavg) %>%
  mutate(
    hwhazwho = ifelse(hwhazwho > -200, 0, 1), # 1 = stunted, 0 = otherwise
    ecoregion = na_if(ecoregion, -998),
    electrc = na_if(electrc, 8),
    tempmax_pnavg = na_if(tempmax_pnavg, -998),
    precip_pnavg = na_if(precip_pnavg, -998),
    ndvi_pnavg = na_if(ndvi_pnavg, -1)
  ) %>%
  drop_na() 

# -------------- quick visual for summary ----------------
library(plyr)
group_med = ddply(data_bf, "urban", summarise, hh.med = median(hwhazwho/100))
group_med$urban <- as.factor(group_med$urban)

data_viz <- data_bf %>%
  select(hwhazwho, urban)

data_viz$urban <- as.factor(data_viz$urban)
ggplot(data_viz, 
       mapping = aes(x = hwhazwho/100, 
                    fill = as.factor(urban), 
                    group = as.factor(urban))) + 
  geom_density(size = 0.5, alpha = 1/3) +
  labs(
    title = "Density of Height-for-Age-Z-Scores (HAZ) in Urban and 
    Rural Areas, Burkina Faso (1993-2010)",
    x = "HAZ",
    y = "Density"
  ) +
  scale_fill_discrete(name = "Residence", labels = c("Urban", "Rural")) +
  geom_vline(data = group_med, 
             mapping = aes(xintercept = hh.med, color = urban),
             show.legend = F, linetype = "dashed")

# library(gtsummary)
data_ready %>%
  zap_label() %>%
  zap_labels() %>%
  zap_formats() %>%
  select(-ecoregion, -kidbord) %>%
  tbl_summary(by = year) # add year to data_ready e if you want to do this again
# ----------------------- end code for visual -------------------

# convert to factor if necessary
col_names <- c("urban", "wealthq", "kidbord", "ecoregion", "electrc", "educlvl")
data_ready[col_names] <- map(data_ready[col_names], as_factor)
# str(combined)

# compare with standard OLS
ols_model <- lm(hwhazwho ~ ., data = data_ready)
summary(ols_model)

## NOTE!!! R uses alpha-numerical FIRST as reference, so 0 before 1 or "a" before "b"

# look at model without contextual?
no_enviro <- data_ready %>% 
  select(-ecoregion, -tempmax_pnavg, -precip_pnavg, -ndvi_pnavg)
model1 <- glm(hwhazwho ~ ., data = no_enviro, family = binomial(link="logit"))
summary(model1)

# code below is based on: https://rpubs.com/guptadeepak/logit-assumptions
model2 <- glm(hwhazwho ~ ., data = data_ready, family = binomial(link="logit")) # something happened w/ tempmax
probabilities <- predict(model2, type = "response")
summary(model2)

preds <- colnames(data_ready)
# look at relationship between dep and continuous ind var
numeric_vars <- data_ready %>%
  select(ndvi_pnavg, precip_pnavg, tempmax_pnavg) %>%
  mutate(logit = log(probabilities / (1 - probabilities))) %>%
  pivot_longer(!logit, names_to = "ind_vars", values_to = "value")

numeric_vars %>%
  ggplot(aes(logit, value)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~ind_vars, scales = "free_y") 
  # no linear associations b/n logit and cont. vars -- violates assumption of logistic


# ordinal logistic regressionz -- running into problems, conflict w/ dplyr pkg? weird bugs above
# data_ord <- data_ready %>%
#   select(-hwhazwho, -stunted)
# 
# model3 <- polr(stunted_ord ~ ., data = data_ord, Hess = TRUE)
# summary(model3)



# quickly output results in readable format
stargazer(ols_model, model1, model2, 
          type = "latex",
          single.row = TRUE,
          omit = "kidbord",
          column.labels = c("", "Binary", "Binary"),
          dep.var.labels = "Height-for-Age Z-score",
          title = "Models with and without Environmental Variables", 
          align = TRUE,
          out = "results_bl_2.html")
