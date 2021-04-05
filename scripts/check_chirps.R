# inspect CHIRPS data to verify data quality

# description: this code calculates annual sum across DHS clusters
# filename: check_chirps.R

chirps <- read_csv("../data/CHIRPS/CHIRPS_DHS_1981_2010.csv")

chirps_small <- chirps[1:100, ]

chirps_annsum <- chirps %>%
  separate(DATE, into = c("year", "month", "day"), sep="_") %>%
  group_by(DHSID, year) %>%
  summarise(
    n = n(),
    annprecip = sum(PRECIP)
  )

summary(chirps_annsum)

# looks fine