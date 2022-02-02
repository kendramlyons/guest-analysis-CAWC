library(tidyverse)
library(zoo)

# Wrangle data for daily arrivals over time
idf <- read_csv("data/individual_travel_data_limited_1.20.csv")

# Rename first column 
idf <- idf %>%
  rename("group_id" = ...1)

# create new dataframe with number of individual arrivals per day
dt.cts <- idf %>%
  count(arrival_date)

# add new column with rolling 7-day-average arrivals
dt.cts <- dt.cts %>%
  mutate(seven_avg = rollmean(n, 7, align='center', fill=0))

# replace zeros at beginning with average number of arrivals for first three days (2)
dt.cts$seven_avg[1:3] <- 2

# replace zeros at end with average from closest date
# REWRITE TO USE NEGATIVE INDEXING FOR LAST THREE!
dt.cts$seven_avg[351:353] <- dt.cts$seven_avg[350] #change index for new data

# Save data to disk 
write_csv(dt.cts, "data/arrival_date_counts_1.20.csv")


