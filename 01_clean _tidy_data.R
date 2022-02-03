library(tidyverse)
library(zoo)

# Wrangle data for daily arrivals over time
idf <- read_csv("data/individual_travel_data_limited_1_20_22.csv")

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

## CLEAN DATES

idf <- idf %>%
  separate(day_m_d_yy, into = c("month", "day", "year"), sep = "/", remove = FALSE)

# clean departure years
idf <- idf %>%
  mutate(year = case_when(year == "2022" ~ "2022", #"2022" %in% arrival_datetime ~ "2022",
                          year == "22" ~ "2022", #this might not do anything
                          year == "22022" ~ "2022",
                          year == "2021" ~ "2021",
                          year == "21" ~ "2021",
                          is.na(year) ~ "",
                          TRUE ~ "2021"))

idf <- idf %>%
  unite("departure_date", c("month", "day", "year"), sep = "/", na.rm = TRUE)

idf <- idf %>%
  mutate(departure_date = as.Date(departure_date, format = "%m/%d/%Y"))

# Add stay length column
idf <- idf %>%
  mutate(stay_length = departure_date - arrival_date)

## CLEAN STATE AND CITY NAMES

idf <- idf %>% 
  mutate(destination = case_when(destination == "Z" ~ "",
                                 destination == "Zip code not found" ~ "",
                                 destination == "none listed" ~ "",
                                 destination == "not recorded" ~ "",
                                 destination == "Washington DC" ~ "District of Columbia",
                                 destination == "Washington, D.C." ~ "District of Columbia",
                                 TRUE ~ destination))

# make two new columns from destination column to hold state name and abbreviation
idf <- idf %>%
  separate(destination, sep = "\\s?[-,] ", into = c("state_name", "state_abb"), 
           remove = FALSE)

# Fix state_abb values
idf <- idf %>% 
  mutate(state_abb = case_when(state_abb == "Texas" ~ "TX", 
                               state_abb == "Fl" ~ "FL",
                               state_abb == "Massachusetts" ~ "MA",
                               TRUE ~ state_abb))

# create city column, extracting values from state column
idf <- idf %>%
  mutate(city = ifelse(state_name %in% state.name,
                       NA, state_name),
         state_name = ifelse(state_name %in% state.name,
                             state_name, NA))

# make dataframe with state names and abbs (matching state_abb column)
state_info_1 <- data.frame(state_names = state.name,
                         state_abb = state.abb)

# left join, adding state_names column with missing values from abbs
idf <- idf %>%
  left_join(state_info_1)

#make new data frame with state names and abbs (matching state_name column)
state_info_2 <- data.frame(state_name = state.name,
                         state_abbs = state.abb)

#left join, adding state_abbs column with missing values from names
idf <- idf %>%
  left_join(state_info_2)

# complete state_name and State_abb columns with new values from state_names/_abbs
idf <- idf %>%
  mutate(state_name = ifelse(is.na(state_name), state_names, state_name),
         state_abb = ifelse(is.na(state_abb), state_abbs, state_abb))

#inspect results
idf %>%
  count(state_name, state_abb) %>%
  View()

idf <- idf %>%
  select(- c(state_names, state_abbs))

# Unite city with destination_city
idf <- idf %>%
  unite("destination_city", c(city, destination_city), na.rm = TRUE)

# clean city names 
idf <- idf %>%
  mutate(destination_city = case_when(destination_city == "Z" ~ "",
                                      destination_city == "Mexico" ~ "",
                                      destination_city == "_none listed" ~ "",
                                      destination_city == "_Z" ~ "",
                                      destination_city == "_not recorded" ~ "", 
                                      destination_city == "_Zip code not found" ~ "",
                                      destination_city == "Metarie" ~ "Metairie",
                                      destination_city == "Marborough" ~ "Marlborough",
                                      destination_city == "Kissimee" ~ "Kissimmee",
                                      destination_city == "Homested" ~ "Homestead",
                                      destination_city == "Holliwood" ~ "Hollywood",
                                      destination_city == "Ft Mitchell" ~ "Fort Mitchell",
                                      destination_city == "Farmingham" ~ "Framingham",
                                      destination_city == "District of Columbia_Washington" ~ "District of Colombia",
                                      destination_city == "_Pequannock" ~ "Pequannock",
                                      destination_city == "_Shelbyville" ~ "Shelbyville",
                                      destination_city == "_Browns Summit" ~ "Brown's Summit",
                                      TRUE ~ destination_city))

idf %>%
  count(destination_city) %>%
  View()

write_csv(idf, "data/individual_clean_dest_dates_1_20_22.csv")

# Calculate percentages of stay lengths
stay_len_perc <- idf %>%
  filter(stay_length >= 0) %>%
  count(stay_length) %>%
  mutate(percent = (n/sum(n))*100)
