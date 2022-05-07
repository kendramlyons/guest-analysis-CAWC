########################## CLEAN TRAVEL DATA ###################################

# This script cleans variables selected from the travel data spreadsheet 
# (dates, states, cities, languages and countries of origin) 
# and aggregates data into tables for visualization

################# Replace dates in file names before running! ###################

# Load Libraries
library(tidyverse)
#library(zoo::rollmean)
library(lubridate)

## Read in Data
df <- read_csv("data/individual_less_raw_limited_1_20_22.csv") #%>%
  #rename("group_id" = ...1)# Rename first column (index)


## CLEAN DATES

# separate departure dates to fix year errors
df <- df %>%
  separate(day_m_d_yy, into = c("month", "day", "year"), 
           sep = "/", remove = FALSE) %>%
  mutate(year = case_when(year == "2022" ~ "2022", # clean departure years
                          #year == "22" ~ "2022",
                          year == "22022" ~ "2022",
                          year == "2021" ~ "2021",
                          #year == "21" ~ "2021", 
                          is.na(year) ~ "",
                          TRUE ~ "2021"))
# re-join cleaned dates
df <- df %>%
  unite("departure_date", c("month", "day", "year"), 
        sep = "/", na.rm = TRUE, remove = FALSE) %>% # convert cleaned date stings into Date type
  mutate(departure_date = as.Date(departure_date, 
                                  format = "%m/%d/%Y"))


# GET STAY LENGTH
# create new column with difference b/w arrival date and departure date
df <- df %>%
  mutate(stay_length = departure_date - arrival_date)

# Calculate percentages of stay lengths
stay_len_perc <- df %>%
  filter(stay_length >= 0) %>%
  count(stay_length, number_in_party) %>% #added number in party
  mutate(percent = (n/sum(n))*100,
         label = case_when(stay_length == 5 ~ "COVID-19 Re-testing",
                           stay_length == 10 ~ "COVID-19 Re-testing"),
         peak = case_when(stay_length == 1.0 ~ n,
                          stay_length == 6.0 ~ n,
                          stay_length == 11.0 ~ n,
                          stay_length == 16.0 ~ n)) 

# save data
write_csv(stay_len_perc, "data/stay_length_percent_gps_1_20_22.csv")

# GET WEEK#, MONTH# AND YEAR
# get week number, month number and year
df <- df %>% 
  mutate(week_number = week(arrival_date),
         arrival_year = year(arrival_date),
         arrival_month = month(arrival_date))

max(df$week_number) # how many weeks?

df <- df %>% # changes week numbers in 2022 so they don't overlap with 2021
  mutate(week_number = case_when(week_number == 1 ~ 54,
                                 week_number == 2 ~ 55,
                                 week_number == 3 ~ 56,
                                 is.na(week_number) ~ 0,
                                 TRUE ~ week_number),
         arrival_month = if_else((arrival_month == 1 & arrival_year == 2022), 
                                 13, arrival_month))

## GET ARRIVAL TIMELINE DATA (ROLLING 7 DAY MEAN)

# create new dataframe with number of individual arrivals per day
dt.cts <- df %>%
  count(arrival_date, week_number, arrival_month) %>% 
  # add new column with rolling 7-day-average arrivals
  mutate(seven_avg = zoo::rollmean(n, 7, align = 'center', fill = 0),
         label = case_when(arrival_date == "2021-01-26" ~ 
                             "January 26, 2021: Welcome Center begins recieving guests again",
                           arrival_date == "2022-01-01" ~
                             "January 1, 2022: Peak daily arrivals, 326 individuals")) 

# replace zeros at beginning with average number of arrivals for first three days (2)
dt.cts$seven_avg[1:3] <- mean(dt.cts$n[1:3])

# replace zeros at end with average from closest date
dt.cts$seven_avg[351:353] <- mean(dt.cts$n[351:353]) #change index for new data

# save data to disk 
write_csv(dt.cts, "data/arrival_date_counts_1.20.csv")


wk.cts <- df %>% # add start date ?
  count(week_number) %>%
  mutate(label = case_when(week_number == 13 ~ n,
                           week_number == 27 ~ n,
                           week_number == 39 ~ n,
                           week_number == 42 ~ n,
                           week_number == 49 ~ n,
                           week_number == 52 ~ n,
                           week_number == 53 ~ n))

write_csv(wk.cts, "data/arrival_week_counts_1.20.csv")


## CLEAN STATES

# replace unknown values and fix D.C.
df <- df %>% 
  mutate(destination = case_when(destination == "Z" ~ "",
                                 destination == "Zip code not found" ~ "",
                                 destination == "none listed" ~ "",
                                 destination == "not recorded" ~ "",
                                 destination == "Washington DC" ~ "District of Columbia",
                                 destination == "Washington, D.C." ~ "District of Columbia",
                                 TRUE ~ destination))

# split destination column into state name and abbreviation
df <- df %>%
  separate(destination, sep = "\\s?[-,] ", 
           into = c("state_name", "state_abb"), 
           remove = FALSE)

# Fix state_abb values
df <- df %>% 
  mutate(state_abb = case_when(state_abb == "Texas" ~ "TX", 
                               state_abb == "Fl" ~ "FL",
                               state_abb == "Massachusetts" ~ "MA",
                               destination == "District of Columbia" ~ "DC",
                               TRUE ~ state_abb))

# create city column, extracting values from state column
df <- df %>%
  mutate(city = ifelse(state_name %in% state.name,
                       NA, state_name),
         state_name = ifelse(state_name %in% state.name,
                             state_name, NA))

# add District of Columbia to built in state info 
state.abb[51] <- "DC"
state.name[51] <- "District of Columbia"

# make dataframe with state names and abbs (matching state_abb column)
state_info_1 <- data.frame(state_name = state.name,
                           state.abb = state.abb) #,state_abb = state.abb
# left join, adding state_names column with missing values from abbs
df <- df %>%
  left_join(state_info_1)

# make new data frame with state names and abbs (matching state_name column)
state_info_2 <- data.frame(state.name = state.name,
                         state_abb = state.abb) ## this might be why I don't ahve all abbs

# left join, adding state_abbs column with missing values from names
df <- df %>%
  left_join(state_info_2)

# complete state_name and State_abb columns with new values from state_names/_abbs
df <- df %>%
  mutate(state_name = ifelse(is.na(state_name), state.name, state_name),
         state_abb = ifelse(is.na(state_abb), state.abb, state_abb))

# inspect results
df %>%
  count(state_name, state_abb) %>%
  View()

df <- df %>%
  select(-c(state.name, state.abbs))


# number of arrivals by Destination
unique(df$state_name)

dst.cts <- df %>%
  count(state_name, state_abb)
dst.cts$percent <- round((dst.cts$n/sum(dst.cts$n))*100, 2)
dst.cts <- arrange(dst.cts, -n)

dst.cts <- drop_na(dst.cts)

# save data
write_csv(dst.cts, "data/destination_counts_1.20.csv")


## CLEAN CITIES

# Unite city with destination_city
df <- df %>%
  unite("destination_city", c(city, destination_city), na.rm = TRUE)

# clean city names 
df <- df %>%
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
                                      destination_city == "District of Columbia_Washington" ~ "District of Columbia",
                                      destination_city == "_Pequannock" ~ "Pequannock",
                                      destination_city == "_Shelbyville" ~ "Shelbyville",
                                      destination_city == "_Browns Summit" ~ "Brown's Summit",
                                      TRUE ~ destination_city))

df %>%
  count(destination_city)


## CLEAN LANGUAGES

df <- df %>% # deal with missing data
  mutate(language = case_when( language == '00' ~ '',
                               language == 'Contact_' ~ '',
                               language == 'N/A' ~ '',
                               language == 'Not Listed (add to notes)' ~ '',
                               # indigenous, speaks English?
                               TRUE ~ language))

# remove (Country) from language column by separating language on space 
df <- df %>%
  separate(language, sep = '\\s\\(', into = c('language', 'country'), remove = TRUE)

# separate language column on forward slash
df <- df %>%
  separate(language, sep = '\\/', into = c('language', 'language2'), remove = TRUE)

# clean up language spelling and remaining multiple entries
df <- df %>%
  mutate(language = case_when(  language == 'Cho Ol' ~ "Ch'ol",
                                language == 'Haitian creole' ~ 'Creole', # Portuguese creole, too
                                language == 'indigenous' ~ '',
                                language == "Q'eqchi" ~ "Q'eqchi'",
                                language == 'Quanjobal' ~ "Q'anjob'al",
                                language == 'Kichwa' ~ 'Quichua',
                                language == 'speaks English' ~ 'English',
                                language == 'Tzotzil Maya' ~ 'Tzeltal Maya', 
                                language == 'Telugu and English' ~ 'Telugu',
                                TRUE ~ language))

# add english to language2
df <- df %>%
  mutate(language2 = if_else(language == 'Telugu', 'English', language2))


# number of arrivals by language
unique(df$language)

#
lang.cts <- df %>%
  count(language)

# add percentage column
lang.cts$percent <- round((lang.cts$n/sum(lang.cts$n))*100, 2)
lang.cts <- arrange(lang.cts, -n)

lang.cts <- drop_na(lang.cts) %>% filter(language != "")

write_csv(lang.cts, "data/language_counts_1.20.csv")


## CLEAN ORIGIN cOUNTRY NAMES

df <- df %>%
  mutate(country_of_origin = case_when(country_of_origin == 'Contact_' ~ '',
                                       country_of_origin == 'Not Identified' ~ '',
                                       country_of_origin == 'Pakistaan' ~ 'Pakistan',
                                       TRUE ~ country_of_origin))

df <- df %>%
  separate(country_of_origin, sep = '[/,]', into = c('country', 'country2')) 


# number of arrivals by Country
unique(df$country) # 26 Countries on 8.1

cntry.cts <- df %>%
  count(country)

cntry.cts$percent <- round((cntry.cts$n/sum(cntry.cts$n))*100, 2)

cntry.cts <- arrange(cntry.cts, -n)

cntry.cts <- drop_na(cntry.cts) %>% filter(country != "")

write_csv(cntry.cts, "data/country_counts_1.20.csv")


# Save Data to Disk

write_csv(df, "data/individual_all_clean_1_20_22.csv")



