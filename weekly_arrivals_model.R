# Weekly arrivals Boxplots/ Meal estimate re3gression?
library(tidyverse)
library(lubridate)

individuals <- read_csv("data/individual_clean_dest_dates_1_20_22.csv")

individuals <- individuals %>% # get week number, month number and year
  mutate(week_number = week(arrival_date),
         arrival_year = year(arrival_date),
         arrival_month = month(arrival_date))

max(individuals$week_number)

individuals <- individuals %>% #changes week numbers in 2022 so they don't overlap with 2021
  mutate(week_number = case_when(week_number == 1 ~ 54,
                                 week_number == 2 ~ 55,
                                 week_number == 3 ~ 56,
                                 is.na(week_number) ~ 0,
                                 TRUE ~ week_number),
         arrival_month = if_else((arrival_month == 1 & arrival_year == 2022), 
                                 13, arrival_month))

individuals %>% # basic plot of weekly arrivals
  ggplot(aes(x = week_number)) +
  geom_bar() 

# Examine daily arrivals by month
daily_arrivals <- individuals %>%
  count(arrival_date, arrival_month, week_number, arrival_year)

daily_arrivals %>% # daily arrivals by month boxplots
  mutate(arrival_month = factor(arrival_month)) %>%
  ggplot(aes(x = week_number, y = n)) +
  geom_boxplot(aes(group=arrival_month), varwidth = TRUE) +
  ylab("Number of Daily Arrivals") +
  xlab("Arrival Month Jan. 2021 (1) - Jan. 2022 (13)") +
  ggtitle("Average Daily Arrivals by Month") +
  theme(text = element_text(size=18))

# Examine monthly arrivals
monthly_arrivals <- individuals %>%
  count(week_number, arrival_month, arrival_year)

monthly_arrivals %>% #average weekly arrivals by month boxplots
  mutate(arrival_month = factor(arrival_month)) %>%
  ggplot(aes(x = arrival_month, y = n)) +
  geom_boxplot(varwidth = TRUE) +
  ylab("Number of Weekly Arrivals") +
  xlab("Arrival Month Jan. 2021 (1) - Jan. 2022 (13)") +
  ggtitle("Average Weekly Arrivals by Month") +
  theme(text = element_text(size=18))

# Examine weekly arrivals

monthly_arrivals %>% # Weekly arrivals by month boxplots
  mutate(week_number = factor(week_number)) %>%
  ggplot(aes(x = week_number, y = n)) +
  geom_boxplot(aes(group = arrival_month), varwidth = FALSE)

arrivals <- individuals %>%
  count(week_number)

arrivals %>% # weekly arrivals barplot with linear regression
  ggplot(aes(x = week_number, y = n)) +
  geom_bar(stat = "identity") +
  geom_smooth(method = "lm") +
  xlab("Arrival Week Number") +
  ylab("Weekly Arrivals")

# linear model of weekly arrivals 
weekly_arrivals_model <- lm(data = arrivals, n ~ week_number)
summary(weekly_arrivals_model)

predict(weekly_arrivals_model, data.frame(week_number = c(57,58,59)))

# linear model of arrivals after the first 10 weeks
recent_arrivals <- arrivals %>%
  filter(week_number > 10) 

recent_arrivals_model <- lm(data = recent_arrivals, n ~ week_number)  
summary(recent_arrivals_model) 

predict(recent_arrivals_model, data.frame(week_number = c(57,58,59)))

recent_arrivals %>% # weekly arrivals, linear regression, >10 weeks
  ggplot(aes(x = week_number, y = n)) +
  geom_bar(stat = "identity") +
  geom_smooth(method = "lm") +
  xlab("Arrival Week Number") +
  ylab("Weekly Arrivals")+
  theme(text = element_text(size=18)) +
  ggtitle("Weekly Arrivals Over Time (Linear Regression)")


stay_len <- individuals %>% # new data frame for stay length counts/percentages
  filter(stay_length >= 0) %>%
  count(stay_length) %>%
  mutate(percent = (n/sum(n))*100)

long_stay <- stay_len %>% # new data frame for those staying longer than 3 days
  filter(stay_length > 3) 

sum(long_stay$percent) # percentage staying longer than three days

