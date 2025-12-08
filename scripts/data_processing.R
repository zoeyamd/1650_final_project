#processing data prior to main analysis

library(tidyverse)
library(dplyr)

ridership_data <- read.csv("/Users/zoeybug/Documents/GitHub/1650_final_project/data/ridership_simulated.csv")
otp_data <- read.csv("/Users/zoeybug/Documents/GitHub/1650_final_project/data/otp_simulated.csv")

#removing error routes and trips
ridership_data <- ridership_data %>%
  filter(!Route %in% c(99, 999), !Trip %in% c(99, 999))

#formatting date
ridership_data <- ridership_data %>%
  mutate(datetime_obj = parse_date_time(Time, orders = c("mdy HM", "mdy")),
         date_only = as_date(datetime_obj),
         #making Day only column
         Day = day(date_only)) %>%
  select(-c(datetime_obj, date_only))

#identifying incident
otp_data <- otp_data %>%
  mutate(Delay.Min = Delay.Sec/60,
         Incident = case_when(Delay.Min < -1 ~ "Early",
                              Delay.Min > 5 ~ "Late",
                              TRUE ~ "On-Time"))

#identifying stop trip frequenecy 
frequency_thresholds <- trip_frequency %>%
  pull(Trips_Served) %>%
  # Calculate the 33rd and 66th percentiles, ignoring any NA values
  quantile(c(0.33, 0.66), na.rm = TRUE)

p33_threshold <- round(frequency_thresholds["33%"]) # 33rd percentile cutoff
p66_threshold <- round(frequency_thresholds["66%"]) # 66th percentile cutoff

#create frequency columns
stop_frequency_categorized <- trip_frequency %>%
  mutate(
    Frequency_Category = case_when(
      # Category 1: High Frequency (Trips_Served is greater than or equal to the 66th percentile)
      Trips_Served >= p66_threshold ~ "High Frequency (Peak Core)",
      
      # Category 2: Medium Frequency (Trips_Served is greater than or equal to the 33rd percentile, but less than p66)
      Trips_Served >= p33_threshold ~ "Medium Frequency (Base Core)",
      
      # Category 3: Low Frequency (Everything else, which is below the 33rd percentile)
      TRUE ~ "Low Frequency (Tail/Coverage)"
    )
  )
