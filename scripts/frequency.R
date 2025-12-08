#identifying unique # of days each trip occurs for averaging
distinct_days <- ridership_data %>% 
  group_by(Route, Trip, Stop.Number) %>%
  summarize(distinct_days = length(unique(Day)), .groups = 'drop')

#total riders
total_riders <- ridership_data %>%
  group_by(Trip, Route, Stop.Number) %>%
  summarize(total_riders = sum(Ride.Count), .groups = 'drop')

#average ridership
average_ridership <- total_riders %>%
  left_join(distinct_days, by = c("Route", "Trip", "Stop.Number")) %>%
  mutate(avg_riders_per_day = total_riders / distinct_days)

combined_data <- average_ridership %>%
  left_join(otp_data, by = c("Route"))

ridership_by_status <- combined_data %>%
  group_by(Incident) %>%
  summarize(
    Average_Boardings = mean(total_riders, na.rm = TRUE),
    Median_Boardings = median(total_riders, na.rm = TRUE),
    Total_Trips = n(),
    .groups = 'drop'
  )


trip_frequency <- ridership_data %>%
  group_by(Route, Stop.Number, Day) %>%
  summarise(Trips_Served = length(unique(Trip)), .groups = 'drop')
  
  group_by(Route, Stop.Number, Day) %>%
  summarise(length(unique(Trip)))
