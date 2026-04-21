library(readr)

flights <- read_csv("data/flights.csv")

# Read 2025 visible flights
flights2025 <- read_csv("data/flights2025_visible.csv")

# Keep only PIT departures and non-cancelled/non-diverted flights
arrivals2025 <- flights2025 %>%
  filter(
    DEST == "PIT",
    CANCELLED == 0,
    DIVERTED == 0
  )

flights2025 <- flights2025 %>%
  filter(
    ORIGIN == "PIT",
    CANCELLED == 0,
    DIVERTED == 0
  )

#Lookup tables 
carrier_lookup <- flights %>%
  select(OP_UNIQUE_CARRIER, carrier_delay_rate) %>%
  distinct()

carrier_route_lookup <- flights %>%
  select(OP_UNIQUE_CARRIER, DEST, carrier_route_delay_rate) %>%
  distinct()

dest_lookup <- flights %>%
  select(DEST, dest_delay_rate) %>%
  distinct()

carrier_month_lookup <- flights %>%
  select(OP_UNIQUE_CARRIER, MONTH, carrier_month_delay_rate) %>%
  distinct()

# Join 2023-2024 data onto 2025 flights
flights2025 <- flights2025 %>%
  left_join(carrier_lookup, by = "OP_UNIQUE_CARRIER") %>%
  left_join(carrier_route_lookup,
            by = c("OP_UNIQUE_CARRIER", "DEST")) %>%
  left_join(dest_lookup, by = "DEST") %>%
  left_join(carrier_month_lookup,
            by = c("OP_UNIQUE_CARRIER", "MONTH"))

#Impute/drop missing features
flights2025 <- flights2025 %>%
  filter(DEST != "SDF")

flights2025 <- flights2025 %>%
  mutate(
    carrier_route_delay_rate = coalesce(
      carrier_route_delay_rate,
      carrier_delay_rate
    )
  )

#Recalculate some features 

# Season
flights2025 <- flights2025 %>%
  mutate(
    SEASON = case_when(
      MONTH %in% c(12, 1, 2) ~ "Winter",
      MONTH %in% c(3, 4, 5) ~ "Spring",
      MONTH %in% c(6, 7, 8) ~ "Summer",
      MONTH %in% c(9, 10, 11) ~ "Fall"
    )
  )

# Departure time features
flights2025 <- flights2025 %>%
  mutate(
    DEP_HOUR = CRS_DEP_TIME %/% 100,
    DEP_MIN = CRS_DEP_TIME %% 100,
    DEP_HOURS_SINCE_MIDNIGHT = DEP_HOUR + DEP_MIN / 60
  )

# Sort chronologically
flights2025 <- flights2025 %>%
  arrange(FL_DATE, CRS_DEP_TIME)

# Carrier delay rate earlier that same day
flights2025 <- flights2025 %>%
  group_by(FL_DATE, OP_UNIQUE_CARRIER) %>%
  mutate(
    carrier_day_delay_rate =
      lag(
        cumsum(DEP_DEL15 == 1 & coalesce(CARRIER_DELAY, 0) > 0),
        default = 0
      ) / pmax(row_number() - 1, 1),
    carrier_day_delay_rate =
      if_else(row_number() == 1, 0, carrier_day_delay_rate)
  ) %>%
  ungroup()

# Weather delay events in previous 3 hours
flights2025 <- flights2025 %>%
  arrange(FL_DATE, DEP_HOURS_SINCE_MIDNIGHT) %>%
  group_by(FL_DATE) %>%
  mutate(
    weather_event = as.integer(
      DEP_DEL15 == 1 & coalesce(WEATHER_DELAY, 0) > 0
    ),
    
    weather_delay_rate_3hr = slide_index_dbl(
      weather_event,
      DEP_HOURS_SINCE_MIDNIGHT,
      ~ mean(head(.x, -1)),
      .before = 3,
      .complete = FALSE
    ),
    
    weather_delay_rate_3hr = coalesce(weather_delay_rate_3hr, 0)
  ) %>%
  ungroup()

# Weekend / long-weekend feature
flights2025 <- flights2025 %>%
  mutate(
    weekend_group = case_when(
      DAY_OF_WEEK %in% c(5, 6, 7, 1) ~ "Long Weekend",
      DAY_OF_WEEK %in% c(2, 3, 4) ~ "Weekday"
    )
  )

# Prepare arrivals time variables
arrivals2025 <- arrivals2025 %>%
  mutate(
    ARR_HOUR = ARR_TIME %/% 100,
    ARR_MIN = ARR_TIME %% 100,
    ARR_HOURS_SINCE_MIDNIGHT = ARR_HOUR + ARR_MIN / 60
  )

# Initialize columns
flights2025$is_first <- 0
flights2025$is_incoming_delayed <- 0
flights2025$turnaround_time <- 0

# Calculate first-flight, incoming delay, turnaround
for (i in 1:nrow(flights2025)) {
  
  flight <- flights2025[i, ]
  
  same_flight_arrival <- arrivals2025 %>%
    filter(
      FL_DATE == flight$FL_DATE,
      TAIL_NUM == flight$TAIL_NUM,
      ARR_HOURS_SINCE_MIDNIGHT < flight$DEP_HOURS_SINCE_MIDNIGHT
    ) %>%
    slice_max(
      ARR_HOURS_SINCE_MIDNIGHT,
      n = 1,
      with_ties = FALSE
    )
  
  if (nrow(same_flight_arrival) == 0) {
    
    flights2025$is_first[i] <- 1
    flights2025$is_incoming_delayed[i] <- 0
    flights2025$turnaround_time[i] <- 0
    
  } else {
    
    flights2025$is_first[i] <- 0
    flights2025$is_incoming_delayed[i] <- same_flight_arrival$ARR_DEL15
    flights2025$turnaround_time[i] <-
      flight$DEP_HOURS_SINCE_MIDNIGHT -
      same_flight_arrival$ARR_HOURS_SINCE_MIDNIGHT
  }
}

# Rename datasets
train_data <- flights
test_data <- flights2025

# Recode turnaround time for first flight of the day
train <- train_data %>%
  mutate(
    turnaround_time = ifelse(is_first == 1, -999, turnaround_time)
  )

test_data <- test_data %>%
  mutate(
    turnaround_time = ifelse(is_first == 1, -999, turnaround_time)
  )

train_data <- train_data %>%
  select(
    YEAR,
    SEASON,
    DEST_STATE_ABR,
    DISTANCE,
    DEP_HOURS_SINCE_MIDNIGHT,
    carrier_delay_rate,
    carrier_day_delay_rate,
    carrier_route_delay_rate,
    dest_delay_rate,
    carrier_month_delay_rate,
    weather_delay_rate_3hr,
    is_first,
    is_incoming_delayed,
    turnaround_time,
    DEP_DEL15
  ) %>%
  mutate(
    is_first = as.factor(is_first),
    is_incoming_delayed = as.factor(is_incoming_delayed),
    DEP_DEL15 = as.factor(DEP_DEL15)
  )

test_data <- test_data %>%
  select(
    YEAR,
    SEASON,
    DEST_STATE_ABR,
    DISTANCE,
    DEP_HOURS_SINCE_MIDNIGHT,
    carrier_delay_rate,
    carrier_day_delay_rate,
    carrier_route_delay_rate,
    dest_delay_rate,
    carrier_month_delay_rate,
    weather_delay_rate_3hr,
    is_first,
    is_incoming_delayed,
    turnaround_time,
    DEP_DEL15
  ) %>%
  mutate(
    is_first = as.factor(is_first),
    is_incoming_delayed = as.factor(is_incoming_delayed),
    DEP_DEL15 = as.factor(DEP_DEL15)
  )