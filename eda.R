# Read data
library(tidyverse)
flights <- read_csv("data/flights_all.csv")

# Delays by day of week
flights |>
  ggplot(aes(y = as.factor(DAY_OF_WEEK), x = DEP_DEL15)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(
    y = "Day of the Week",
    x = "Proportion of Flights Delayed"
  ) +
  theme_bw()

# Delays by scheduled hours since midnight
flights |>
  ggplot(aes(y = DEP_TIME_BLK, x = DEP_DEL15)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(
    y = "Scheduled Departure Time Block",
    x = "Proportion of Flights Delayed"
  ) +
  theme_bw()

# Delays by carrier
flights |>
  ggplot(aes(y = OP_CARRIER, x = DEP_DEL15)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(
    y = "Airline",
    x = "Proportion of Flights Delayed"
  ) +
  theme_bw()

# Delays by state
flights |>
  ggplot(aes(y = DEST_STATE_ABR, x = DEP_DEL15)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(
    y = "Destination State",
    x = "Proportion of Flights Delayed"
  ) +
  theme_bw()

# Turnaround time
flights |>
  filter(is_first == 0) |>
  ggplot(aes(x = turnaround_time, y = DEP_DEL15)) +
  geom_point() +
  geom_smooth() +
  xlim(c(0, 2))

flights |>
  ggplot(aes(x = DEP_HOURS_SINCE_MIDNIGHT, y = is.na(weather_delay_rate_3hr))) +
  geom_point()
