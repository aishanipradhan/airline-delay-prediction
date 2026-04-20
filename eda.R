# Read data
library(tidyverse)
flights2023 <- read_csv("data/flights2023.csv")
flights2024 <- read_csv("data/flights2024.csv")
flights2025 <- read_csv("data/flights2025_visible.csv")

# Combine
flights <- rbind(flights2023, flights2024, flights2025)

# Keep only departing PIT flights if arrivals are included
# Remove cancelled and diverted flights
flights <- flights %>%
  filter(ORIGIN == "PIT",
         CANCELLED == 0,
         DIVERTED == 0)

# Delays by destination
flights |>
  ggplot(aes(y = DEST, x = DEP_DEL15)) +
  geom_bar(stat = "summary", fun = "mean")

# Delays by state
flights |>
  ggplot(aes(y = DEST_STATE_ABR, x = DEP_DEL15)) +
  geom_bar(stat = "summary", fun = "mean")

# Delays by distance
flights |>
  ggplot(aes(x = DISTANCE, y = DEP_DEL15)) +
  geom_point() +
  geom_smooth()

# Delays by month
flights |>
  ggplot(aes(y = as.factor(MONTH), x = DEP_DEL15)) +
  geom_bar(stat = "summary", fun = "mean")

# Delays by season
flights |>
  mutate(SEASON = case_when(
    MONTH %in% c(12, 1, 2) ~ "Winter",
    MONTH %in% c(3, 4, 5) ~ "Spring",
    MONTH %in% c(6, 7, 8) ~ "Summer",
    MONTH %in% c(9, 10, 11) ~ "Fall"
  )) |>
  ggplot(aes(y = as.factor(SEASON), x = DEP_DEL15)) +
  geom_bar(stat = "summary", fun = "mean")

# Delays by day of week
flights |>
  ggplot(aes(y = as.factor(DAY_OF_WEEK), x = DEP_DEL15)) +
  geom_bar(stat = "summary", fun = "mean")

# Delays by day of month
flights |>
  ggplot(aes(y = as.factor(DAY_OF_MONTH), x = DEP_DEL15)) +
  geom_bar(stat = "summary", fun = "mean")

# Delays by scheduled hours since midnight
flights |>
  mutate(DEP_HOUR = CRS_DEP_TIME %/% 100,
         DEP_MIN = CRS_DEP_TIME %% 100,
         DEP_HOURS_SINCE_MIDNIGHT = DEP_HOUR + DEP_MIN / 60) |>
  ggplot(aes(x = DEP_HOURS_SINCE_MIDNIGHT, y = DEP_DEL15)) +
  geom_point() +
  geom_smooth()

# Delays by carrier
flights |>
  ggplot(aes(y = OP_CARRIER, x = DEP_DEL15)) +
  geom_bar(stat = "summary", fun = "mean")
