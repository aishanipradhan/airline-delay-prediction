# First, run all cells in delay_prediction.qmd to load data
library(ranger)
set.seed(123)

# Fit a model (no regularization)
rf <- ranger(DEP_DEL15 ~ ., data = flights)

# Test model on visible 2025 flights
flights2025_visible <- read_csv("data/flights2025_visible.csv") |>
  
  # Correct data types
  mutate(
    FL_DATE = parse_date_time(FL_DATE, orders = "mdY IMS p"),
    FL_DATE = as.Date(FL_DATE),
    
    YEAR = as.factor(YEAR),
    MONTH = as.factor(MONTH),
    DAY_OF_WEEK = as.factor(DAY_OF_WEEK),
    
    OP_UNIQUE_CARRIER = as.factor(OP_UNIQUE_CARRIER),
    DEST = as.factor(DEST),
    DEST_STATE_ABR = as.factor(DEST_STATE_ABR),
    
    # DEP_DEL15 = as.factor(DEP_DEL15)
  ) |>
  
  # Compute carrier day delay rate
  filter(!is.na(DEP_DEL15)) %>%
  arrange(FL_DATE, OP_UNIQUE_CARRIER, CRS_DEP_TIME) %>%
  group_by(FL_DATE, OP_UNIQUE_CARRIER) %>%
  mutate(
    carrier_day_delay_rate =
      lag(cumsum(DEP_DEL15), default = 0) /
      pmax(row_number() - 1, 1),
    n_flights_day = n()
  ) %>%
  mutate(
    carrier_day_delay_rate = if_else(row_number() == 1, 0, carrier_day_delay_rate)
  ) %>%
  ungroup()

# Join delay rates from 23-24, fill missing ones with 0
carrier_dest_rates <- flights |>
  select(OP_UNIQUE_CARRIER, DEST, carrier_route_delay_rate) |>
  distinct()
dest_rates <- flights |>
  select(DEST, dest_delay_rate) |>
  distinct()
carrier_month_rates <- flights |>
  select(OP_UNIQUE_CARRIER, MONTH, carrier_month_delay_rate) |>
  distinct()
carrier_rates <- flights |>
  select(OP_UNIQUE_CARRIER, carrier_delay_rate) |>
  distinct()

flights2025_visible <- flights2025_visible |>
  left_join(carrier_dest_rates, by = c("OP_UNIQUE_CARRIER", "DEST")) |>
  left_join(dest_rates, by = "DEST") |>
  left_join(carrier_month_rates, by = c("OP_UNIQUE_CARRIER", "MONTH")) |>
  left_join(carrier_rates, by = "OP_UNIQUE_CARRIER") |>
  mutate(across(ends_with("_delay_rate"), ~coalesce(.x, 0)))

# Check if any NAs
colSums(is.na(flights2025_visible))

predictions_2025 <- predict(rf, data = flights2025_visible)

# Brier score (MSE)
mean((predictions_2025$predictions - flights2025_visible$DEP_DEL15)^2)
