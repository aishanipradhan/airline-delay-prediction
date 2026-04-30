predictions <- load("AlcoholicsAnonymous462final.RData")
true <- read_csv("data/flights2025_true.csv")
test_data <- read_csv("data/test_data.csv")

library(pROC)
true_auc <- roc(
  response = true$DEP_DEL15,
  predictor = delay.guesses
)
auc(true_auc)

# Compare predicted to actual
test_data$DEL_PROB <- delay.guesses
test_data$residual <- true$DEP_DEL15 - test_data$DEL_PROB
test_data$actual <- true$DEP_DEL15

# Remove NAs
test_data <- test_data |> drop_na()

# Calibration plot
buckets <- test_data |>
  mutate(bucket = 0.1 * ceiling(DEL_PROB / 0.1) - 0.05) |>
  group_by(bucket) |>
  summarize(n_flights = n(),
            bin_prob = mean(actual),
            bin_se = sqrt((bin_prob * (1 - bin_prob)) / n_flights)) |>
  ungroup() |>
  mutate(bin_upper = pmin(bin_prob + 2 * bin_se, 1),
         bin_lower = pmax(bin_prob - 2 * bin_se, 0))

buckets |>
  ggplot(aes(x = bucket, y = bin_prob)) +
  geom_point() +
  geom_errorbar(aes(ymin = bin_lower, ymax = bin_upper)) +
  geom_abline(slope = 1, intercept = 0, 
              color = "black", linetype = "dashed") +
  coord_equal() + 
  scale_x_continuous(limits = c(0,1)) + 
  scale_y_continuous(limits = c(0,1)) + 
  labs(x = "Estimated Delay Probability",
       y = "Observed Delay Frequency") + 
  theme_bw() +
  theme(legend.position = "bottom")

# Compare residuals to turnaround time
test_data |>
  ggplot(aes(x = turnaround_time, y = residual)) +
  geom_point() +
  facet_wrap(~ actual)

# Compare residuals to state
test_data |>
  ggplot(aes(y = DEST_STATE_ABR, x = residual)) +
  geom_boxplot() +
  facet_wrap(~ actual)
