# First, run all cells in delay_prediction.qmd to load data
library(ranger)
library(caret)
set.seed(123)

# Hyperparameter grid
grid <- expand.grid(
  mtry = c(3, 5),
  splitrule = c("gini"),
  min.node.size = c(10, 25, 50)
)

# Fit cross validation on grid
cv <- train(
  DEP_DEL15 ~ . - YEAR,
  data = train_data,
  method = "ranger",
  tuneGrid = grid,
  num.trees = 500
)

# Save the CV results
saveRDS(cv, "RandomForest_CV.rds")

# Train the best model
rf <- ranger(
  DEP_DEL15 ~ . - YEAR,
  data = train_data,
  num.trees = 500,
  mtry = 5,
  min.node.size = 50,
  probability = TRUE
)

predictions_test <- predict(rf, data = test_data)

library(pROC)
# Calculate AUC
roc_object <- roc(
  response = test_data$DEP_DEL15,
  predictor = predictions_test$predictions[, "1"]
)
