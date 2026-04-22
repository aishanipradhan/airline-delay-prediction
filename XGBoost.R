# First, run all cells in delay_prediction.qmd to load data
set.seed(67)

library(xgboost)
library(pROC)

# Formula with interactions
form <- DEP_DEL15 ~ . - YEAR +
  is_first:is_incoming_delayed +
  is_first:turnaround_time

# Training matrix
x_train <- model.matrix(form, data = train_data)[, -1]
y_train <- as.numeric(as.character(train_data$DEP_DEL15))

# Test matrix
x_test <- model.matrix(form, data = test_data)[, -1]

# Add missing columns and align order
missing_cols <- setdiff(colnames(x_train), colnames(x_test))

for (col in missing_cols) {
  x_test <- cbind(x_test, 0)
  colnames(x_test)[ncol(x_test)] <- col
}

# Drop any extra columns and reorder
x_test <- x_test[, colnames(x_train)]
y_test  <- as.numeric(as.character(test_data$DEP_DEL15))

# Convert to xgboost matrix format
dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

# Set model parameters
params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 4,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Cross-validation to determine best number of rounds
cv_xgb <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 500,
  nfold = 5,
  early_stopping_rounds = 20,
  verbose = 0
)

# Best number of boosting rounds
best_nrounds <- cv_xgb$early_stop$best_iteration

# Fit final model
xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds,
  verbose = 0
)

# Predicted probabilities on test set
xgb_pred <- predict(
  xgb_model,
  newdata = dtest
)

# Calculate AUC
xgb_auc <- auc(
  roc(
    response = y_test,
    predictor = xgb_pred,
    levels = c(0, 1),
    direction = "<"
  )
)

print(xgb_auc)