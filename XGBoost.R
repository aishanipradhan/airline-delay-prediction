# First, run all cells in delay_prediction.qmd to load data
set.seed(67)

library(xgboost)
library(pROC)

# Create model matrices
x_train <- model.matrix(DEP_DEL15 ~ . - YEAR, data = train_data)[, -1]
y_train <- train_data$DEP_DEL15

x_test <- model.matrix(DEP_DEL15 ~ . - YEAR, data = test_data)[, -1]
y_test <- test_data$DEP_DEL15

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
  roc(y_test, xgb_pred)
)

print(xgb_auc)