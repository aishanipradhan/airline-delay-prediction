# First, run all cells in delay_prediction.qmd to load data
set.seed(67)

library(glmnet)
library(pROC)

# Create model matrices
x_train <- model.matrix(DEP_DEL15 ~ . - YEAR, data = train_data)[, -1]
y_train <- train_data$DEP_DEL15

x_test <- model.matrix(DEP_DEL15 ~ . - YEAR, data = test_data)[, -1]

# Fit ridge logistic regression with cross-validation
cv_ridge <- cv.glmnet(
  x_train,
  y_train,
  family = "binomial",
  alpha = 0
)

# Best lambda
best_lambda <- cv_ridge$lambda.min

# Fit final ridge model
ridge_model <- glmnet(
  x_train,
  y_train,
  family = "binomial",
  alpha = 0,
  lambda = best_lambda
)

# Predicted probabilities on test set
ridge_pred <- predict(
  ridge_model,
  newx = x_test,
  type = "response"
)

# AUC
auc(
  roc(test_data$DEP_DEL15, as.numeric(ridge_pred))
)