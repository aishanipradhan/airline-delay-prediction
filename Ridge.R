# First, run all cells in delay_prediction.qmd to load data
set.seed(67)

library(glmnet)
library(pROC)

# Formula with interactions
form <- DEP_DEL15 ~ . - YEAR +
  is_first:is_incoming_delayed +
  is_first:turnaround_time

# Training matrix
x_train <- model.matrix(form, data = train_data)[, -1]
y_train <- train_data$DEP_DEL15

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