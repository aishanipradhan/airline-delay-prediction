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

# Fit lasso logistic regression with cross-validation
cv_lasso <- cv.glmnet(
  x_train,
  y_train,
  family = "binomial",
  alpha = 1
)

# Best lambda
best_lambda <- cv_lasso$lambda.min

# Fit final lasso model
lasso_model <- glmnet(
  x_train,
  y_train,
  family = "binomial",
  alpha = 1,
  lambda = best_lambda
)

# Predicted probabilities on test set
lasso_pred <- predict(
  lasso_model,
  newx = x_test,
  type = "response"
)

# AUC
auc(
  roc(
    response = test_data$DEP_DEL15,
    predictor = as.numeric(lasso_pred),
    levels = c("0", "1"),
    direction = "<"
  )
)