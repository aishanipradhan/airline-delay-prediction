# First, run all cells in delay_prediction.qmd to load data
library(ranger)
library(caret)
set.seed(123)

# Hyperparameter grid
grid <- expand.grid(
  mtry = c(3, 5),
  splitrule = c("gini"),
  min.node.size = c(5, 10, 25)
)

# Fit cross validation on grid
cv <- train(
  DEP_DEL15 ~ . - YEAR,
  data = train_data,
  method = "ranger",
  tuneGrid = grid,
  num.trees = 500
)

library(pROC)
