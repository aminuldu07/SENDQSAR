# XGBoost Model
xgboost_model <- function(rfData) {
  library(xgboost)

  # Convert data to a format suitable for XGBoost
  trainIndex <- sample(seq_len(nrow(rfData)), size = 0.7 * nrow(rfData))
  train <- rfData[trainIndex, ]
  test <- rfData[-trainIndex, ]

  # Prepare matrices for XGBoost
  train_matrix <- xgb.DMatrix(data = as.matrix(train[,-1]), label = train$Target_Organ)
  test_matrix <- xgb.DMatrix(data = as.matrix(test[,-1]), label = test$Target_Organ)

  # Train the model
  model <- xgboost(data = train_matrix,
                   max_depth = 6,
                   eta = 0.3,
                   nrounds = 100,
                   objective = "binary:logistic")

  # Predict on the test set
  predictions <- predict(model, test_matrix)
  pred_class <- ifelse(predictions > 0.5, 1, 0)

  # Confusion Matrix
  caret::confusionMatrix(factor(pred_class), factor(test$Target_Organ))
}

# Run XGBoost
#xgboost_model(rfData)
