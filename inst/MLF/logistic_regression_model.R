# Logistic Regression Model
logistic_model <- function(rfData) {
  # Ensure the target is a factor
  rfData$Target_Organ <- as.factor(rfData$Target_Organ)

  # Split data into training and testing sets
  set.seed(123)  # For reproducibility
  trainIndex <- sample(seq_len(nrow(rfData)), size = 0.7 * nrow(rfData))
  train <- rfData[trainIndex, ]
  test <- rfData[-trainIndex, ]

  # Train logistic regression model
  model <- glm(Target_Organ ~ ., data = train, family = binomial())
  summary(model)

  # Predict probabilities on the test data
  predictions <- predict(model, newdata = test, type = "response")

  # Convert probabilities to binary predictions (default threshold = 0.5)
  pred_class <- ifelse(predictions > 0.5, 1, 0)

  # Compute confusion matrix and accuracy
  caret::confusionMatrix(factor(pred_class), test$Target_Organ)
}

# Run Logistic Regression
#logistic_model(rfData)
