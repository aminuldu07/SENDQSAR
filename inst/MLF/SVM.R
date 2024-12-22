# SVM Model
svm_model <- function(rfData) {
  # Ensure the target is a factor
  rfData$Target_Organ <- as.factor(rfData$Target_Organ)

  # Split data into training and testing sets
  set.seed(123)
  trainIndex <- sample(seq_len(nrow(rfData)), size = 0.7 * nrow(rfData))
  train <- rfData[trainIndex, ]
  test <- rfData[-trainIndex, ]

  # Train SVM model with a radial kernel
  model <- e1071::svm(Target_Organ ~ ., data = train, kernel = "radial", probability = TRUE)

  # Predict probabilities on the test data
  predictions <- predict(model, newdata = test, probability = TRUE)

  # Convert predictions to factor for confusion matrix
  pred_class <- predictions
  caret::confusionMatrix(pred_class, test$Target_Organ)
}

# Run SVM
#svm_model(rfData)
