# k-NN Model
knn_model <- function(rfData) {
  # Ensure the target is a factor
  rfData$Target_Organ <- as.factor(rfData$Target_Organ)

  # Normalize data (excluding the target column)
  normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
  rfData[,-1] <- as.data.frame(lapply(rfData[,-1], normalize))

  # Split data into training and testing sets
  set.seed(123)
  trainIndex <- sample(seq_len(nrow(rfData)), size = 0.7 * nrow(rfData))
  train <- rfData[trainIndex, ]
  test <- rfData[-trainIndex, ]

  # Extract train and test data
  train_x <- train[, -1]
  test_x <- test[, -1]
  train_y <- train$Target_Organ
  test_y <- test$Target_Organ

  # Train and predict using k-NN
  predictions <- class::knn(train = train_x, test = test_x, cl = train_y, k = 5)

  # Compute confusion matrix and accuracy
  caret::confusionMatrix(predictions, test_y)
}

# Run k-NN
#knn_model(rfData)
