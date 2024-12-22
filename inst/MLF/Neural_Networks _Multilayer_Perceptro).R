# Neural Network Model
nn_model <- function(rfData) {
  library(neuralnet)

  # Ensure target is numeric (required for neuralnet)
  rfData$Target_Organ <- as.numeric(rfData$Target_Organ)

  # Split data into training and testing sets
  set.seed(123)
  trainIndex <- sample(seq_len(nrow(rfData)), size = 0.7 * nrow(rfData))
  train <- rfData[trainIndex, ]
  test <- rfData[-trainIndex, ]

  # Train the Neural Network model
  formula_nn <- as.formula(paste("Target_Organ ~", paste(colnames(rfData)[-1], collapse = " + ")))
  model <- neuralnet(formula_nn, data = train, hidden = c(5, 3), linear.output = FALSE)

  # Predict on the test set
  predictions <- compute(model, test[,-1])$net.result
  pred_class <- ifelse(predictions > 0.5, 1, 0)

  # Confusion Matrix
  caret::confusionMatrix(factor(pred_class), factor(test$Target_Organ))
}

# Run Neural Network
#nn_model(rfData)
