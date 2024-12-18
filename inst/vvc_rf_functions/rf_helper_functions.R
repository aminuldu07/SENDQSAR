# Helper Function: Preprocess data (imputation and rounding)
preprocess_data <- function(data, impute = TRUE, round_values = TRUE) {
  data <- data[, -2]
  data$indst_TO <- factor(ifelse(data$indst_TO == 'Liver', 1, 0), levels = c(1, 0))

  if (impute) {
    data <- randomForest::rfImpute(indst_TO ~ ., data)
    if (round_values) {
      zscoreIndex <- grep('avg_|liver', colnames(data))
      data[, zscoreIndex] <- pmin(floor(data[, zscoreIndex]), 5)

      histoIndex <- setdiff(which(substr(colnames(data), 1, 1) %in% LETTERS), 1)
      data[, histoIndex] <- ceiling(data[, histoIndex])
    }
  }
  return(data)
}

# Helper Function: Perform undersampling on the training set
perform_undersampling <- function(train) {
  posIndex <- which(train[, 1] == 1)
  nPos <- length(posIndex)
  sampledIndex <- c(posIndex, sample(which(train[, 1] == 0), nPos, replace = FALSE))
  return(train[sampledIndex, ])
}

# Helper Function: Build random forest model
build_random_forest <- function(train, mtry = 4) {
  randomForest::randomForest(indst_TO ~ ., data = train, mtry = mtry, importance = TRUE, ntree = 500, proximity = TRUE)
}

# Helper Function: Evaluate model performance and handle indeterminate predictions
evaluate_model <- function(rf, test, indeterminateUpper, indeterminateLower) {
  predictions <- stats::predict(rf, test, type = 'prob')[, 1]
  indeterminateIndex <- which(predictions < indeterminateUpper & predictions > indeterminateLower)
  predictions[indeterminateIndex] <- NA
  rounded_predictions <- round(predictions)
  return(list(predictions = rounded_predictions, indeterminateIndex = indeterminateIndex))
}

# Helper Function: Calculate performance metrics
calculate_performance <- function(predictions, test, indeterminateIndex) {
  cm <- caret::confusionMatrix(factor(predictions, levels = c(1, 0)), factor(test$indst_TO, levels = c(1, 0)))
  performance <- list(
    Sensitivity = cm$byClass['Sensitivity'],
    Specificity = cm$byClass['Specificity'],
    PPV = cm$byClass['Pos Pred Value'],
    NPV = cm$byClass['Neg Pred Value'],
    Prevalence = cm$byClass['Prevalence'],
    Accuracy = cm$byClass['Balanced Accuracy'],
    nRemoved = length(indeterminateIndex) / length(predictions)
  )
  return(performance)
}

# Helper Function: Save dataset after modifications (flip or prune)
save_dataset <- function(data, reps, threshold, holdback, ErrorMethod, Round, count) {
  suffix <- ifelse(Round, "_Round_", "_")
  saveRDS(data, paste0('rfData_', reps, '_', threshold, '_', holdback, '_', ErrorMethod, suffix, count, '.rds'))
}
