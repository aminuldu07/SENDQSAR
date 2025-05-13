#' @title Compute and Plot AUC Curve with Random Forest Model
#'
#'@description
#' This function trains a Random Forest model, computes the ROC curve, and
#' calculates the AUC (Area Under the Curve). It allows various preprocessing
#' options, such as imputation, rounding, undersampling, and hyperparameter tuning.
#'
#' @param ml_formatted_scores_df A data frame containing the training data. If `NULL`, data will be fetched from the database.
#' @param best.m The 'mtry' hyperparameter for Random Forest. If `NULL`, it is determined by the function.


#' @return This function does not return any explicit value. It generates:
#'   \itemize{
#'     \item The AUC (Area Under the Curve) printed to the console.
#'     \item A ROC curve plot with the calculated AUC value.
#'     \item Various performance metrics (e.g., True Positive Rate, False Positive Rate), displayed in the plot.
#'   }
#'
#' @details
#' The function requires a data frame that is generated from function f8. The model is trained using the Random Forest algorithm, and
#' performance is evaluated via the ROC curve and AUC metric. After training the model,
#' predictions are made, and the AUC is calculated and visualized with a ROC curve plot.
#'
#' @examples
#' \dontrun{
#' # Example 1: Using real data from the database
#' #This is a placeholder example. Replace the path with a valid database.
#'  get_auc_curve_with_rf_model(Data = NULL, path_db = "path/to/database.db", rat_studies = TRUE, reps = 10,
#'                              holdback = 0.75, error_correction_method = "Prune")
#'
#' # Example 2: Using sample data (if applicable)
#'  get_auc_curve_with_rf_model(Data = sample_data, path_db = NULL, rat_studies = FALSE, reps = 5,
#'                              holdback = 0.5, error_correction_method = "CorrectMethod")
#' }

#' @seealso
#' `randomForest`, `ROCR`
#'
#' @import DBI
#' @import RSQLite
#' @import ROCR
#' @import randomForest
#'
#' @export



get_auc_curve_with_rf_model_modular  <- function(ml_formatted_scores_df,
                               best.m # The 'mtry' hyperparameter for Random Forest
                               ) {


  # Pass the input Data as rfData and best.m
  rfData <- ml_formatted_scores_df
  best.m <- best.m

  # else {
  #
  #
  #
  # }
  # reassignment of the data
  #rfData <- rfData_and_best_m[["rfData"]]

  # # best.m input handling------------------------------------------------
  # if(is.null(best.m)){
  #   best.m <- rfData_and_best_m[["best.m"]]
  # } else {
  #   best.m <- best.m
  # }

 # Build the Random forest model

  rfAll <- randomForest::randomForest(Target_Organ ~ ., data=rfData, mytry = best.m,
                                      importance = F, ntree = 500, proximity = T)
  # # print(rfAll)
  #
  # if (Undersample == T) {
  #   posIndex <- which(train[,1] == 1)
  #   nPos <- length(posIndex)
  #   trainIndex <- c(posIndex, sample(which(train[,1] == 0), nPos, replace = F))
  #   train <- train[trainIndex,]
  #   test <- rbind(train[-trainIndex,], test)
  # }
  #
  # train_data_two <- train
  # print(dim(train_data_two))
  # Predict probabilities and calculate AUC
  pred1 <- stats::predict(rfAll, type = "prob")
  perf <- ROCR::prediction(pred1[,1], levels(rfData[,1])[rfData[,1]])

  # 1. Area under curve
  auc <- ROCR::performance(perf, "auc")
  AUC <- auc@y.values[[1]]
  print(AUC)

  # 2. True Positive and Negative Rate
  pred3 <- ROCR::performance(perf, "tpr", "fpr")

  # 3. Plot the ROC curve
  plot(pred3, main = paste0("ROC Curve for Random Forest (AUC = ", round(AUC, digits = 3), ")"),
       col = 2, lwd = 2)
  abline(a = 0, b = 1, lwd = 2, lty = 2, col = "gray")

  return()
}










# get_auc_curve <- function(rfData = NULL,# Input data frame for training
#                           best.m  # The 'mtry' hyperparameter for Random Forest
#                           ) {
#
#   # Check if rfData is NULL, calculate rfData
#   if (is.null(rfData)) {
#     # logic for generating rfData
#     rfData_and_best_m <- get_rfData_and_best_m(
#       path_db = path_db,
#       studyid_metadata_path = studyid_metadata_path,
#       fake_study = TRUE,
#       Round = TRUE,
#       Undersample = TRUE
#     )
#
#   }
#
#   rfData <- rfData_and_best_m[["rfData"]]
#   best.m <- rfData_and_best_m[[""]]
#
#   # Train a Random Forest model using the specified mtry value
#   rfAll <- randomForest::randomForest(Target_Organ ~ ., data=rfData, mytry = best.m,
#                                       importance = F, ntree = 500, proximity = T)
#   pred1= stats::predict(rfAll,type = "prob")
#   perf = ROCR::prediction(pred1[,1], levels(rfData[,1])[rfData[,1]])
#   # 1. Area under curve
#   auc = ROCR::performance(perf, "auc")
#   AUC <- auc@y.values[[1]]
#   print(AUC)
#   # 2. True Positive and Negative Rate
#   pred3 = ROCR::performance(perf, "tpr","fpr") # check the ROCR packge assignment here
#   # 3. Plot the ROC curve
#   plot(pred3,main=paste0("ROC Curve for Random Forest (AUC = ", round(AUC, digits = 3), ")"),col=2,lwd=2)
#   abline(a=0,b=1,lwd=2,lty=2,col="gray")
# }
