#' Generate and Plot AUC Curve for Random Forest Model
#'
#' This function trains a Random Forest model on provided or dynamically generated data, computes the
#' Area Under the Curve (AUC) for the model's performance, and plots the Receiver Operating Characteristic (ROC) curve.
#'
#' @param rfData Data frame. The input data for training the Random Forest model. If `NULL`, the data is generated using
#'   \code{get_rfData_and_best_m}.
#' @param best.m Integer. The `mtry` hyperparameter for Random Forest. If `NULL`, the value is determined dynamically
#'   using \code{get_rfData_and_best_m}.
#' @param path_db Character. Path to the SQLite database. Required if `rfData` or `best.m` is `NULL`.
#' @param studyid_metadata_path Character. Path to the CSV file containing study ID metadata. Required if `rfData` or
#'   `best.m` is `NULL`.
#' @param fake_study Logical. Whether to use fake study IDs. Default is \code{TRUE}.
#' @param Round Logical. Whether to round numerical values in the data. Default is \code{TRUE}.
#' @param Undersample Logical. Whether to perform undersampling to balance the data. Default is \code{TRUE}.
#'
#' @return This function does not return a value. It prints the AUC value and plots the ROC curve.
#' @details
#' If `rfData` and `best.m` are not provided, the function dynamically generates the required data by connecting to
#' the specified SQLite database and processing metadata.
#'
#' The function uses the `randomForest` package to train the model and the `ROCR` package to calculate and plot
#' the AUC and ROC curve.
#'
#' @export
#'
#' @examples
#' # Using pre-calculated rfData and best.m
#' get_auc_curve(rfData = my_rfData, best.m = 5)
#'
#' # Dynamically generating rfData and best.m
#' get_auc_curve(
#'   path_db = "path/to/database.db",
#'   studyid_metadata_path = "path/to/study_metadata.csv",
#'   fake_study = TRUE,
#'   Round = TRUE,
#'   Undersample = TRUE
#' )




get_auc_curve <- function(rfData = NULL, # Input data frame for training
                          best.m = NULL, # The 'mtry' hyperparameter for Random Forest
                          path_db = NULL, # Path to the SQLite database
                          studyid_metadata_path = NULL, # Path to the study ID metadata CSV
                          fake_study = TRUE, # Whether to use fake study IDs
                          Round = TRUE, # Whether to round numerical values
                          Undersample = TRUE # Whether to perform undersampling
) {

  # Check if rfData is NULL, calculate rfData
  if (is.null(rfData) || is.null(best.m)) {
    if (is.null(path_db) || is.null(studyid_metadata_path)) {
      stop("Both 'path_db' and 'studyid_metadata_path' must be provided if 'rfData' or 'best.m' is NULL.")
    }

    # Generate rfData and best.m using get_rfData_and_best_m
    rfData_and_best_m <- get_rfData_and_best_m(
      path_db = path_db,
      studyid_metadata_path = studyid_metadata_path,
      fake_study = fake_study,
      Round = Round,
      Undersample = Undersample
    )

    rfData <- rfData_and_best_m[["rfData"]]
    best.m <- rfData_and_best_m[["best.m"]]
  }

  # Train a Random Forest model using the specified mtry value
  rfAll <- randomForest::randomForest(Target_Organ ~ ., data = rfData, mytry = best.m,
                                      importance = FALSE, ntree = 500, proximity = TRUE)

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
