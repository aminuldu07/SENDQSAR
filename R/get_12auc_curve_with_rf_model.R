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




get_auc_curve_with_rf_model  <- function(Data = NULL, # Input data frame for training
                               path_db=NULL, # Path to the SQLite database
                               rat_studies=FALSE,
                               studyid_metadata,
                               fake_study = FALSE, # Whether to use fake study IDs
                               use_xpt_file = FALSE,
                               Round = FALSE, # Whether to round numerical values
                               Impute = FALSE,
                               best.m = NULL, # The 'mtry' hyperparameter for Random Forest
                               reps, # from 0 to any numeric number
                               holdback, # either 1 or fraction value like 0.75 etc.
                               Undersample = FALSE,
                               hyperparameter_tuning = FALSE,
                               error_correction_method,# # Choose: "Flip" or "Prune" or "None"
                               output_individual_scores = TRUE,
                               output_zscore_by_USUBJID = FALSE) {# Whether to perform undersampling


  # Generate data if not provided
  if (is.null(Data)) {

    if(use_xpt_file){

      studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)

    } else {

      if (fake_study) {
        # Helper function to fetch data from SQLite database
        fetch_domain_data <- function(db_connection, domain_name) {
          # Convert domain name to uppercase
          domain_name <- toupper(domain_name)
          # Create SQL query statement
          query_statement <- paste0('SELECT * FROM ', domain_name)
          # Execute query and fetch the data
          query_result <- DBI::dbGetQuery(db_connection, statement = query_statement)
          # Return the result
          query_result
        }
        # Establish a connection to the SQLite database
        db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path_db)

        # Fetch data for required domains
        dm <- fetch_domain_data(db_connection, 'dm')

        # Close the database connection
        DBI::dbDisconnect(db_connection)

        # get the studyids from the dm table
        studyid_or_studyids <- as.vector(unique(dm$STUDYID)) # unique STUDYIDS from DM table

        # Filter the fake data for the "rat_studies"
        if(rat_studies){

          studyid_or_studyids <- studyid_or_studyids
        }

        #--------------------------------------------------------------------
        #-----------we can set logic here for rat studies in "fake data"----
        #--------------------------------------------------------------------

      } else {
        # For the real data in sqlite database
        # filter for the repeat-dose and parallel studyids

        studyid_or_studyids <- get_repeat_dose_parallel_studyids(path_db=path_db,
                                                                 rat_studies = rat_studies)

      }
    }

    # get scores for the lb,mi and om data frame combined
    calculated_liver_scores <- get_liver_om_lb_mi_tox_score_list(studyid_or_studyids = studyid_or_studyids,
                                                                 path_db = path_db,
                                                                 fake_study = fake_study,
                                                                 use_xpt_file = use_xpt_file,
                                                                 output_individual_scores = TRUE,
                                                                 output_zscore_by_USUBJID = FALSE)

    # Harmonize the column
    column_harmonized_liverscr_df <- get_col_harmonized_scores_df(liver_score_data_frame = calculated_liver_scores,
                                                                  Round = Round)

    #Data <- column_harmonized_liverscr_df

    rfData_and_best_m <- get_ml_data_and_tuned_hyperparameters( Data = column_harmonized_liverscr_df,
                                                                studyid_metadata = studyid_metadata,
                                                                Impute = Impute,
                                                                Round = Round,
                                                                reps=reps,
                                                                holdback=holdback,
                                                                Undersample = Undersample,
                                                                hyperparameter_tuning = hyperparameter_tuning,
                                                                error_correction_method = error_correction_method)

  }


  # reassignment of the data
  rfData <- rfData_and_best_m[["rfData"]]

  # best.m input handling------------------------------------------------
  if(is.null(best.m)){
    best.m <- rfData_and_best_m[["best.m"]]
  } else {
    best.m <- best.m
  }





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
