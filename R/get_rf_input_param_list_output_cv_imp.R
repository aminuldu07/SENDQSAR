#' @title Prepare and Evaluate Random Forest Model with Cross-Validation and Feature Importance
#'
#'@description
#' This function prepares the data for training a Random Forest (RF) model with cross-validation, handles imputation, hyperparameter tuning, and evaluates the model's performance. It supports both real and fake study data, with options for rat studies, error correction, and feature importance selection.
#'
#' @param path_db A character string specifying the path to the SQLite database or directory containing the XPT file.
#' @param rat_studies A logical value indicating whether to filter for rat studies. Default is `FALSE`.
#' @param studyid_metadata A data frame containing metadata for the studies.
#' @param fake_study A logical value indicating whether to use fake study data. Default is `FALSE`.
#' @param use_xpt_file A logical value indicating whether to use XPT file data. Default is `FALSE`.
#' @param Round A logical value indicating whether to round the liver scores. Default is `FALSE`.
#' @param Impute A logical value indicating whether to impute missing values. Default is `FALSE`.
#' @param reps An integer specifying the number of repetitions for model evaluation.
#' @param holdback A numeric value specifying the proportion of data to hold back for validation.
#' @param Undersample A logical value indicating whether to undersample the data to balance classes. Default is `FALSE`.
#' @param hyperparameter_tuning A logical value indicating whether to tune the Random Forest model's hyperparameters. Default is `FALSE`.
#' @param error_correction_method A character string specifying the error correction method. Options are 'Flip', 'Prune', or 'None'.
#' @param best.m A numeric value specifying the number of trees in the Random Forest model. If `NULL`, the function determines this automatically.
#' @param testReps An integer specifying the number of test repetitions for model evaluation.
#' @param indeterminateUpper A numeric value for the upper threshold of indeterminate predictions.
#' @param indeterminateLower A numeric value for the lower threshold of indeterminate predictions.
#' @param Type A character string specifying the type of Random Forest model to use. Options include 'classification' or 'regression'.
#' @param nTopImportance An integer specifying the number of top important features to consider for the model.
#'
#' @return A list containing the trained Random Forest model, cross-validation results, and feature importance scores.
#' The list is returned by the `get_rf_model_with_cv` function.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Fetches the study data based on the specified parameters.
#'   \item Calculates liver scores and harmonizes the data.
#'   \item Prepares data for machine learning, including imputation and optional hyperparameter tuning.
#'   \item Trains and evaluates the Random Forest model with cross-validation.
#'   \item Applies error correction (if specified) and selects the most important features.
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage of the function
#' result <- get_rf_input_param_list_output_cv_imp(
#'   path_db = "path/to/database",
#'   rat_studies = TRUE,
#'   studyid_metadata = metadata_df,
#'   fake_study = FALSE,
#'   use_xpt_file = FALSE,
#'   Round = TRUE,
#'   Impute = TRUE,
#'   reps = 10,
#'   holdback = 0.2,
#'   Undersample = TRUE,
#'   hyperparameter_tuning = TRUE,
#'   error_correction_method = "Flip",
#'   best.m = NULL,
#'   testReps = 5,
#'   indeterminateUpper = 0.9,
#'   indeterminateLower = 0.1,
#'   Type = "classification",
#'   nTopImportance = 10
#' )
#' }
#' @import DBI
#' @import RSQLite
#' @importFrom stats lm
#'
#' @export




get_rf_input_param_list_output_cv_imp <- function(path_db,
                                                  rat_studies=FALSE,
                                                  studyid_metadata,
                                                  fake_study = FALSE,
                                                  use_xpt_file = FALSE,
                                                  Round = FALSE,
                                                  Impute = FALSE,
                                                  reps,
                                                  holdback,
                                                  Undersample = FALSE,
                                                  hyperparameter_tuning = FALSE,
                                                  error_correction_method, # = must be 'Flip' or "Prune' or 'None'
                                                  best.m = NULL, #rf mytr parameter
                                                  testReps , # at least 2
                                                  indeterminateUpper,
                                                  indeterminateLower,
                                                  Type,
                                                  nTopImportance
                                                  ){


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
        studyid_or_studyids <- as.vector(studyid_or_studyids$STUDYID)

      }
    }

  # process the database to get the "studyid_metadata"------------
  if (is.null(studyid_metadata)) {

    repeat_dose_parallel_studyids <- get_repeat_dose_parallel_studyids(path_db,
                                                                       rat_studies = FALSE)
    repeat_dose_parallel_studyids$Target_Organ <- NA
    studyid_metadata <- repeat_dose_parallel_studyids
    #studyid_metadata <- input_scores_df[,1:2]
    #studyid_metadata$Target_Organ <- NA
    #studyid_metadata <- studyid_metadata[,c("STUDYID", "Target_Organ")]
    n_rows <- nrow(studyid_metadata)
    half_n <- ceiling(n_rows / 2)
    studyid_metadata$Target_Organ <- c(rep("Liver", half_n),
                                       rep("not_Liver", n_rows - half_n))

  }

  #-----------------------------------------------------------------------
  # if studyid_metadata is not provided then use the data frame to
  # creae a data frame with two columns "STUDYID" and "Target_Organ"

  #-------------------------------------------------------------------------

  # get_liver_om_lb_mi_tox_score_list(
  calculated_liver_scores <- get_liver_om_lb_mi_tox_score_list(studyid_or_studyids = studyid_or_studyids,
                                                         path_db = path_db,
                                                         fake_study = fake_study,
                                                         use_xpt_file = use_xpt_file,
                                                         output_individual_scores = TRUE,
                                                         output_zscore_by_USUBJID = FALSE)

  # Harmonize the column
  column_harmonized_liverscr_df <- get_col_harmonized_scores_df(liver_score_data_frame = calculated_liver_scores,
                                                              Round = Round)



  rfData_and_best_m <- get_ml_data_and_tuned_hyperparameters( column_harmonized_df = column_harmonized_liverscr_df,
                                                            studyid_metadata = studyid_metadata,
                                                            Impute = Impute,
                                                            Round = Round,
                                                            reps=reps,
                                                            holdback=holdback,
                                                            Undersample = Undersample,
                                                            hyperparameter_tuning = hyperparameter_tuning,
                                                            error_correction_method = error_correction_method)



  rfData <- rfData_and_best_m[["rfData"]]
  #best.m <- rfData_and_best_m[["best.m"]]

  # best.m input handling------------------------------------------------
  if(is.null(best.m)){
    best.m <- rfData_and_best_m[["best.m"]]
    } else {
    best.m <- best.m
  }


  train_and_evaluate_rf_model <- get_rf_model_output_cv_imp(scores_data_df = rfData,
                                                     Undersample = Undersample,
                                                      best.m = best.m ,
                                                      testReps = testReps,
                                                      indeterminateUpper = indeterminateUpper,
                                                      indeterminateLower = indeterminateLower,
                                                       Type = Type ,
                                                       nTopImportance =  nTopImportance)



return(train_and_evaluate_rf_model)

}
