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




get_all_TESTCD_rf_input_param_list_output_cv_imp <- function(path_db,
                                                  rat_studies=FALSE,
                                                  studyid_metadata,
                                                  fake_study = FALSE,
                                                  use_xpt_file = FALSE,
                                                  all_lb_TESTCD_score=FALSE,
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

  studyid_metadata <-  studyid_metadata

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

        # 'parallel_repeatdose_df' is a data frame with one column "STUDYID"
        parallel_repeatdose_df <- get_repeat_dose_parallel_studyids(path_db=path_db,
                                                                    rat_studies = rat_studies)

        # Now, filter the "studyid_or_studyids" for the studyids
        # present in the "studyid_metadata

        studyid_or_studyids <- parallel_repeatdose_df[parallel_repeatdose_df$STUDYID %in% studyid_metadata$STUDYID,  ]

      }
    }

  #-----------------------------------------------------------------------
  # if studyid_metadata is not provided then use the data frame to
  # creae a data frame with two columns "STUDYID" and "Target_Organ"

  #-------------------------------------------------------------------------

  # get_liver_om_lb_mi_tox_score_list(
  # calculated_liver_scores <- get_liver_om_lb_mi_tox_score_list(studyid_or_studyids = studyid_or_studyids,
  #                                                        path_db = path_db,
  #                                                        fake_study = fake_study,
  #                                                        use_xpt_file = use_xpt_file,
  #                                                        output_individual_scores = TRUE,
  #                                                        output_zscore_by_USUBJID = FALSE)

  calculated_indiv_liver_scores <- get_indiv_score_om_lb_mi_domain_df(studyid_or_studyids = studyid_or_studyids,
                                                  path_db=path_db,
                                                  fake_study = fake_study,
                                                  use_xpt_file = use_xpt_file,
                                                  all_lb_TESTCD_score = all_lb_TESTCD_score,
                                                  output_individual_scores = TRUE,
                                                  output_zscore_by_USUBJID = FALSE
  )

  write.csv(calculated_indiv_liver_scores, "calculated_indiv_liver_scores.csv", row.names = FALSE)

  # Harmonize the column
  indiv_column_harmonized_liverscr_df <- get_col_harmonized_scores_df(liver_score_data_frame = calculated_indiv_liver_scores,
                                                              Round = Round)



  indiv_rfData_and_best_m <- get_ml_data_and_tuned_hyperparameters( Data = indiv_column_harmonized_liverscr_df,
                                                            studyid_metadata = studyid_metadata,
                                                            Impute = Impute,
                                                            Round = Round,
                                                            reps=reps,
                                                            holdback=holdback,
                                                            Undersample = Undersample,
                                                            hyperparameter_tuning = hyperparameter_tuning,
                                                            error_correction_method = error_correction_method)



  rfData <- indiv_rfData_and_best_m [["rfData"]]

  # best.m input handling------------------------------------------------
  if(is.null(best.m)){
    best.m <- indiv_rfData_and_best_m [["best.m"]]
    } else {
    best.m <- best.m
  }


  # train_and_evaluate_rf_model <- get_rf_model_with_cv (scores_data_df = rfData,
  #                                                    Undersample = Undersample,
  #                                                     best.m = best.m ,
  #                                                     testReps = testReps,
  #                                                     indeterminateUpper = indeterminateUpper,
  #                                                     indeterminateLower = indeterminateLower,
  #                                                      Type = Type ,
  #                                                      nTopImportance =  nTopImportance)

indiv_train_and_evaluate_rf_model <- get_rf_model_with_cv (scores_data_df = rfData,
                                                     Undersample = Undersample,
                                                     best.m = best.m ,
                                                     testReps = testReps,
                                                     Type = Type)



rf_model_performance <- get_zone_exclusioned_rf_model_with_cv (scores_data_df =rfData, #scores_df
                                                              Undersample = Undersample,
                                                              best.m = best.m , # any numeric value or call function to get it
                                                              testReps=testReps, # testRps must be at least 2;
                                                              indeterminateUpper=indeterminateUpper,
                                                              indeterminateLower=indeterminateLower,
                                                              Type=Type)
return(rf_model_performance)

}
