#' Get Random Forest Data and Best Model
#'
#' This function retrieves and processes data for random forest analysis from a SQLite database.
#' It performs the following steps:
#' 1. Connects to the SQLite database and retrieves unique `STUDYID` values from the `dm` table.
#' 2. Generates liver toxicity scores for the given study IDs.
#' 3. Harmonizes the columns in the scores data frame.
#' 4. Reads metadata for study IDs.
#' 5. Prepares the data and tunes hyperparameters for a random forest model.
#'
#' @param path_db Character. Path to the SQLite database.
#' @param studyid_metadata_path Character. Path to the CSV file containing metadata for study IDs.
#' @param fake_study Logical. Whether to use fake study IDs. Default is `TRUE`.
#' @param use_xpt_file Logical. Whether to use XPT file format. Default is `FALSE`.
#' @param output_individual_scores Logical. Whether to output individual scores. Default is `TRUE`.
#' @param output_zscore_by_USUBJID Logical. Whether to output z-scores by `USUBJID`. Default is `FALSE`.
#' @param Impute Logical. Whether to impute missing values in the data. Default is `TRUE`.
#' @param Round Logical. Whether to round numerical values in the data. Default is `TRUE`.
#' @param reps Integer. Number of repetitions for model evaluation. Default is `1`.
#' @param holdback Numeric. Proportion of data to hold back for validation. Default is `0.75`.
#' @param Undersample Logical. Whether to perform undersampling to balance the data. Default is `TRUE`.
#' @param hyperparameter_tuning Logical. Whether to perform hyperparameter tuning. Default is `FALSE`.
#' @param error_correction_method Character. Method for error correction. Default is `'None'`.
#'
#' @return A list containing the processed data and the best model parameters.
#' @export
#'
#' @examples
#' path_db <- "C:/path/to/database.db"
#' studyid_metadata_path <- "C:/path/to/study_metadata.csv"
#' rfData_and_best_m <- get_rfData_and_best_m(
#'   path_db = path_db,
#'   studyid_metadata_path = studyid_metadata_path,
#'   fake_study = TRUE,
#'   Round = TRUE,
#'   Undersample = TRUE
#' )



get_rfData_and_best_m <- function(path_db,
                                  studyid_metadata_path,
                                  fake_study = TRUE,
                                  use_xpt_file = FALSE,
                                  output_individual_scores = TRUE,
                                  output_zscore_by_USUBJID = FALSE,
                                  Impute = TRUE,
                                  Round = TRUE,
                                  reps = 1,
                                  holdback = 0.75,
                                  Undersample = TRUE,
                                  hyperparameter_tuning = FALSE,
                                  error_correction_method = 'None') {
  # Load required libraries
  if (!requireNamespace("DBI", quietly = TRUE)) stop("Package 'DBI' is required.")
  if (!requireNamespace("RSQLite", quietly = TRUE)) stop("Package 'RSQLite' is required.")

  # Create a connection to the database
  dbtoken <- DBI::dbConnect(RSQLite::SQLite(), dbname = path_db)

  # Retrieve the STUDYID column from the dm table
  query <- "SELECT STUDYID FROM dm"
  studyid_data <- DBI::dbGetQuery(dbtoken, query)

  # Extract unique STUDYID values
  unique_studyids <- unique(studyid_data$STUDYID)

  # Disconnect from the database
  DBI::dbDisconnect(dbtoken)

  # Set study IDs
  studyid_or_studyids <- unique_studyids

  # Get liver scores
  fake80_liver_scores <- get_liver_om_lb_mi_tox_score_list(
    studyid_or_studyids = studyid_or_studyids,
    path_db = path_db,
    fake_study = fake_study,
    use_xpt_file = use_xpt_file,
    output_individual_scores = output_individual_scores,
    output_zscore_by_USUBJID = output_zscore_by_USUBJID
  )

  # Harmonize the columns
  column_harmonized_liverscr_df <- get_col_harmonized_scores_df(
    liver_score_data_frame = fake80_liver_scores,
    Round = Round
  )

  # Load the study metadata
  fake_80_medata <- read.csv(studyid_metadata_path, header = TRUE, sep = ",", stringsAsFactors = FALSE)

  # Prepare data and tune hyperparameters
  rfData_and_best_m <- prepare_data_and_tune_hyperparameters(
    scores_df = column_harmonized_liverscr_df,
    studyid_metadata = fake_80_medata,
    Impute = Impute,
    Round = Round,
    reps = reps,
    holdback = holdback,
    Undersample = Undersample,
    hyperparameter_tuning = hyperparameter_tuning,
    error_correction_method = error_correction_method
  )

  return(rfData_and_best_m)
}
