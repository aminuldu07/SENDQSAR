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
