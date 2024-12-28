




get_random_forest_model_performance <- function(path_db,
                                                studyid_metadata,
                                                fake_study = FALSE,
                                                use_xpt_file = FALSE
                                                ){


    if(use_xpt_file){

      studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)

    } else {
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
    }


calculated_liver_scores <- get_liver_om_lb_mi_tox_score_list(studyid_or_studyids = studyid_or_studyids,
                                                         path_db = path_db,
                                                         fake_study = fake_study,
                                                         use_xpt_file = use_xpt_file,
                                                         output_individual_scores = TRUE,
                                                         output_zscore_by_USUBJID = FALSE)

# Harmonize the column
column_harmonized_liverscr_df <- get_col_harmonized_scores_df(liver_score_data_frame = calculated_liver_scores,
                                                              Round = TRUE)



rfData_and_best_m <- prepare_data_and_tune_hyperparameters( scores_df = column_harmonized_liverscr_df,
                                                            studyid_metadata = studyid_metadata,
                                                            Impute = TRUE,
                                                            Round = TRUE,
                                                            reps=1,
                                                            holdback=0.25,
                                                            Undersample = TRUE,
                                                            hyperparameter_tuning = FALSE,
                                                            error_correction_method = 'None')



rfData <- rfData_and_best_m[["rfData"]]
best.m <- rfData_and_best_m[["best.m"]]

train_and_evaluate_rf_model <- train_eval_rf_with_cv_imp(scores_df = rfData,
                                                         Undersample = TRUE,
                                                         best.m = best.m ,
                                                         testReps = 5,
                                                         indeterminateUpper = .75,
                                                         indeterminateLower = .25,
                                                         Type = 1,
                                                         nTopImportance = 20)



return(train_and_evaluate_rf_model)

}
