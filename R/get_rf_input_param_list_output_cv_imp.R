




get_rf_input_param_list_output_cv_imp <- function(path_db,
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
                                                testReps , # at least 2
                                                indeterminateUpper,
                                                indeterminateLower,
                                                Type,
                                                nTopImportance
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

      if(!fake_study ){
        # filer the dm to get the repeat does and parallel studyids


      }

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
                                                              Round = Round)



rfData_and_best_m <- get_ml_data_and_tuned_hyperparameters( scores_df = column_harmonized_liverscr_df,
                                                            studyid_metadata = studyid_metadata,
                                                            Impute = Impute,
                                                            Round = Round,
                                                            reps=reps,
                                                            holdback=holdback,
                                                            Undersample = Undersample,
                                                            hyperparameter_tuning = hyperparameter_tuning,
                                                            error_correction_method = error_correction_method)



rfData <- rfData_and_best_m[["rfData"]]
best.m <- rfData_and_best_m[["best.m"]]

train_and_evaluate_rf_model <- get_rf_model_output_cv_imp(scores_df = rfData,
                                                         Undersample = Undersample,
                                                         best.m = best.m ,
                                                         testReps = testReps,
                                                         indeterminateUpper = indeterminateUpper,
                                                         indeterminateLower = indeterminateLower,
                                                         Type = Type ,
                                                         nTopImportance =  nTopImportance)



return(train_and_evaluate_rf_model)

}
