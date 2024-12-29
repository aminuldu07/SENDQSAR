




get_Data_formatted_for_ml <- function(path_db,
                                      rat_studies=FALSE,
                                      studyid_metadata=NULL,
                                      fake_study = FALSE,
                                      use_xpt_file = FALSE,
                                      Round = FALSE,
                                      Impute = FALSE,
                                      reps,
                                      holdback,
                                      Undersample = FALSE,
                                      hyperparameter_tuning = FALSE,
                                      error_correction_method # = must be 'Flip' or "Prune' or 'None'
                                      ){

  # Process the database to retrieve the vector of "STUDYIDs"-------------
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


  # process the database to get the "studyid_metadata"------------
  if(is.null(studyid_metadata)) {
    if(fake_study) {
      # Extract study ID metadata
      studyid_metadata <- dm[, "STUDYID", drop=FALSE]

      # Remove duplicates based on STUDYID
      studyid_metadata <- studyid_metadata[!duplicated(studyid_metadata$STUDYID), , drop =FALSE]

      # Add a new column for Target_Organ
      studyid_metadata$Target_Organ <- NA

      # assign "Target_Organ" column values randomly
      # randomly 50% of the value is Liver and rest are not_Liver
      set.seed(123)  # Set seed for reproducibility
      rows_number <- nrow(studyid_metadata)  # Number of rows

      # Randomly sample 50% for "Liver" and rest for "not_Liver"
      studyid_metadata$Target_Organ <- sample(c("Liver", "not_Liver"), size = rows_number, replace = TRUE, prob = c(0.5, 0.5))

      # View the result

    } else {

      # create "studyid_metadata" data frame from "studyid_or_studyids" vector
      studyid_metadata <- data.frame(STUDYID = studyid_or_studyids)

      # Remove duplicates based on STUDYID
      studyid_metadata <- studyid_metadata[!duplicated(studyid_metadata$STUDYID), , drop = FALSE]

      # Add a new column for Target_Organ
      studyid_metadata$Target_Organ <- NA

      # assign "Target_Organ" column values randomly
      # randomly 50% of the value is Liver and rest are not_Liver
      set.seed(123)  # Set seed for reproducibility
      rows_number <- nrow(studyid_metadata)  # Number of rows

      # Randomly sample 50% for "Liver" and rest for "not_Liver"
      studyid_metadata$Target_Organ <- sample(c("Liver", "not_Liver"), size = rows_number, replace = TRUE, prob = c(0.5, 0.5))

    }
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

  # best.m input handling------------------------------------------------
  # if(is.null(best.m)){
  #   best.m <- rfData_and_best_m[["best.m"]]
  #   } else {
  #   best.m <- best.m
  # }



return(Data = rfData)

}
