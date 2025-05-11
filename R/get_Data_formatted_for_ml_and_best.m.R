#' @title Retrieve and Preprocess Data for Machine Learning Models
#'
#'@description
#' This function processes data from a given SQLite database or XPT file, calculates liver toxicity scores, and prepares data for machine learning models.
#' It can also tune hyperparameters and apply error correction methods.
#'
#' @param path_db A character string representing the path to the SQLite database or XPT file.
#' @param rat_studies A logical flag to filter for rat studies (default is FALSE).
#' @param studyid_metadata A data frame containing metadata for the study IDs. If NULL, metadata is generated (default is NULL).
#' @param fake_study A logical flag to use fake study data (default is FALSE).
#' @param use_xpt_file A logical flag to indicate whether to use an XPT file instead of a SQLite database (default is FALSE).
#' @param Round A logical flag to round liver toxicity scores (default is FALSE).
#' @param Impute A logical flag to impute missing values in the dataset (default is FALSE).
#' @param reps An integer specifying the number of repetitions for cross-validation.
#' @param holdback A numeric value indicating the fraction of data to hold back for validation.
#' @param Undersample A logical flag to undersample the majority class (default is FALSE).
#' @param hyperparameter_tuning A logical flag to perform hyperparameter tuning (default is FALSE).
#' @param error_correction_method A character string specifying the error correction method. Must be one of 'Flip', 'Prune', or 'None'.
#'
#' @return A list containing:
#'   \item{Data}{A data frame containing the preprocessed data ready for machine learning.}
#'   \item{best.m}{The best machine learning model after hyperparameter tuning, if applicable.}
#'
#' @details
#' This function performs several key steps:
#' - Retrieves study IDs from an SQLite database or XPT file.
#' - Generates or uses provided study metadata, including a random assignment of "Target_Organ" values (either "Liver" or "not_Liver").
#' - Calculates liver toxicity scores using the `get_liver_om_lb_mi_tox_score_list` function.
#' - Harmonizes the calculated scores using the `get_col_harmonized_scores_df` function.
#' - Prepares the data for machine learning and tunes hyperparameters (if enabled) using the `get_ml_data_and_tuned_hyperparameters` function.
#' - Returns the processed data and the best model.
#'
#' @examples
#' \dontrun{
#' result <- get_Data_formatted_for_ml_and_best.m(
#'   path_db = "path/to/database.db",
#'   rat_studies = TRUE,
#'   reps = 5,
#'   holdback = 0.2,
#'   error_correction_method = "Flip"
#' )
#'
#' # Access the processed data and the best model
#' processed_data <- result$Data
#' best_model <- result$best.m
#' }
#'
#' @import DBI
#' @import RSQLite
#' @export




get_Data_formatted_for_ml_and_best.m <- function( path_db,
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


  studyid_metadata <- studyid_metadata

  #-----------------------------------------------------------------------
  # if studyid_metadata is not provided then use the provided data base to
  # create a data frame with two columns "STUDYID" and "Target_Organ".
  # This is done using "column_harmonized_liverscr_df

  #`````````````````````````````````````````````````````````````````````````````
  # However, first we have to get the studyids from the database or "xpt-folders"
  #-------------------------------------------------------------------------

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

        # 'parallel_repeatdose_df' is a data frame with one column "STUDYID"
        parallel_repeatdose_df <- get_repeat_dose_parallel_studyids(path_db=path_db,
                                                                    rat_studies = rat_studies)

        # Now, filter the "studyid_or_studyids" for the studyids
        # present in the "studyid_metadata

        studyid_or_studyids <- parallel_repeatdose_df[parallel_repeatdose_df$STUDYID %in% studyid_metadata$STUDYID,  ]

      }
    }

#--------------------------------------------------------------------------------------

#------------------------------------------------------------------------------------------
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


  #------------------------------------------------------------------------------------
  # if "studyid_metadata" is empty, create it # from "column_harmonized_liverscr_df"
  #----------------------------------------------------------------------------------
  # process the "column_harmonized_liverscr_df" to get the "studyid_metadata"------------
  if (is.null(studyid_metadata)) {
    studyid_metadata <- column_harmonized_liverscr_df[,1:2]
    studyid_metadata$Target_Organ <- NA
    studyid_metadata <- studyid_metadata[,c("STUDYID", "Target_Organ")]

    # assign "Target_Organ" column values randomly
    # randomly 50% of the value is Liver and rest are not_Liver
    set.seed(123)  # Set seed for reproducibility
    rows_number <- nrow(studyid_metadata)  # Number of rows

    # Randomly sample 50% for "Liver" and rest for "not_Liver"
    studyid_metadata$Target_Organ <- sample(c("Liver", "not_Liver"), size = rows_number, replace = TRUE, prob = c(0.5, 0.5))
  }


  #---------------------------------------------------------------------------------------

  rfData_and_best_m <- get_ml_data_and_tuned_hyperparameters( Data = column_harmonized_liverscr_df,
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

return(list(Data = rfData,
            best.m= best.m))

}



#----------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
# # process the database to get the "studyid_metadata"------------
# if (is.null(studyid_metadata)) {
#
#   repeat_dose_parallel_studyids <- get_repeat_dose_parallel_studyids(path_db,
#                                                                      rat_studies = FALSE)
#   repeat_dose_parallel_studyids$Target_Organ <- NA
#   studyid_metadata <- repeat_dose_parallel_studyids
#   #studyid_metadata <- input_scores_df[,1:2]
#   #studyid_metadata$Target_Organ <- NA
#   #studyid_metadata <- studyid_metadata[,c("STUDYID", "Target_Organ")]
#   n_rows <- nrow(studyid_metadata)
#   half_n <- ceiling(n_rows / 2)
#   studyid_metadata$Target_Organ <- c(rep("Liver", half_n),
#                                      rep("not_Liver", n_rows - half_n))
#
# }

# # process the database to get the "studyid_metadata"------------
# if(is.null(studyid_metadata)) {
#   if(fake_study) {
#     # Extract study ID metadata
#     studyid_metadata <- dm[, "STUDYID", drop=FALSE]
#
#     # Remove duplicates based on STUDYID
#     studyid_metadata <- studyid_metadata[!duplicated(studyid_metadata$STUDYID), , drop =FALSE]
#
#     # Add a new column for Target_Organ
#     studyid_metadata$Target_Organ <- NA
#
#     # assign "Target_Organ" column values randomly
#     # randomly 50% of the value is Liver and rest are not_Liver
#     set.seed(123)  # Set seed for reproducibility
#     rows_number <- nrow(studyid_metadata)  # Number of rows
#
#     # Randomly sample 50% for "Liver" and rest for "not_Liver"
#     studyid_metadata$Target_Organ <- sample(c("Liver", "not_Liver"), size = rows_number, replace = TRUE, prob = c(0.5, 0.5))
#
#     # View the result
#
#   } else {
#
#     # create "studyid_metadata" data frame from "studyid_or_studyids" vector
#     studyid_metadata <- data.frame(STUDYID = studyid_or_studyids)
#
#     # Remove duplicates based on STUDYID
#     studyid_metadata <- studyid_metadata[!duplicated(studyid_metadata$STUDYID), , drop = FALSE]
#
#     # Add a new column for Target_Organ
#     studyid_metadata$Target_Organ <- NA
#
#     # assign "Target_Organ" column values randomly
#     # randomly 50% of the value is Liver and rest are not_Liver
#     set.seed(123)  # Set seed for reproducibility
#     rows_number <- nrow(studyid_metadata)  # Number of rows
#
#     # Randomly sample 50% for "Liver" and rest for "not_Liver"
#     studyid_metadata$Target_Organ <- sample(c("Liver", "not_Liver"), size = rows_number, replace = TRUE, prob = c(0.5, 0.5))
#
#   }
# }
