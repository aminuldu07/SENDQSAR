#' @title Get Representation Tree from Random Forest Model
#'
#'@description
#' This function trains a Random Forest model on a provided dataset and generates a representation tree (ReprTree) from the trained model. It supports various preprocessing configurations, model hyperparameters, and sampling strategies, including random undersampling. The function also allows for error correction and hyperparameter tuning.
#'
#' @param Data A data frame containing the dataset to train the Random Forest model. If `NULL`, data is fetched using the `get_Data_formatted_for_ml_and_best.m` function.
#' @param path_db A character string representing the path to the database used for fetching or processing the data.
#' @param rat_studies A logical flag indicating whether rat studies are used (default: `FALSE`).
#' @param studyid_metadata A data frame containing metadata related to study IDs (default: `NULL`).
#' @param fake_study A logical flag indicating whether to use fake study data (default: `FALSE`).
#' @param use_xpt_file A logical flag indicating whether to use the XPT file format for data input (default: `FALSE`).
#' @param Round A logical flag indicating whether to round the data before processing (default: `FALSE`).
#' @param Impute A logical flag indicating whether to impute missing values in the data (default: `FALSE`).
#' @param reps An integer specifying the number of repetitions to perform for cross-validation or resampling.
#' @param holdback A numeric value representing the fraction of data to hold back for testing.
#' @param Undersample A logical flag indicating whether undersampling should be applied to balance the dataset (default: `FALSE`).
#' @param hyperparameter_tuning A logical flag indicating whether hyperparameter tuning should be performed (default: `FALSE`).
#' @param error_correction_method A character string specifying the method for error correction. Must be one of `'Flip'`, `'Prune'`, or `'None'`.
#'
#' @return A plot of the first tree from the Random Forest model is displayed. The function does not return the ReprTree object explicitly, but it is generated and used for plotting.
#'
#' @details
#' The function performs the following steps:
#' 1. **Data Preparation**: If `Data` is `NULL`, it is fetched using the `get_Data_formatted_for_ml_and_best.m` function. Data is then split into training (70%) and testing (30%) sets. If `Undersample` is `TRUE`, the training data is balanced using undersampling.
#' 2. **Model Training**: A Random Forest model is trained using the `randomForest::randomForest` function. The target variable is `Target_Organ`, and the model uses the best hyperparameter (`best.m`). The number of trees is set to 500.
#' 3. **ReprTree Generation**: The `reprtree::ReprTree` function is used to generate the representation tree from the trained Random Forest model.
#' 4. **Visualization**: The first tree from the Random Forest model is plotted using the `reprtree::plot.getTree` function.
#'
#' @examples
#' \dontrun{
#' #get_reprtree_from_rf_model(
#'   Data = my_data,
#'   path_db = "path/to/database",
#'   rat_studies = TRUE,
#'   studyid_metadata = my_metadata,
#'   fake_study = FALSE,
#'   use_xpt_file = TRUE,
#'   Round = TRUE,
#'   Impute = TRUE,
#'   reps = 5,
#'   holdback = 0.3,
#'   Undersample = TRUE,
#'   hyperparameter_tuning = FALSE,
#'   error_correction_method = "Flip"
#' )
#' }
#' @import randomForest
#' @import reprtree
#' @export


get_reprtree_from_rf_model <- function ( ml_formatted_scores_df=NULL,
                                         path_db,
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
                                         error_correction_method,
                                         best.m = NULL) { # = must be 'Flip' or "Prune' or 'None'

    Data <- ml_formatted_scores_df
    #best.m <- best.m

    # enforce that Data and best.m must either both be NULL or both be non-NULL
    if (xor(is.null(Data), is.null(best.m))) {
      stop("Error: Either both 'Data' and 'best.m' must be NULL or both must be non-NULL.")
    }
#
#   if (is.null(studyid_metadata)) {
#
#     repeat_dose_parallel_studyids <- get_repeat_dose_parallel_studyids(path_db,
#                                                    rat_studies = FALSE)
#     repeat_dose_parallel_studyids$Target_Organ <- NA
#     studyid_metadata <- repeat_dose_parallel_studyids
#     #studyid_metadata <- input_scores_df[,1:2]
#     #studyid_metadata$Target_Organ <- NA
#     #studyid_metadata <- studyid_metadata[,c("STUDYID", "Target_Organ")]
#     n_rows <- nrow(studyid_metadata)
#     half_n <- ceiling(n_rows / 2)
#     studyid_metadata$Target_Organ <- c(rep("Liver", half_n),
#                                        rep("not_Liver", n_rows - half_n))
#
#   }
#
#
#   if (is.null(ml_formatted_scores_df) && is.null(best.m)) {
#
#     data_and_best.m <- get_Data_formatted_for_ml_and_best.m(path_db=path_db,
#                                                    rat_studies=rat_studies,
#                                                    studyid_metadata=studyid_metadata,
#                                                    fake_study = fake_study,
#                                                    use_xpt_file = use_xpt_file,
#                                                    Round =  Round,
#                                                    Impute = Impute,
#                                                    reps=reps,
#                                                    holdback=holdback,
#                                                    Undersample = Undersample,
#                                                    hyperparameter_tuning = hyperparameter_tuning,
#                                                    error_correction_method=error_correction_method) # = must be 'Flip' or "Prune' or 'None'
#     Data <- data_and_best.m[["ml_formatted_scores_df"]]
#     best.m <- data_and_best.m[["best.m"]]
#
#     }


    # Generate data if not provided
    if (is.null(ml_formatted_scores_df) && is.null(best.m)) {

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


      # If "studyid_metadata = NULL"-----------------

      if(is.null(studyid_metadata)){

        studyid_metadata <- get_studyid_metadata(input_df = column_harmonized_liverscr_df)
      }

      #Data <- column_harmonized_liverscr_df

      rfData_and_best_m <- get_ml_data_and_tuned_hyperparameters( column_harmonized_df = column_harmonized_liverscr_df,
                                                                  studyid_metadata = studyid_metadata,
                                                                  Impute = Impute,
                                                                  Round = Round,
                                                                  reps=reps,
                                                                  holdback=holdback,
                                                                  Undersample = Undersample,
                                                                  hyperparameter_tuning = hyperparameter_tuning,
                                                                  error_correction_method = error_correction_method)

      Data <- rfData_and_best_m[["ml_formatted_scores_df"]]
      best.m <- rfData_and_best_m[["best.m"]]

    }

    # First way---------------------------------------------------------
    # Use a single random split (e.g., 70% train, 30% test)
    set.seed(123)  # Ensure reproducibility
    trainIndex <- sample(seq(nrow(Data)), size = floor(0.7 * nrow(Data)), replace = FALSE)
    testIndex <- setdiff(seq(nrow(Data)), trainIndex)

    train <- Data[trainIndex, ]
    test <- Data[testIndex, ]

    if (Undersample == T) {

      posIndex <- which(train[,1] == 1)
      nPos <- length(posIndex)
      trainIndex <- c(posIndex, sample(which(train[,1] == 0), nPos, replace = T))
      train <- train[trainIndex,]
      test <- rbind(train[-trainIndex,], test)
    }

    # # Second wat----------------------------------------------------
    # # Use a single random split (e.g., 70% train, 30% test)
    # set.seed(123)  # Ensure reproducibility
    # trainIndex <- sample(seq(nrow(rfData)), size = floor(0.7 * nrow(rfData)), replace = FALSE)
    # testIndex <- setdiff(seq(nrow(rfData)), trainIndex)
    #
    # train <- rfData[trainIndex, ]
    # test <- rfData[testIndex, ]
    #
    # # Handle undersampling if required
    # if (Undersample == TRUE) {
    #
    #   # Find indices of positive samples in the training data
    #   posIndex <- which(train[, 1] == 1)
    #   nPos <- length(posIndex)
    #
    #   # Sample the same number of negative samples (undersampling)
    #   negIndex <- sample(which(train[, 1] == 0), nPos, replace = FALSE)
    #
    #   # Create balanced training data
    #   balancedIndex <- c(posIndex, negIndex)
    #   train <- train[balancedIndex, ]
    # }


    # Train a Random Forest model using the specified mtry value
    rfAll <- randomForest::randomForest(Target_Organ ~ .,
                                        data = Data,
                                        mytry = best.m,
                                        importance = FALSE,
                                        ntree = 500,
                                        proximity = TRUE)

    ReprTree <- reprtree::ReprTree(rfAll,
                         train,
                         metric='d2')

    #plot(ReprTree)
    #library(reprtree)

    # Plot the first tree (k = 1) from the random forest
    reprtree::plot.getTree(rforest = rfAll, k = 5, depth = 10)#, main = "Tree 1")

    #reprtree::plot.getTree(rfAll,train,  )
    #plot(ReprTree(rfAll, train, metric = "d2"))

    #reprtree::plot.reprtree(ReprTree(rfAll, train, metric='d2'))

}
