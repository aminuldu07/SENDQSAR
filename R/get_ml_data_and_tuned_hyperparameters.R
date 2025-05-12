#' @title Get Random Forest Data and Tuned Hyperparameters
#'
#' @description
#' The `get_ml_data_and_tuned_hyperparameters` function processes input data and metadata to prepare data for
#' random forest analysis. It includes steps for data preprocessing, optional imputation, rounding,
#' error correction, and hyperparameter tuning.
#'
#' @param Data data.frame. Input data frame containing scores, typically named `scores_df`.
#' First column is "STUDYID", followed by columns with score values.
#' @param studyid_metadata data.frame. Metadata containing `STUDYID` and `Target_Organ`.
#' @param Impute logical. Indicates whether to impute missing values in the dataset using random forest imputation. Default is `FALSE`.
#' @param Round logical. Specifies whether to round specific numerical columns according to predefined rules. Default is `FALSE`.
#' @param reps integer. Number of repetitions for cross-validation. A value of `0` skips repetition.
#' @param holdback numeric. Fraction of data to hold back for testing. A value of `1` performs leave-one-out cross-validation.
#' @param Undersample logical. Indicates whether to undersample the training data to balance the target classes. Default is `FALSE`.
#' @param hyperparameter_tuning logical. Specifies whether to perform hyperparameter tuning for the random forest model. Default is `FALSE`.
#' @param error_correction_method character. Specifies the method for error correction. Can be `"Flip"`, `"Prune"`, or `None`. Default is `None`.
#'
#' @return
#' A list containing:
#' \describe{
#'   \item{rfData}{The final processed data after preprocessing and error correction.}
#'   \item{best.m}{The best `mtry` hyperparameter determined for the random forest model.}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' Data <- scores_df
#' studyid_metadata <- read.csv("path/to/study_metadata.csv")
#' result <- get_ml_data_and_tuned_hyperparameters(
#'   Data = Data,
#'   studyid_metadata = studyid_metadata,
#'   Impute = TRUE,
#'   Round = TRUE,
#'   reps = 10,
#'   holdback = 0.75,
#'   Undersample = TRUE,
#'   hyperparameter_tuning = TRUE,
#'   error_correction_method = "Flip"
#' )
#' rfData <- result$rfData
#' best_mtry <- result$best.m
#' }


get_ml_data_and_tuned_hyperparameters <- function(column_harmonized_df,
                                                  studyid_metadata,
                                                  Impute = FALSE,
                                                  Round =FALSE,
                                                  reps, # from 0 to any numeric number
                                                  holdback, # either 1 or fraction value like 0.75 etc.
                                                  Undersample = FALSE,
                                                  hyperparameter_tuning = FALSE,
                                                  error_correction_method = "None") { #Default to "None";
                                                                                       #options: "Flip", "Prune", "None"

  input_scores_df <- column_harmonized_df

  #----------------------------------------------------------------------------
  # if "studyid_metadata" is NULL, we can make it using the "STUDYID" column
  #----------------------------------------------------------------------------
  if (is.null(studyid_metadata)) {
    studyid_metadata <- input_scores_df[,1:2]
    studyid_metadata$Target_Organ <- NA
    studyid_metadata <- studyid_metadata[,c("STUDYID", "Target_Organ")]
    n_rows <- nrow(studyid_metadata)
    half_n <- ceiling(n_rows / 2)
    studyid_metadata$Target_Organ <- c(rep("Liver", half_n),
                                       rep("not_Liver", n_rows - half_n))

  }

  metadata_df <- studyid_metadata
  metadata_df$STUDYID <- as.character(metadata_df$STUDYID)

  # Join metadata with the data
  merged_Data <- dplyr::left_join(metadata_df, input_scores_df, by = "STUDYID"  )


  #---------------------------------------------------------------------------
  ###-----------------------rfData preparation--------------------------------
  #---------------------------------------------------------------------------
  rfData <- merged_Data[, -1]
  rfData[which(rfData$Target_Organ == 'Liver'), 1] <- 1
  rfData[which(rfData$Target_Organ == 'not_Liver'), 1] <- 0
  # rfData[,1] <- as.numeric(rfData[,1])
  rfData[,1] <- factor(rfData[,1], levels = c(1, 0))

  # removeIndex <- which(colnames(rfData) %in% c('INFILTRATE'))
  # rfData <- rfData[, -removeIndex]

  #-------------------------------------------------------------------------
  #######-----------------------missing values imputation-------------------
  #--------------------------------------------------------------------------
  ##missing values imputation
  if (Impute == T) {

    # Use random forest imputation to fill in missing values in the dataset
    # Target variable: 'Target_Organ'

    rfData <- randomForest::rfImpute(Target_Organ ~ ., rfData)

    # After Imputation, agian rounding.

    # If rounding of specific variables is enabled
    if (Round == T) {
      # Identify the column indices for columns  related to "averages or liver-related data"liverTOBW" and "LB"

      zscoreIndex <- c(grep('avg_', colnames(rfData)), grep('liver', colnames(rfData)))

      # For the identified columns, round down (floor) the values
      for (i in zscoreIndex) {
        rfData[, i] <- floor(rfData[, i])


        # Find indices of values greater than 5 in the column
        maxIndex <- which(rfData[, i] > 5)

        # Cap values at a maximum of 5 for these columns
        rfData[maxIndex, i] <- 5
      }
      #---------for "MI"-------------columns--------------------------
      # Identify column indices where column names start with uppercase letters,
      # excluding the first column (assumed to be the target variable)
      histoIndex <- which(substr(colnames(rfData), 1, 1) %in% toupper(letters))
      histoIndex <- histoIndex[-1]
      for (i in histoIndex) {
        # For the identified columns, round up (ceiling) the values
        rfData[, i] <- ceiling(rfData[, i])
      }
    }
  }

  #________________________________________________________________________

  count <- 0
  if (reps > 0) {
    for (rep in seq(reps)) {
      print(paste0(rep/reps*100, '% Complete...'))

      # Split data into training and testing
      if (holdback == 1) {
      ind <- sample(seq(nrow(rfData)), 1)
      train <- rfData[-ind, ]
      test <- rfData[ind, ]
      testIndex <- ind
      } else {

      ind <- sample(2, nrow(rfData), replace = TRUE, prob = c(1 - holdback, holdback))
      train <- rfData[ind == 1, ]
      test <- rfData[ind == 2, ]
      testIndex <- which(ind == 2)
      }

      # Undersample the training data if required
      if (Undersample == T) {
      # Identify the indices of rows in the training data where,
      # the first column (target variable) is equal to 1 (positive class).
      # posIndex <- which(train[, 1] == 1)

      # Determine the number of positive instances by calculating the length of the positive class indices.
      # nPos <- length(posIndex)

      # Combine the indices of positive instances with a sample of negative instances (rows where the first column is 0).
      # The sample size is equal to the number of positive instances (`nPos`), and sampling is done without replacement.

      # trainIndex <- c(posIndex, sample(which(train[, 1] == 0), nPos, replace = FALSE))

      ##......issue is , if the negative instances is less than "nPos",...........
        #.....above code will# through an error................................
        #..solve is put replacement=True or follow the below code ............

      # Subset the training data to include only the rows corresponding to the selected indices (`trainIndex`),
      # effectively creating a balanced training dataset with an equal number of positive and negative instances.

      # train <- train[trainIndex, ]

      #-------------------------------------------------------------
      #-------------------------------------------------------------
      # Newly modifed code to address the issue from the above code
      # if replace is FALSE

      # Get indices of positive and negative instances
      posIndex <- which(train[, 1] == 1)  # Positive class indices
      negIndex <- which(train[, 1] == 0)  # Negative class indices

      # Number of positive instances
      nPos <- length(posIndex)

      # Ensure the sample size for negative instances does not exceed the available negative instances
      if (length(negIndex) >= nPos) {
        # Sample 'nPos' negative instances without replacement
        negSample <- sample(negIndex, nPos, replace = FALSE)
      } else {
        # If not enough negative instances, take all available negative instances
        negSample <- negIndex
      }
      # Combine positive indices and sampled negative indices
      trainIndex <- c(posIndex, negSample)

      # Subset the training dataset to the balanced set
      train <- train[trainIndex, ]
      #-------------------------------------------------------------
      #-------------------------------------------------------------

      }

     if(hyperparameter_tuning == T) {

      control <- trainControl(method="repeatedcv", number=10, repeats=3)
      metric <- "Accuracy"
      mtry <- sqrt(ncol(train))
      tunegrid <- expand.grid(.mtry=mtry)

      #Grid and Random Search (caret package)
      rf_default <- train(Target_Organ~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
      rf_random <- train(Target_Organ~., data=train, method="rf", metric=metric, tuneLength=15, trControl=control)

      mtry <- tuneRF(rfData[,-1], rfData[,1], ntreeTry=500,
                   stepFactor=1.5,improve=0.01, trace=F, plot=F)
      best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

      } else {

      best.m <- 4

      }


    # Correction of the labels of the train and test data set
    # using a rf model
  train <- train %>%
    dplyr::filter(rowSums(!is.na(train[, -1])) > 0)


    rf_for_correction <- randomForest::randomForest(Target_Organ ~ .,
                                                  data=train,
                                                  mytry = best.m,
                                                  importance = F,
                                                  ntree = 500,
                                                  proximity = F)



  # train and test data correction/adjustment based on method

  #---------------------------------------------------------------------------
    #if (!is.null(error_correction_method) && error_correction_method %in% c("Flip", "Prune")) {
    if (error_correction_method %in% c("Flip", "Prune")) {

    # Prediction on the test data for correction
    p <- stats::predict(rf_for_correction, test, type = 'prob')[,1]

    # Find indices to flip or prune
    flipLiverIndex <- testIndex[which((rfData[testIndex, 'Target_Organ'] == 1)&(as.numeric(p) < threshold))]
    flipnot_LiverIndex <- testIndex[which((rfData[testIndex, 'Target_Organ'] == 0)&(as.numeric(p) > (1 - threshold)))]

    #If there are indices to flip or prune, the data set is modified
    if (length(flipLiverIndex) > 0) {
      count <- count + 1
      if (ErrorMethod == 'Flip') {
        # Flip 'Liver' to 'not_Liver'
        rfData[flipLiverIndex, 1] <- 0
      } else {
        # Prune (remove) 'Liver' instances
        rfData <- rfData[-flipLiverIndex,]
      }
      if (Round == T) {
        # Save the modified dataset
        saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_Round_', count, '.rds'))
      } else {
        # Save the modified dataset
        saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_', count, '.rds'))
      }
    }
    if (length(flipnot_LiverIndex) > 0) {
      count <- count + 1
      if (ErrorMethod == 'Flip') {
        # Flip 'not_Liver' to 'Liver'
        rfData[flipnot_LiverIndex, 1] <- 1
      } else {
        # Prune (remove) 'not_Liver' instances
        rfData <- rfData[-flipnot_LiverIndex,]
      }
      if (Round == T) {
        # Save the modified dataset
        saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_Round_', count, '.rds'))
      } else {
        # Save the modified dataset
        saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_', count, '.rds'))

       }

    }
    # read teh rfDataRDS
    #rfData <- readRDS(rfDataRDS) # (if data were flipped or pruned)
    #} else if (is.null(error_correction_method)) {
    } else if (error_correction_method == 'None') {
      #if "error_correction_method" = "NULL"
      # If no error correction method is provided, just print a message
      message("No error correction applied. Continuing with the remaining steps,
              \n returning unmodified -rfData- data.")

      #rfData <-  rfData
      rfData <-  train

      }

    }

  }

    return(list(rfData=rfData,
           best.m=best.m))

}
