#' @title Get Important Features from Random Forest Model with Cross-Validation
#'
#'@description
#' This function performs cross-validation with test repetitions on a random forest model, calculates feature importance using Gini importance, and returns the top `n` important features.
#'
#' @param Data A data frame containing the training data (rows as samples, columns as features). The first column is assumed to be the target variable.
#' @param Undersample A logical value indicating whether to apply under-sampling to balance the classes in the training data. Default is `FALSE`.
#' @param best.m A numeric value representing the number of variables to consider at each split of the Random Forest model (or a function to determine this). Default is `NULL`.
#' @param testReps A numeric value indicating the number of test repetitions (must be at least 2).
#' @param Type A numeric value indicating the type of importance to be calculated. `1` for Mean Decrease Accuracy and `2` for Mean Decrease Gini.
#' @param nTopImportance A numeric value indicating the number of top important features to return based on their importance scores.
#'
#' @return A list containing:
#' \describe{
#'   \item{gini_scores}{A matrix of Gini importance scores for each feature across the different cross-validation iterations. The matrix has rows representing features and columns representing test iterations.}
#' }
#'
#' @details
#' This function trains a Random Forest model using cross-validation with specified repetitions and calculates the feature importance using Gini importance scores. The function also supports optional under-sampling to balance the class distribution in the training set.
#'
#' The function performs the following steps:
#' \itemize{
#'   \item Initializes performance metric trackers.
#'   \item Prepares the input data for cross-validation.
#'   \item Performs cross-validation, where each repetition involves training the model on a subset of data and testing on the remaining data.
#'   \item Optionally applies under-sampling to the training data.
#'   \item Trains a Random Forest model on each fold and calculates Gini importance scores.
#'   \item Aggregates and sorts the Gini importance scores to identify the top features.
#'   \item Plots the importance of top features.
#' }
#'
#' @examples
#' \dontrun{
#' # Example of calling the function
#' result <- get_imp_features_from_rf_model_with_cv(
#'   Data = scores_df,
#'   Undersample = FALSE,
#'   best.m = 3,
#'   testReps = 5,
#'   Type = 2,
#'   nTopImportance = 10
#' )
#' }
#'
#' @export



get_imp_features_from_rf_model_with_cv <- function(scores_data_df, #scores_df
                                                  Undersample = FALSE,
                                                  best.m = NULL, # any numeric value or call function to get it
                                                  testReps, # testRps must be at least 2;
                                                  Type,
                                                  nTopImportance) {




    rfData <- scores_data_df #rfData <- scores_df
    #---------------------------------------------------------------------
    # Initialize model performance metric trackers------------------------
    #---------------------------------------------------------------------

    # custom function definition
    `%ni%` <- Negate('%in%')

    # Initialize model performance metric trackers
    Sensitivity <- NULL
    Specificity <- NULL
    PPV <- NULL
    NPV <- NULL
    Prevalence <- NULL
    Accuracy <- NULL
    nRemoved <- NULL


    #-----------------doing cross-validation--------------------------
    #-----------------------------------------------------------------
    #------------------------------------------------------------------

    #-----create and prepare "`rfTestData data` frame" for storing predictions----
    rfTestData <- rfData

    #replaces the existing column names with simple numeric identifiers
    colnames(rfTestData) <- seq(ncol(rfTestData))

    #emptying the data frame.
    for (j in seq(ncol(rfTestData))) {
      rfTestData[,j] <- NA
    }

    #prepares rfTestData to maintain a consistent structure with the necessary
    #columns for storing predictions in subsequent iterations of the loop
    rfTestData <- rfTestData[,1:2] # Keep structure for predictions

    #remove 'gini' from the previous iteration
    if (exists('gini')) {rm(gini)}


    #-------------------------------------------------------------------
    # model building and testing----------------------------------------
    #-------------------------------------------------------------------


    # Perform cross-validation with test repetitions
    # Iterate through test repetitions----------------------------------
    for (i in seq(testReps)) {
      if (i == 1) {
        sampleIndicies <- seq(nrow(rfData))
      }
      if (i < testReps) {
        ind <- sample(seq(nrow(rfData)), floor((nrow(rfData)/testReps)-1), replace = F)
        sampleIndicies <- sampleIndicies[which(sampleIndicies %ni% ind)]
      } else {
        ind <- sampleIndicies
      }

      trainIndex <- which(seq(nrow(rfData)) %ni% ind)
      testIndex <- ind

      # Extract train and test data--------------------------------
      # ind <- sample(2, nrow(rfData), replace = T, prob = c((1- testHoldBack), testHoldBack))
      train <- rfData[trainIndex,]
      test <- rfData[testIndex,]

      # rfAll <- randomForest::randomForest(Target_Organ ~ ., data=rfData, mytry = best.m,
      #                                     importance = F, ntree = 500, proximity = T)


      # Perform under sampling if enabled
      if (Undersample == T) {
        posIndex <- which(train[,1] == 1)
        nPos <- length(posIndex)
        # trainIndex <- c(posIndex, sample(which(train[,1] == 0), nPos, replace = F))
        trainIndex <- c(posIndex, sample(which(train[,1] == 0), nPos, replace = T))
        train <- train[trainIndex,]
        test <- rbind(train[-trainIndex,], test)
      }


      # Train Random Forest model with current iteration's train data
      rf <- randomForest::randomForest(Target_Organ ~ ., data=train, mytry = best.m,
                                       importance = T, ntree = 500, proximity = T)

      print(rf)


      # Calculate Gini importance scores for the model
      giniTmp <-  randomForest::importance(rf, type = Type)

      # Aggregate Gini importance scores across iterations
      if (exists('gini')) {
        gini <- cbind(gini, giniTmp)
      } else {
        gini <- giniTmp
      }
    }



    #-------------------------------------------------------------------------
    # Feature Importance------------------------------------------------------
    #-------------------------------------------------------------------------

    print("Feature Importance (Mean Decrease):")
    print(sort(rowMeans(gini), decreasing = T))


    #-------------------------------------------------------------------------
    # Top Important Features--------------------------------------------------
    #--------------------------------------------------------------------------
    # Get the top n important features based on Gini importance
    imp <- as.matrix(rowMeans(gini)[1:nTopImportance])
    if (Type == 1) {
      colnames(imp) <- 'MeanDecreaseAccuracy'
    } else {
      colnames(imp) <- 'MeanDecreaseGini'
    }
    ord <- order(imp[,1])

    # #------------------------------------------------------------------------
    # # Dotchart for top Variable Importance
    # #------------------------------------------------------------------------
    dotchart(imp[ord, 1], xlab = colnames(imp)[1], ylab = "",
             main = paste0('Top ', nrow(imp), ' - Variable Importance'))#, xlim = c(xmin, max(imp[, i])))

    # varImpPlot(rf,
    #            sort = T,
    #            n.var = 20,
    #            main = "Top 20 - Variable Importance")
print(".............................................................................")

    return(list(

        gini_scores = gini
      )
    )

  }
