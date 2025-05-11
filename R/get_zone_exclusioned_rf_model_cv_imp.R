#' @title Perform Cross-Validation with Random Forest and Feature Importance Calculation
#'
#'@description
#' This function performs cross-validation on a Random Forest model, tracks
#' performance metrics (such as sensitivity, specificity, accuracy), handles
#' indeterminate predictions, and computes feature importance based on either
#' Gini or Accuracy. The function returns performance summaries and feature
#' importance rankings after a specified number of test repetitions.
#'
#' @param scores_df A data frame containing the features and target variable for training and testing the model.
#' @param Undersample A logical flag indicating whether to apply undersampling to the training data. Defaults to `FALSE`.
#' @param best.m A numeric value representing the number of features to sample for the Random Forest model, or `NULL` to calculate it automatically.
#' @param testReps An integer specifying the number of repetitions for cross-validation. Must be at least 2.
#' @param indeterminateUpper A numeric threshold above which predictions are not considered indeterminate.
#' @param indeterminateLower A numeric threshold below which predictions are not considered indeterminate.
#' @param Type An integer specifying the type of importance to compute. `1` for MeanDecreaseAccuracy, `2` for MeanDecreaseGini.
#' @param nTopImportance An integer specifying the number of top features to display based on their importance scores.
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{performance_metrics}{A vector of aggregated performance metrics (e.g., sensitivity, specificity, accuracy, etc.).}
#'   \item{feature_importance}{A matrix containing the importance of the top `nTopImportance` features, ordered by their importance score.}
#'   \item{raw_results}{A list containing raw results for debugging or further analysis, including sensitivity, specificity, accuracy, and Gini scores across all test repetitions.}
#' }
#'
#' @details
#' The function splits the input data into training and testing sets based on the specified number of test repetitions (`testReps`).
#' During each iteration, it trains a Random Forest model and makes predictions on the test data. Indeterminate predictions are handled
#' by marking them as `NA`. The function tracks performance metrics such as sensitivity, specificity, and accuracy, and computes the
#' top `nTopImportance` features based on either Mean Decrease Accuracy or Mean Decrease Gini.
#'
#' @examples
#' \dontrun{
#' #Example usage of the function
#' result <- get_rf_model_output_cv_imp(
#'   scores_df = your_data,
#'   Undersample = FALSE,
#'   best.m = 3,
#'   testReps = 5,
#'   indeterminateUpper = 0.8,
#'   indeterminateLower = 0.2,
#'   Type = 1,
#'   nTopImportance = 10
#' )
#'
#' #View performance metrics
#' print(result$performance_metrics)
#'
#' #View top features by importance
#' print(result$feature_importance)
#' }
#'
#' @import randomForest
#' @import caret
#' @import stats
#' @export



get_zone_exclusioned_rf_model_cv_imp <- function(ml_formatted_scores_df,
                                      Undersample = FALSE,
                                      best.m = NULL, # any numeric value or call function to get it
                                      testReps, # testRps must be at least 2;
                                      indeterminateUpper,
                                      indeterminateLower,
                                      Type,
                                      nTopImportance) {

  rfData <- scores_data_df
    #---------------------------------------------------------------------
    # Initialize model performance metric trackers------------------------
    #---------------------------------------------------------------------

    # custom function definition
    `%ni%` <- Negate('%in%')

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

      # ind <- sample(2, nrow(rfData), replace = T, prob = c((1- testHoldBack), testHoldBack))
      train <- rfData[trainIndex,]

      #train_data_two <- train

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

      #train_data_two <- train


      #model building with current iteration train data
      # Train Random Forest model--------------------------------------------
      rf <- randomForest::randomForest(Target_Organ ~ ., data=train, mytry = best.m,
                                       importance = T, ntree = 500, proximity = T)

      print(rf)

      #----------------------------------------------------------------------
      #predictions with current model  with current test data
      # @___________________this_line_has_problems_______
      # Predict probabilities on test data
      #----------------------------------------------------------------------

      p2r <- stats::predict(rf, test, type = 'prob')[,1]

      #Store these predictions in a structured data frame
      rfTestData[names(p2r), i] <- as.numeric(p2r)


      #--------------------------------------------------------------------------
      #--------------------------------------------------------------------------
      #--------------------------------------------------------------------------
      #Identifying Indeterminate Predictions (Tracking Indeterminate Predictions)
      #Keeps track of the proportion of indeterminate predictions in each iteration
      #Proportion Tracking
      #------------------------------------------------------------------------
      #------------------------------------------------------------------------

      indeterminateIndex <- which((p2r < indeterminateUpper)&(p2r > indeterminateLower))

      #Calculating the Proportion of Indeterminate Predictions
      #Sets the indeterminate predictions to NA, effectively marking them
      #as missing or invalid.
      nRemoved <- c(nRemoved, length(indeterminateIndex)/length(p2r))

      #Handling Indeterminate Predictions
      p2r[indeterminateIndex] <- NA

      #Rounding the Predictions:
      p2r <- round(p2r)


      # Compute confusion matrix and extract metrics using "caret" package----

      Results <- caret::confusionMatrix(factor(p2r, levels = c(1, 0)), factor(test$Target_Organ, levels = c(1, 0)))
      Sensitivity <- c(Sensitivity, Results$byClass[['Sensitivity']])
      Specificity <- c(Specificity, Results$byClass[['Specificity']])
      PPV <- c(PPV, Results$byClass[['Pos Pred Value']])
      NPV <- c(NPV, Results$byClass[['Neg Pred Value']])
      Prevalence <- c(Prevalence, Results$byClass[['Prevalence']])
      Accuracy <- c(Accuracy, Results$byClass[['Balanced Accuracy']])


      # Aggregate Gini importance scores
      giniTmp <-  randomForest::importance(rf, type = Type)
      if (exists('gini')) {
        gini <- cbind(gini, giniTmp)
      } else {
        gini <- giniTmp
      }
    }


    #------------------------------------------------------------------------
    # Performance Summary
    #-------------------------------------------------------------------------

    PerformanceMatrix <- cbind(Sensitivity,
                               Specificity,
                               PPV,
                               NPV,
                               Prevalence,
                               Accuracy,
                               nRemoved)
    PerformanceSummary <- colMeans(PerformanceMatrix, na.rm = T)
    print(PerformanceSummary)

    #-------------------------------------------------------------------------
    # Feature Importance------------------------------------------------------
    #-------------------------------------------------------------------------

    print("Feature Importance (Mean Decrease):")
    print(sort(rowMeans(gini), decreasing = T))


    #-------------------------------------------------------------------------
    # Top Important Features--------------------------------------------------
    #--------------------------------------------------------------------------
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
    # dotchart(imp[ord, 1], xlab = colnames(imp)[1], ylab = "",
    #          main = paste0('Top ', nrow(imp), ' - Variable Importance'))#, xlim = c(xmin, max(imp[, i])))
    # # varImpPlot(rf,
    # #            sort = T,
    # #            n.var = 20,
    # #            main = "Top 20 - Variable Importance")
print(".............................................................................")
    print(PerformanceSummary)

    return(list(
      performance_metrics = PerformanceSummary,  # Aggregated performance metrics
      feature_importance = imp,                  # Top n features by importance
      raw_results = list(                        # Raw data for debugging or extended analysis
        sensitivity = Sensitivity,
        specificity = Specificity,
        accuracy = Accuracy,
        gini_scores = gini
      )
    ))

  }
