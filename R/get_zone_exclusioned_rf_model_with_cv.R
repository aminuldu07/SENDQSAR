#' @title Random Forest Model with Cross-validation and Exclusion
#' @description This function implements a Random Forest classification model
#'   with cross-validation and allows for undersampling, handling indeterminate
#'   predictions, and calculating various model performance metrics such as
#'   sensitivity, specificity, and accuracy. It tracks the proportion of indeterminate
#'   predictions and provides an aggregated performance summary across multiple test repetitions.
#'
#' @param Data A data frame containing the features and the target variable `Target_Organ`
#'   to train the Random Forest model on.
#'
#' @param Undersample A logical value indicating whether to perform undersampling to
#'   balance the classes in the training data. Defaults to `FALSE`.
#'
#' @param best.m A numeric value representing the best number of variables (`mytry`)
#'   to use at each split in the Random Forest model. This can be manually set or
#'   determined through optimization.
#'
#' @param testReps An integer specifying the number of test repetitions. This must
#'   be at least 2, as the function relies on multiple test sets to assess the model performance.
#'
#' @param indeterminateUpper A numeric value indicating the upper bound for the
#'   predicted probability to consider a prediction indeterminate. Predictions with
#'   probabilities within this range are marked as indeterminate.
#'
#' @param indeterminateLower A numeric value indicating the lower bound for the
#'   predicted probability to consider a prediction indeterminate. Predictions with
#'   probabilities within this range are marked as indeterminate.
#'
#' @param Type An integer indicating the type of feature importance to use in the
#'   Random Forest model. Typically, `1` for "Mean Decrease Accuracy" or `2` for "Mean Decrease Gini".
#'
#' @return A list containing two components:
#' \describe{
#'   \item{performance_metrics}{A vector with the aggregated performance metrics,
#'     including sensitivity, specificity, accuracy, and others, calculated across
#'     all test repetitions.}
#'   \item{raw_results}{A list containing the raw performance metrics for each repetition,
#'     including sensitivity, specificity, and accuracy.}
#' }
#'
#' @examples
#' \dontrun{
#' # Example usage
#' Data <- your_data_frame  # Replace with actual dataset
#' results <- get_zone_exclusioned_rf_model_with_cv(Data = Data,
#'                                                  Undersample = TRUE,
#'                                                best.m = 5,
#'                                                 testReps = 10,
#'                                                 indeterminateUpper = 0.8,
#'                                                 indeterminateLower = 0.2,
#'                                                  Type = 1)
#'
#'  View the aggregated performance metrics
#' print(results$performance_metrics)
#'
#'  Access raw results for further analysis
#' print(results$raw_results)
#' }
#'
#' @seealso \link[randomForest]{randomForest}, \link[caret]{confusionMatrix}
#'
#' @import randomForest
#' @import caret


get_zone_exclusioned_rf_model_with_cv <- function(scores_data_df, #scores_df
                                      Undersample = FALSE,
                                      best.m = NULL, # any numeric value or call function to get it
                                      testReps, # testRps must be at least 2;
                                      indeterminateUpper,
                                      indeterminateLower,
                                      Type) {

    rfData <- scores_data_df #rfData <- scores_df
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
      # giniTmp <-  randomForest::importance(rf, type = Type)
      # if (exists('gini')) {
      #   gini <- cbind(gini, giniTmp)
      # } else {
      #   gini <- giniTmp
      # }
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

    # print("Feature Importance (Mean Decrease):")
    # print(sort(rowMeans(gini), decreasing = T))


    #-------------------------------------------------------------------------
    # Top Important Features--------------------------------------------------
    #--------------------------------------------------------------------------
    # imp <- as.matrix(rowMeans(gini)[1:nTopImportance])
    # if (Type == 1) {
    #   colnames(imp) <- 'MeanDecreaseAccuracy'
    # } else {
    #   colnames(imp) <- 'MeanDecreaseGini'
    # }
    # ord <- order(imp[,1])

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
      raw_results = list(                        # Raw data for debugging or extended analysis
        sensitivity = Sensitivity,
        specificity = Specificity,
        accuracy = Accuracy
      )
    ))

  }
