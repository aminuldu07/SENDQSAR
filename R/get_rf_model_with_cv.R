#' @title Random Forest with Cross-Validation
#'
#' @description
#' This function builds a random forest model using the `randomForest` package, evaluates it through cross-validation,
#' and computes performance metrics such as sensitivity, specificity, and accuracy.
#' It optionally applies undersampling to handle class imbalance and supports custom settings for the number of predictors sampled at each split.
#'
#' @param Data Mandatory, data frame
#' #' First column is `Target_Organ`, followed by columns with score values.
#'   The input dataset, which must include a column named `Target_Organ` as the response variable.
#' @param Undersample Optional, logical
#'   If `TRUE`, balances the dataset by undersampling the majority class. Default is `FALSE`.
#' @param best.m Optional, numeric or `NULL`
#'   Specifies the number of predictors sampled at each split. If `NULL`, the default value of `randomForest` is used.
#' @param testReps Mandatory, integer
#'   The number of cross-validation repetitions. Must be at least 2.
#' @param Type Mandatory, numeric
#'   Specifies the importance metric type: `1` for Mean Decrease Accuracy or `2` for Gini.
#'
#' @return
#' A list with the following elements:
#' \itemize{
#'   \item \code{performance_metrics}: A vector of aggregated performance metrics, including sensitivity, specificity, and accuracy.
#'   \item \code{raw_results}: A list containing raw sensitivity, specificity, and accuracy values for each cross-validation fold.
#' }
#'
#' @details
#' This function splits the input data into training and testing subsets based on the specified `testReps` cross-validation folds.
#' If undersampling is enabled, the function balances the training set to reduce class imbalance.
#' A random forest model is trained on the training set, and predictions are evaluated on the test set. The results are aggregated to provide summary performance metrics.
#'
#' @examples
#' \dontrun{
#' # Load necessary libraries
#' library(randomForest)
#' library(caret)
#'
#' # Example dataset
#'
#' Data$Target_Organ <- ifelse(iris$Species == "setosa", 1, 0)
#' Data <- Data[, -5]  # Remove Species column
#'
#' # Run the function
#' results <- get_rf_model_with_cv(Data = iris,
#'                                Undersample = TRUE,
#'                                 best.m = 2,
#'                                 testReps = 5,
#'                                 Type = 2)
#'
#' # Print results
#' print(results$performance_metrics)
#' }
#'
#' @export


get_rf_model_with_cv <- function(ml_formatted_scores_df,
                                 Undersample = FALSE,
                                 best.m = NULL, # any numeric value or call function to get it
                                 testReps, # testRps must be at least 2;
                                 Type) {


# This functin must need a data input.
# This funcitno is is designed to work with the chanin  way.
# There is scond function with list of parmaters



    rfData <- ml_formatted_scores_df
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
    #nRemoved <- NULL


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
    #if (exists('gini')) {rm(gini)}


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


      # # Aggregate Gini importance scores
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
                               Accuracy)
    PerformanceSummary <- colMeans(PerformanceMatrix, na.rm = T)
    print(PerformanceSummary)

    # #-------------------------------------------------------------------------
    # # Feature Importance------------------------------------------------------
    # #-------------------------------------------------------------------------
    #
    # print("Feature Importance (Mean Decrease):")
    # print(sort(rowMeans(gini), decreasing = T))
    #
    #
    # #-------------------------------------------------------------------------
    # # Top Important Features--------------------------------------------------
    # #--------------------------------------------------------------------------
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
      #feature_importance = imp,                  # Top n features by importance
      raw_results = list(                        # Raw data for debugging or extended analysis
        sensitivity = Sensitivity,
        specificity = Specificity,
        accuracy = Accuracy

      )
    ))

}












# #Add a special case for testReps = 1 that directly splits data into train and test sets without looping or iterative sampling.
# if (testReps == 1) {
#   # Use a single random split (e.g., 70% train, 30% test)
#   set.seed(123)  # Ensure reproducibility
#   trainIndex <- sample(seq(nrow(rfData)), size = floor(0.7 * nrow(rfData)), replace = FALSE)
#   testIndex <- setdiff(seq(nrow(rfData)), trainIndex)
#
#   train <- rfData[trainIndex, ]
#   test <- rfData[testIndex, ]
#
# } else {
#   # Multiple iterations (original logic)
#   for (i in seq(testReps)) {
#     if (i == 1) {
#       sampleIndicies <- seq(nrow(rfData))
#     }
#     if (i < testReps) {
#       ind <- sample(seq(nrow(rfData)), floor((nrow(rfData) / testReps) - 1), replace = FALSE)
#       sampleIndicies <- sampleIndicies[which(sampleIndicies %ni% ind)]
#     } else {
#       ind <- sampleIndicies
#     }
#
#     trainIndex <- which(seq(nrow(rfData)) %ni% ind)
#     testIndex <- ind
#
#     train <- rfData[trainIndex, ]
#     test <- rfData[testIndex, ]
#   }
# }























#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------
#Identifying Indeterminate Predictions (Tracking Indeterminate Predictions)
#Keeps track of the proportion of indeterminate predictions in each iteration
#Proportion Tracking
#------------------------------------------------------------------------
#------------------------------------------------------------------------

# indeterminateIndex <- which((p2r < indeterminateUpper)&(p2r > indeterminateLower))
#
# #Calculating the Proportion of Indeterminate Predictions
# #Sets the indeterminate predictions to NA, effectively marking them
# #as missing or invalid.
# nRemoved <- c(nRemoved, length(indeterminateIndex)/length(p2r))
#
# #Handling Indeterminate Predictions
# p2r[indeterminateIndex] <- NA
#
# #Rounding the Predictions:
# p2r <- round(p2r)

