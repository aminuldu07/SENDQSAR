train_and_evaluate_rf_model <- function(scores_df,
                                        studyid_metadata,
                                        Impute = FALSE,
                                        Round =FALSE,
                                        reps, # from 0 to any numeric number
                                        holdback, # either 1 or fraction value like 0.75 etc.
                                        Undersample = FALSE,
                                        hyperparameter_tuning = FALSE,
                                        error_correction_method = NULL,
                                        testReps,
                                        best.m) {


  rfData_and_best_m <- prepare_data_and_tune_hyperparameters( scores_df = scores_df,
                                                          studyid_metadata =studyid_metadata,
                                                          Impute = Impute,
                                                          Round =Round,
                                                          reps=reps,
                                                          holdback=holdback,
                                                          Undersample = Undersampl,
                                                          hyperparameter_tuning = hyperparameter_tuning,
                                                          error_correction_method=error_correction_method)

    rfData <- rfData_and_best_m[["rfData"]]
    best.m <- rfData_and_best_m[["best.m"]]

    # Initialize model performance metric trackers
    Sensitivity <- NULL
    Specificity <- NULL
    PPV <- NULL
    NPV <- NULL
    Prevalence <- NULL
    Accuracy <- NULL
    nRemoved <- NULL


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

      # rfAll <- randomForest::randomForest(indst_TO ~ ., data=rfData, mytry = best.m,
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
      rf <- randomForest::randomForest(indst_TO ~ ., data=train, mytry = best.m,
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

      Results <- caret::confusionMatrix(factor(p2r, levels = c(1, 0)), factor(test$indst_TO, levels = c(1, 0)))
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

    return(list(metrics = metrics, rfData = rfData))
  }
