
get_imp_features_from_rf_model_with_cv <- function(Data=NULL, #scores_df
                                                  Undersample = FALSE,
                                                  best.m = NULL, # any numeric value or call function to get it
                                                  testReps, # testRps must be at least 2;
                                                  Type,
                                                  nTopImportance) {


    rfData <- Data #rfData <- scores_df
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
