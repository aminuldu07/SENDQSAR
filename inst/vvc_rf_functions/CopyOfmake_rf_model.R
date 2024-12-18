
#' @title get_random_forest_model
#' @param Liver_get_liver_om_lb_mi_tox_score_list Mandatory, character \cr
#'   Studyid number
#' @param not_Liver_get_liver_om_lb_mi_tox_score_list Mandatory, character \cr
#'   path of database
#' @return score
#'
#' @examples
#' \dontrun{
#' get_liver_lb_score(studyid='1234123', database_path = dbtoken)
#' }
#' @export
#'



get_random_forest_model <- function( Data,
                                     Impute = TRUE,
                                     ErrorMethod = 'Prune', # Choose: "Flip" or "Prune" or "None"
                                     Undersample = TRUE,
                                     reps <- 0,
                                     threshold = 0.05,
                                     holdback = 1,
                                     testReps = 5,
                                     testHoldBack = 0.2,
                                     Round = T,
                                     generateBarPlot = T,
                                     nTopImportance = 20,
                                     indeterminateUpper = .75,
                                     indeterminateLower = .25,
                                     Type = 1,
                                     hyperparameter_tuning = F ){

  # rfData <- Data
  #
  # `%ni%` <- Negate('%in%')
  #
  #
  #
  # rfData <- Data[, -2]
  # rfData[which(rfData$indst_TO == 'Liver'), 1] <- 1
  # rfData[which(rfData$indst_TO == 'not_Liver'), 1] <- 0
  # # rfData[,1] <- as.numeric(rfData[,1])
  # rfData[,1] <- factor(rfData[,1], levels = c(1, 0))
  #
  #
  #  ##missing values imputation
  # if (Impute == T) {
  #   rfData <- randomForest::rfImpute(indst_TO ~ ., rfData)
  #
  #   if (Round == T) {
  #     zscoreIndex <- c(grep('avg_', colnames(rfData)), grep('liver', colnames(rfData)))
  #     for (i in zscoreIndex) {
  #       rfData[, i] <- floor(rfData[, i])
  #       maxIndex <- which(rfData[, i] > 5)
  #       rfData[maxIndex, i] <- 5
  #     }
  #     histoIndex <- which(substr(colnames(rfData), 1, 1) %in% toupper(letters))
  #     histoIndex <- histoIndex[-1]
  #     for (i in histoIndex) {
  #       rfData[, i] <- ceiling(rfData[, i])
  #     }
  #   }
  # }
  #
  #
  # count <- 0
  # if (reps > 0) {
  #   for (rep in seq(reps)) {
  #     print(paste0(rep/reps*100, '% Complete...'))
  #
  #     if (holdback == 1) {
  #
  #       train <- rfData[-ind,]
  #       test <- rfData[ind,]
  #       testIndex <- ind
  #     } else {
  #       ind <- sample(2, nrow(rfData), replace = T, prob = c((1- holdback), holdback))
  #       train <- rfData[ind==1,]
  #       test <- rfData[ind==2,]
  #       testIndex <- which(ind == 2)
  #     }
  #
  #     # Under sampling for balancing the positive and negative instances
  #     if (Undersample == T) {
  #       #If Undersample == TRUE, the training set is balanced by
  #       #undersampling the majority class
  #       posIndex <- which(train[,1] == 1)
  #       nPos <- length(posIndex)
  #       trainIndex <- c(posIndex, sample(which(train[,1] == 0), nPos, replace = F))
  #       train <- train[trainIndex,]
  #     }
  #
  #
  #   if(hyperparameter_tuning == T) {
  #
  #      control <- trainControl(method="repeatedcv", number=10, repeats=3)
  #      metric <- "Accuracy"
  #      mtry <- sqrt(ncol(train))
  #      tunegrid <- expand.grid(.mtry=mtry)
  #
  #      #Grid and Random Search (caret package)
  #      rf_default <- train(indst_TO~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
  #      rf_random <- train(indst_TO~., data=train, method="rf", metric=metric, tuneLength=15, trControl=control)
  #
  #      mtry <- tuneRF(rfData[,-1], rfData[,1], ntreeTry=500,
  #                    stepFactor=1.5,improve=0.01, trace=F, plot=F)
  #      best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  #
  #   }
  #
  #
  #     best.m <- 4
  #
  #     rf <- randomForest::randomForest(indst_TO ~ ., data=train, mytry = best.m, importance = F, ntree = 500, proximity = F)
  #
  #     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #
  #     if ((ErrorMethod == 'Flip')|(ErrorMethod == 'Prune')) {
  #       # Predict probabilities (using random_forest_package)
  #       p <- stats::predict(rf, test, type = 'prob')[,1]
  #
  #       # Find indices to flip or prune
  #       flipLiverIndex <- testIndex[which((rfData[testIndex, 'indst_TO'] == 1)&(as.numeric(p) < threshold))]
  #       flipnot_LiverIndex <- testIndex[which((rfData[testIndex, 'indst_TO'] == 0)&(as.numeric(p) > (1 - threshold)))]
  #
  #       #If there are indices to flip or prune, the data set is modified
  #       if (length(flipLiverIndex) > 0) {
  #         count <- count + 1
  #         if (ErrorMethod == 'Flip') {
  #           # Flip 'Liver' to 'not_Liver'
  #           rfData[flipLiverIndex, 1] <- 0
  #         } else {
  #           # Prune (remove) 'Liver' instances
  #           rfData <- rfData[-flipLiverIndex,]
  #         }
  #         if (Round == T) {
  #           # Save the modified dataset
  #           saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_Round_', count, '.rds'))
  #         } else {
  #           # Save the modified dataset
  #           saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_', count, '.rds'))
  #         }
  #       }
  #       if (length(flipnot_LiverIndex) > 0) {
  #         count <- count + 1
  #         if (ErrorMethod == 'Flip') {
  #           # Flip 'not_Liver' to 'Liver'
  #           rfData[flipnot_LiverIndex, 1] <- 1
  #         } else {
  #           # Prune (remove) 'not_Liver' instances
  #           rfData <- rfData[-flipnot_LiverIndex,]
  #         }
  #         if (Round == T) {
  #           # Save the modified dataset
  #           saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_Round_', count, '.rds'))
  #         } else {
  #           # Save the modified dataset
  #           saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_', count, '.rds'))
  #
  #         }
  #       }
  #     }
  #   }
  # }


  #rfData <- readRDS(rfDataRDS) # (if data were flipped or prunned)
  rfData <- rfData
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
  rfTestData <- rfTestData[,1:2]

  #remove 'gini' from the previous iteration
  if (exists('gini')) {rm(gini)}

  # model building and testing---------------------------
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

    train_data_two <- train

    test <- rfData[testIndex,]

    rfAll <- randomForest::randomForest(indst_TO ~ ., data=rfData, mytry = best.m,
                          importance = F, ntree = 500, proximity = T)


    if (Undersample == T) {

      posIndex <- which(train[,1] == 1)
      nPos <- length(posIndex)
     # trainIndex <- c(posIndex, sample(which(train[,1] == 0), nPos, replace = F))
      trainIndex <- c(posIndex, sample(which(train[,1] == 0), nPos, replace = T))
      train <- train[trainIndex,]
      test <- rbind(train[-trainIndex,], test)
    }

    train_data_two <- train


    #model building with current train data
    rf <- randomForest::randomForest(indst_TO ~ ., data=train, mytry = best.m,
                       importance = T, ntree = 500, proximity = T)

    print(rf)

    #predictions with current model  with current test data

    # @___________________this_line_has_problems_______
    p2r <- stats::predict(rf, test, type = 'prob')[,1]

    #Store these predictions in a structured dataframe
    rfTestData[names(p2r), i] <- as.numeric(p2r)

    #Identifying Indeterminate Predictions (Tracking Indeterminate Predictions)
    #Keeps track of the proportion of indeterminate predictions in each iteration
    #Proportion Tracking
    indeterminateIndex <- which((p2r < indeterminateUpper)&(p2r > indeterminateLower))

    #Calculating the Proportion of Indeterminate Predictions
    #Sets the indeterminate predictions to NA, effectively marking them
    #as missing or invalid.
    nRemoved <- c(nRemoved, length(indeterminateIndex)/length(p2r))

    #Handling Indeterminate Predictions
    p2r[indeterminateIndex] <- NA

    #Rounding the Predictions:
    p2r <- round(p2r)


    #"confusionMatrix" function from the caret package
    Results <- caret::confusionMatrix(factor(p2r, levels = c(1, 0)), factor(test$indst_TO, levels = c(1, 0)))
    Sensitivity <- c(Sensitivity, Results$byClass[['Sensitivity']])
    Specificity <- c(Specificity, Results$byClass[['Specificity']])
    PPV <- c(PPV, Results$byClass[['Pos Pred Value']])
    NPV <- c(NPV, Results$byClass[['Neg Pred Value']])
    Prevalence <- c(Prevalence, Results$byClass[['Prevalence']])
    Accuracy <- c(Accuracy, Results$byClass[['Balanced Accuracy']])

    giniTmp <-  randomForest::importance(rf, type = Type)
    if (exists('gini')) {
      gini <- cbind(gini, giniTmp)
    } else {
      gini <- giniTmp
    }
  }

  # Performance-and-Importance-Analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #performance summary for the model and  dotchart of the top important
  #variables based on their mean decrease in accuracy or Gini index
  PerformanceMatrix <- cbind(Sensitivity,
                             Specificity,
                             PPV, NPV,
                             Prevalence,
                             Accuracy,
                             nRemoved)
  PerformanceSummary <- colMeans(PerformanceMatrix, na.rm = T)
  print(PerformanceSummary)
  print(sort(rowMeans(gini), decreasing = T))

  imp <- as.matrix(rowMeans(gini)[1:nTopImportance])
  if (Type == 1) {
    colnames(imp) <- 'MeanDecreaseAccuracy'
  } else {
    colnames(imp) <- 'MeanDecreaseGini'
  }
  ord <- order(imp[,1])
  dotchart(imp[ord, 1], xlab = colnames(imp)[1], ylab = "",
           main = paste0('Top ', nrow(imp), ' - Variable Importance'))#, xlim = c(xmin, max(imp[, i])))
  # varImpPlot(rf,
  #            sort = T,
  #            n.var = 20,
  #            main = "Top 20 - Variable Importance")

}


