
prepare_data_and_tune_hyperparameters <- function(scores_df,
                                                  studyid_metadata,
                                                  Impute = FALSE,
                                                  Round =FALSE,
                                                  reps, # from 0 to any numeric number
                                                  holdback,
                                                  Undersample = FALSE,
                                                  hyperparameter_tuning = FALSE,
                                                  correction_method) {




  input_scores_df <- scores_df
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

  #######-----------------------missing values imputation---------------------------
  ##missing values imputation
  if (Impute == T) {
    rfData <- randomForest::rfImpute(Target_Organ ~ ., rfData)

    if (Round == T) {
      zscoreIndex <- c(grep('avg_', colnames(rfData)), grep('liver', colnames(rfData)))
      for (i in zscoreIndex) {
        rfData[, i] <- floor(rfData[, i])
        maxIndex <- which(rfData[, i] > 5)
        rfData[maxIndex, i] <- 5
      }
      histoIndex <- which(substr(colnames(rfData), 1, 1) %in% toupper(letters))
      histoIndex <- histoIndex[-1]
      for (i in histoIndex) {
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
      if (Undersample) {
      posIndex <- which(train[, 1] == 1)
      nPos <- length(posIndex)
      trainIndex <- c(posIndex, sample(which(train[, 1] == 0), nPos, replace = FALSE))
      train <- train[trainIndex, ]
      }

     if(hyperparameter_tuning == T) {

      control <- trainControl(method="repeatedcv", number=10, repeats=3)
      metric <- "Accuracy"
      mtry <- sqrt(ncol(train))
      tunegrid <- expand.grid(.mtry=mtry)

      #Grid and Random Search (caret package)
      rf_default <- train(indst_TO~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
      rf_random <- train(indst_TO~., data=train, method="rf", metric=metric, tuneLength=15, trControl=control)

      mtry <- tuneRF(rfData[,-1], rfData[,1], ntreeTry=500,
                   stepFactor=1.5,improve=0.01, trace=F, plot=F)
      best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

      } else {

      best.m <- 4
      }

    # Correction of the labels of the train and test data set
    # using a rf model

    rf_for_correction <- randomForest::randomForest(indst_TO ~ .,
                                                  data=train,
                                                  mytry = best.m,
                                                  importance = F,
                                                  ntree = 500,
                                                  proximity = F)


  # train and test data correction/adjustment baed on method
  #---------------------------------------------------------------------------
  if ((ErrorMethod == 'Flip')|(ErrorMethod == 'Prune')) {

    # Prediction on the test data for correction
    p <- stats::predict(rf_for_correction, test, type = 'prob')[,1]

    # Find indices to flip or prune
    flipLiverIndex <- testIndex[which((rfData[testIndex, 'indst_TO'] == 1)&(as.numeric(p) < threshold))]
    flipnot_LiverIndex <- testIndex[which((rfData[testIndex, 'indst_TO'] == 0)&(as.numeric(p) > (1 - threshold)))]

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

     }

    }

  }

  if (c("flip","prune" %in% correction_method)) {
    rfData <- readRDS(rfDataRDS) # (if data were flipped or prunned)
  } else {
    return(rfData=rfData)
  }

}
