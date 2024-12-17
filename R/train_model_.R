
train_random_forest_model <- function(rfData,
                                      reps,
                                      holdback,
                                      Undersample = FALSE,
                                      hyperparameter_tuning = FALSE,
                                      ErrorMethod = 'Flip',
                                      threshold = 0.5,
                                      Round = TRUE) {

  ########--------prepare training and testing data sets for model building-------------
  # Error handling counter
  count <- 0

  if (reps > 0) {
    for (rep in seq(reps)) {
      print(paste0(rep / reps * 100, '% Complete...'))

      # Split training and testing data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      if (holdback == 1) {
        #If "holdback == 1", a single instance is sampled for the test set,
        # and the rest are used for training:::  Randomly assigns each row to
        #the training set (1) or test set (2) based on the holdback proportion.

        ind <- sample(seq(nrow(rfData)), 1) # selection of single rows
        train <- rfData[-ind, ]
        test <- rfData[ind, ]
        testIndex <- ind
      } else {
        #Otherwise, the data is split into training and testing sets based
        # on the holdback proportion.
        ## categorizing all rows into two groups based on probabilities.

        ind <- sample(2, nrow(rfData), replace = TRUE, prob = c((1 - holdback), holdback))
        train <- rfData[ind == 1, ]
        test <- rfData[ind == 2, ]
        testIndex <- which(ind == 2)
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Under sampling for balancing the positive and negative instances
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Undersample majority class if specified~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      if (Undersample) {
        posIndex <- which(train[, 1] == 1)
        nPos <- length(posIndex)
        trainIndex <- c(posIndex, sample(which(train[, 1] == 0), nPos, replace = FALSE))
        train <- train[trainIndex, ]
      }



      ### Perform hyperparameter tuning if specified~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ###----------------perform hyperparameter tuning and get model --------
      if (hyperparameter_tuning) {
        # "mtry" in Caret Package Grid Search
        #sets up the cross-validation method.
        control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
        metric <- "Accuracy"
        mtry <- sqrt(ncol(train))
        tunegrid <- expand.grid(.mtry = mtry)

        #Grid and Random Search (caret package)
        rf_default <- train(indst_TO ~ ., data = train, method = "rf", metric = metric, tuneGrid = tunegrid, trControl = control)
        rf_random <- train(indst_TO ~ ., data = train, method = "rf", metric = metric, tuneLength = 15, trControl = control)

        #mtry in Random Forest Parameter Tuning
        #tuning on the mtry parameter
        mtry <- tuneRF(rfData[,-1], rfData[,1], ntreeTry=500,
                       stepFactor=1.5,improve=0.01, trace=F, plot=F)
        best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
        print(mtry)
        print(best.m)

      } else {
        best.m <- 4
      }

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Train the Random Forest Model
      rf <- randomForest::randomForest(indst_TO ~ .,
                                       data = train,
                                       mtry = best.m,
                                       importance = FALSE,
                                       ntree = 500,
                                       proximity = FALSE)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      ###------------Error-Handling-(Flip-or-Prune)from "model predictions"-----------
      ###------------Errored rows adjustment and final data set preparation-----------


      # Handle prediction errors if required
      if (ErrorMethod %in% c('Flip', 'Prune')) {
        p <- stats::predict(rf, test, type = 'prob')[, 1]

        flipLiverIndex <- testIndex[which((rfData[testIndex, 'indst_TO'] == 1) & (as.numeric(p) < threshold))]
        flipnot_LiverIndex <- testIndex[which((rfData[testIndex, 'indst_TO'] == 0) & (as.numeric(p) > (1 - threshold)))]

        if (length(flipLiverIndex) > 0) {
          count <- count + 1
          if (ErrorMethod == 'Flip') {
            rfData[flipLiverIndex, 1] <- 0
          } else {
            rfData <- rfData[-flipLiverIndex, ]
          }
          saveRDS(rfData, paste0('rfData_', as.integer(rep), '_', threshold, '_', holdback, '_', ErrorMethod, '_Round_', count, '.rds'))
        }

        if (length(flipnot_LiverIndex) > 0) {
          count <- count + 1
          if (ErrorMethod == 'Flip') {
            rfData[flipnot_LiverIndex, 1] <- 1
          } else {
            rfData <- rfData[-flipnot_LiverIndex, ]
          }
          saveRDS(rfData, paste0('rfData_', as.integer(rep), '_', threshold, '_', holdback, '_', ErrorMethod, '_Round_', count, '.rds'))
        }
      }
    }
  }
}
