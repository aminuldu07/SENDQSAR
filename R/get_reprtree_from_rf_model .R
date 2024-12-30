
get_reprtree_from_rf_model <- function ( Data=NULL,
                                         path_db,
                                         rat_studies=FALSE,
                                         studyid_metadata=NULL,
                                         fake_study = FALSE,
                                         use_xpt_file = FALSE,
                                         Round = FALSE,
                                         Impute = FALSE,
                                         reps,
                                         holdback,
                                         Undersample = FALSE,
                                         hyperparameter_tuning = FALSE,
                                         error_correction_method) { # = must be 'Flip' or "Prune' or 'None'

  browser()
  if(is.null(Data)){
    data_and_best.m <- get_Data_formatted_for_ml_and_best.m(path_db=path_db,
                                                   rat_studies=rat_studies,
                                                   studyid_metadata=studyid_metadata,
                                                   fake_study = fake_study,
                                                   use_xpt_file = use_xpt_file,
                                                   Round =  Round,
                                                   Impute = Impute,
                                                   reps=reps,
                                                   holdback=holdback,
                                                   Undersample = Undersample,
                                                   hyperparameter_tuning = hyperparameter_tuning,
                                                   error_correction_method=error_correction_method) # = must be 'Flip' or "Prune' or 'None'

        }
  browser()
    Data <- data_and_best.m[["Data"]]
    best.m <- data_and_best.m[["best.m"]]

    # First way---------------------------------------------------------
    # Use a single random split (e.g., 70% train, 30% test)
    set.seed(123)  # Ensure reproducibility
    trainIndex <- sample(seq(nrow(Data)), size = floor(0.7 * nrow(Data)), replace = FALSE)
    testIndex <- setdiff(seq(nrow(Data)), trainIndex)

    train <- Data[trainIndex, ]
    test <- Data[testIndex, ]

    if (Undersample == T) {

      posIndex <- which(train[,1] == 1)
      nPos <- length(posIndex)
      trainIndex <- c(posIndex, sample(which(train[,1] == 0), nPos, replace = T))
      train <- train[trainIndex,]
      test <- rbind(train[-trainIndex,], test)
    }

    # # Second wat----------------------------------------------------
    # # Use a single random split (e.g., 70% train, 30% test)
    # set.seed(123)  # Ensure reproducibility
    # trainIndex <- sample(seq(nrow(rfData)), size = floor(0.7 * nrow(rfData)), replace = FALSE)
    # testIndex <- setdiff(seq(nrow(rfData)), trainIndex)
    #
    # train <- rfData[trainIndex, ]
    # test <- rfData[testIndex, ]
    #
    # # Handle undersampling if required
    # if (Undersample == TRUE) {
    #
    #   # Find indices of positive samples in the training data
    #   posIndex <- which(train[, 1] == 1)
    #   nPos <- length(posIndex)
    #
    #   # Sample the same number of negative samples (undersampling)
    #   negIndex <- sample(which(train[, 1] == 0), nPos, replace = FALSE)
    #
    #   # Create balanced training data
    #   balancedIndex <- c(posIndex, negIndex)
    #   train <- train[balancedIndex, ]
    # }


    # Train a Random Forest model using the specified mtry value
    rfAll <- randomForest::randomForest(Target_Organ ~ .,
                                        data = Data,
                                        mytry = best.m,
                                        importance = FALSE,
                                        ntree = 500,
                                        proximity = TRUE)

    ReprTree <- reprtree::ReprTree(rfAll,
                         train,
                         metric='d2')

    plot.reprtree(ReprTree(rfAll, train, metric='d2'))

}
