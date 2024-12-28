
org_train_eval_rf_with_cv_imp <- function(scores_df=NULL,
                                      path_db = NULL, # need when scores_df is NULL and need to calculate it
                                      studyid_metadata=NULL,
                                      use_xpt_file = FALSE,
                                      fake_study= FALSE,
                                      Impute = FALSE,
                                      Round =FALSE,
                                      reps, # from 0 to any numeric number
                                      holdback, # either 1 or fraction value like 0.75 etc.
                                      Undersample = FALSE,
                                      hyperparameter_tuning = FALSE,
                                      error_correction_method = NULL,
                                      best.m = NULL, # any numeric value or call function to get it
                                      testReps, # testRps must be at least 2;
                                      indeterminateUpper = .75,
                                      indeterminateLower = .25,
                                      Type,
                                      nTopImportance) {

  # Read the STUDYID metadata csv file
  # fake_80_medata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_80_MD.csv",
  #                            header = TRUE, sep = ",", stringsAsFactors = FALSE)


  #'C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_xpt'
  #path_db = path_db
  # if scores_df is not provided, it must be calculated and need path_db
  # Check if scores_df is NULL and calculate if necessary
  if (is.null(scores_df)) {

    if(use_xpt_file){

      studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)

    } else {
      # Helper function to fetch data from SQLite database
      fetch_domain_data <- function(db_connection, domain_name) {
        # Convert domain name to uppercase
        domain_name <- toupper(domain_name)
        # Create SQL query statement
        query_statement <- paste0('SELECT * FROM ', domain_name)
        # Execute query and fetch the data
        query_result <- DBI::dbGetQuery(db_connection, statement = query_statement)
        # Return the result
        query_result
      }
      # Establish a connection to the SQLite database
      db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path_db)

      # Fetch data for required domains
      dm <- fetch_domain_data(db_connection, 'dm')

      # Close the database connection
      DBI::dbDisconnect(db_connection)

      # get the studyids from the dm table

      studyid_or_studyids <- as.vector(unique(dm$STUDYID)) # unique STUDYIDS from DM table
    }


    # Calculate the liver scores for lb, mi and oM domain
    calculated_liver_scores <- get_liver_om_lb_mi_tox_score_list(studyid_or_studyids = studyid_or_studyids,
                                                             path_db = path_db,
                                                             fake_study = fake_study,
                                                             use_xpt_file = use_xpt_file,
                                                             output_individual_scores = TRUE,
                                                             output_zscore_by_USUBJID = FALSE)

    # Harmonize the column for the calculated_liver_scores

    column_harmonized_liverscr_df <- get_col_harmonized_scores_df(liver_score_data_frame = calculated_liver_scores,
                                                                  Round = TRUE)
  }


  # get the rfData and best.m value------------------------------------------
  if (is.null(best.m)){

    rfData_and_best_m <- prepare_data_and_tune_hyperparameters( scores_df = column_harmonized_liverscr_df,
                                                                studyid_metadata =studyid_metadata,
                                                                Impute = Impute,
                                                                Round =Round,
                                                                reps=reps,
                                                                holdback=holdback,
                                                                Undersample = Undersample,
                                                                hyperparameter_tuning = hyperparameter_tuning,
                                                                error_correction_method=error_correction_method)

    rfData <- rfData_and_best_m[["rfData"]]
    best.m <- rfData_and_best_m[["best.m"]]

  }


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
browser()
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
