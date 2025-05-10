#' @title Generate Prediction Plot for Random Forest Model
#'
#'@description
#' This function performs model building and prediction using a random forest algorithm. It iterates over multiple test repetitions, training the model on the training data and predicting on the test data. After predictions are made, a histogram plot is generated to visualize the distribution of predicted probabilities for the outcome variable (`LIVER`).
#'
#' @param Data A data frame containing the dataset to use for training and testing. If `NULL`, the function will attempt to fetch and format the data from the database using `get_Data_formatted_for_ml_and_best.m` function.
#' @param path_db A string indicating the path to the database that contains the dataset.
#' @param rat_studies A logical flag indicating whether to use rat studies data. Defaults to `FALSE`.
#' @param studyid_metadata A data frame containing metadata related to the study IDs. Defaults to `NULL`.
#' @param fake_study A logical flag indicating whether to use fake study data. Defaults to `FALSE`.
#' @param use_xpt_file A logical flag indicating whether to use an XPT file. Defaults to `FALSE`.
#' @param Round A logical flag indicating whether to round the predictions. Defaults to `FALSE`.
#' @param Impute A logical flag indicating whether to impute missing values. Defaults to `FALSE`.
#' @param reps An integer specifying the number of repetitions for cross-validation.
#' @param holdback A numeric value indicating the proportion of data to hold back for testing during cross-validation.
#' @param Undersample A logical flag indicating whether to perform undersampling on the dataset to balance the classes. Defaults to `FALSE`.
#' @param hyperparameter_tuning A logical flag indicating whether to perform hyperparameter tuning. Defaults to `FALSE`.
#' @param error_correction_method A string specifying the error correction method to be used. Possible values are "Flip", "Prune", or "None".
#' @param testReps An integer specifying the number of test repetitions for model evaluation.
#'
#' @return A `ggplot` object representing the histogram of predicted probabilities for the `LIVER` variable across test repetitions.
#'
#' @details
#' The function works as follows:
#' - If `Data` is `NULL`, the function fetches the data and the best model configuration by calling the `get_Data_formatted_for_ml_and_best.m` function.
#' - The dataset is divided into training and test sets for each repetition (`testReps`).
#' - If `Undersample` is enabled, undersampling is applied to balance the dataset.
#' - A random forest model is trained on the training data and predictions are made on the test data.
#' - The predictions are averaged over the test repetitions and a histogram is plotted to visualize the distribution of predicted probabilities for `LIVER`.
#'
#' @examples
#' \dontrun{
#' # Example function call
#' get_prediction_plot(
#'   path_db = "path_to_db",
#'   rat_studies = FALSE,
#'   reps = 10,
#'   holdback = 0.2,
#'   Undersample = TRUE,
#'   hyperparameter_tuning = FALSE,
#'   error_correction_method = "Flip",
#'   testReps = 5
#' )
#' }
#'
#' @export


get_prediction_plot <- function(Data=NULL,
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
                                error_correction_method,
                                best.m = best.m,
                                testReps){


  # enforce that Data and best.m must either both be NULL or both be non-NULL
  if (xor(is.null(Data), is.null(best.m))) {
    stop("Error: Either both 'Data' and 'best.m' must be NULL or both must be non-NULL.")
  }

  if (is.null(Data) && is.null(best.m)){
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


    Data <- data_and_best.m[["Data"]]
    best.m <- data_and_best.m[["best.m"]]
  }

  rfData <- Data
  best.m <- best.m
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
  }


  #-------------------------------------------------------
  histoData <- as.data.frame(cbind(rowMeans(rfTestData, na.rm = T), rfData[,1]))
  histoData[which(histoData[,2] == 1), 2] <- 'Y'
  histoData[which(histoData[,2] == 2), 2] <- 'N'
  colnames(histoData) <- c('Probability', 'LIVER')

  H <- p <- histoData %>%
    ggplot2::ggplot( ggplot2::aes(x=Probability, fill=LIVER)) +
    ggplot2::geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    ggplot2::scale_fill_manual(values=c("#69b3a2", "#404080")) +
    ggplot2::labs(fill = "LIVER", x = "Model Prediction P(LIVER)", y = "Count")

print(H)

 }
