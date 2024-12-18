
tune_hyperparameters <- function(train) {

  # "mtry" in Caret Package Grid Search
  #sets up the cross-validation method.
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  metric <- "Accuracy"
  mtry <- sqrt(ncol(train))
  tunegrid <- expand.grid(.mtry=mtry)
  #tunegrid <- expand.grid(.mtry = sqrt(ncol(train)))

  #Grid and Random Search (caret package)
  rf_default <- train(indst_TO~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
  #rf_random <- train(indst_TO~., data=train, method="rf", metric=metric, tuneLength=15, trControl=control)
  #print(rf_default)
  #print(rf_random)

  #mtry in Random Forest Parameter Tuning
  #tuning on the mtry parameter
  mtry <- tuneRF(rfData[,-1],
                 rfData[,1],
                 ntreeTry=500,
                 stepFactor=1.5,
                 improve=0.01,
                 trace=F,
                 plot=F)

  best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

  #best.m <- rf_default$bestTune$.mtry
  print(mtry)
  print(best.m)


  return(best.m)

}
