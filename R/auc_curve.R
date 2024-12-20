

get_auc_curve <- function(rfData,# Input data frame for training
                          best.m  # The 'mtry' hyperparameter for Random Forest
                          ) {

  # Check if rfData is NULL
  if (is.null(rfData)) {
    # logic for generating rfData

  }

  # Train a Random Forest model using the specified mtry value
  rfAll <- randomForest::randomForest(Target_Organ ~ ., data=rfData, mytry = best.m,
                                      importance = F, ntree = 500, proximity = T)
  pred1= stats::predict(rfAll,type = "prob")
  perf = ROCR::prediction(pred1[,1], levels(rfData[,1])[rfData[,1]])
  # 1. Area under curve
  auc = ROCR::performance(perf, "auc")
  AUC <- auc@y.values[[1]]
  print(AUC)
  # 2. True Positive and Negative Rate
  pred3 = ROCR::performance(perf, "tpr","fpr") # check the ROCR packge assignment here
  # 3. Plot the ROC curve
  plot(pred3,main=paste0("ROC Curve for Random Forest (AUC = ", round(AUC, digits = 3), ")"),col=2,lwd=2)
  abline(a=0,b=1,lwd=2,lty=2,col="gray")
}
