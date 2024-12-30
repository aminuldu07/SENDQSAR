


###-----------------------@ROC-Curve-and-AUC------------------------------------
# @ROC-Curve-and-AUC~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# pred1= stats::predict(rfAll,type = "prob")
# perf = ROCR::prediction(pred1[,1], levels(rfData[,1])[rfData[,1]])
# # 1. Area under curve
# auc = ROCR::performance(perf, "auc")
# AUC <- auc@y.values[[1]]
# print(AUC)
# # 2. True Positive and Negative Rate
# pred3 = ROCR::performance(perf, "tpr","fpr") # check the ROCR packge assignment here
# # 3. Plot the ROC curve
# plot(pred3,main=paste0("ROC Curve for Random Forest (AUC = ", round(AUC, digits = 3), ")"),col=2,lwd=2)
# abline(a=0,b=1,lwd=2,lty=2,col="gray")









###-----------------------Visualization-and-Saving-Results----------------------
# @Visualization-and-Saving-Results~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#reprtree:::plot.reprtree(reprtree::ReprTree(rfAll, train, metric='d2'))

ReprTree <- ReprTree(rfAll, train, metric='d2')

plot.reprtree(ReprTree(rfAll, train, metric='d2'))

# saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '.rds'))

histoData <- as.data.frame(cbind(rowMeans(rfTestData, na.rm = T), rfData[,1]))
histoData[which(histoData[,2] == 1), 2] <- 'Y'
histoData[which(histoData[,2] == 2), 2] <- 'N'
colnames(histoData) <- c('Probability', 'LIVER')

H <- p <- histoData %>%
  ggplot2::ggplot( ggplot2::aes(x=Probability, fill=LIVER)) +
  ggplot2::geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  ggplot2::scale_fill_manual(values=c("#69b3a2", "#404080")) +
  # theme_ipsum() +
  ggplot2::labs(fill = "LIVER", x = "Model Prediction P(LIVER)", y = "Count")



