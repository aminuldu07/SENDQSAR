
evaluate_model <- function(model, data) {


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
#}

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
#}
