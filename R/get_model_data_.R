

get_model_data <- function(scores_df,
                           studyid_metadata,
                           Impute = FALSE,
                           Round =FALSE){
browser()
  input_scores_df <- scores_df
  metadata_df <- studyid_metadata


  metadata_df$STUDYID <- as.character(metadata_df$STUDYID)

  # Join metadata with the data
  merged_Data <- dplyr::left_join(metadata_df, input_scores_df, by = "STUDYID"  )

  ###-----------------------rfData preparation------------------------------------
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

return(rfData)
}
