
#' @title get_random_forest_model
#' @param data_frame Mandatory, character \cr
#'   Studyid number
#' @param Liver_get_liver_om_lb_mi_tox_score_list Mandatory, character \cr
#'   Studyid number
#' @param not_Liver_get_liver_om_lb_mi_tox_score_list Mandatory, character \cr
#'   path of database
#' @return score
#'
#' @examples
#' \dontrun{
#' get_liver_lb_score(studyid='1234123', database_path = dbtoken)
#' }
#' @export
#'



get_harmonized_column <- function(data_frame = NULL, Liver_get_liver_om_lb_mi_tox_score_list,
                                    not_Liver_get_liver_om_lb_mi_tox_score_list){

  `%ni%` <- Negate('%in%')
  Impute <- T
  ErrorMethod <- 'Prune' # Choose: "Flip" or "Prune" or "None"
  Undersample <- T
  reps <- 0
  #rfDataRDS <- 'rfData_1000000_0.05_1_Prune_Round_8.rds'
  threshold <- 0.05
  holdback <- 1
  testReps <- 5
  testHoldBack <- 0.2
  Round <- T
  generateBarPlot <- T
  nTopImportance <- 20
  indeterminateUpper <- .75
  indeterminateLower <- .25
  Type = 1
  hyperparameter_tuning <- F
  removeEndpoints <- c('Infiltrate', 'UNREMARKABLE', 'THIKENING', 'POSITIVE')

  ###-------------------------
  # get lb score for Liver
  Liver_master_LB_list <- Liver_get_liver_om_lb_mi_tox_score_list[['master_lb_score_six']]
  Liver_master_LB_list$indst_TO <- "Liver"

  # get LivertoBW score for Liver
  Liver_master_liverToBW <- Liver_get_liver_om_lb_mi_tox_score_list[['master_liverToBW']]
  Liver_master_liverToBW $indst_TO   <- "Liver"

  # For mi score for Liver
  Liver_master_MI_list <- Liver_get_liver_om_lb_mi_tox_score_list[['master_mi_df']]
  Liver_master_MI_list$indst_TO <- "Liver"

  # Reorder the columns to make `indst_TO` the first column
  Liver_master_MI_list <- Liver_master_MI_list[, c("indst_TO", setdiff(names(Liver_master_MI_list), "indst_TO"))]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # get lb score for not_Liver
  not_Liver_master_LB_list <- not_Liver_get_liver_om_lb_mi_tox_score_list[['master_lb_score_six']]
  not_Liver_master_LB_list$indst_TO  <- "not_Liver"

  # get LivertoBW score for not_Liver
  not_Liver_master_liverToBW <- not_Liver_get_liver_om_lb_mi_tox_score_list[['master_liverToBW']]
  not_Liver_master_liverToBW$indst_TO  <- "not_Liver"

  # For mi score for not_Liver
  not_Liver_master_MI_list <- not_Liver_get_liver_om_lb_mi_tox_score_list[['master_mi_df']]
  not_Liver_master_MI_list$indst_TO <- "not_Liver"

  # Reorder the columns to make `indst_TO` the first column
  not_Liver_master_MI_list <- not_Liver_master_MI_list[, c("indst_TO", setdiff(names(not_Liver_master_MI_list), "indst_TO"))]


  #--------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  LB <- rbind(Liver_master_LB_list, not_Liver_master_LB_list)

  # Move the last column to the first position
  LB <- LB[, c(ncol(LB), 1:(ncol(LB)-1))]


  OM <- rbind(Liver_master_liverToBW, not_Liver_master_liverToBW)

  # Move the last column to the first position
  OM <- OM[, c(ncol(OM), 1:(ncol(OM)-1))]

  ###------------------------
  MIl <- Liver_master_MI_list
  # Replace spaces and commas in column names with dots
  colnames(MIl) <- gsub(' ', '.', colnames(MIl))
  colnames(MIl) <- gsub(',', '.', colnames(MIl))
  colnames(MIl) <- gsub('/', '.', colnames(MIl))


  MIn <- not_Liver_master_MI_list
  # Replace spaces and commas in column names with dots
  colnames(MIn) <- gsub(' ', '.', colnames(MIn))
  colnames(MIn) <- gsub(',', '.', colnames(MIn))
  colnames(MIn) <- gsub('/', '.', colnames(MIn))



  MIinter <- intersect(colnames(MIl), colnames(MIn))
  MI <- rbind(MIl[, MIinter], MIn[, MIinter])

  MIextraL <- setdiff(colnames(MIl), colnames(MIn))
  MIextraN <- setdiff(colnames(MIn), colnames(MIl))
  for (j in MIextraL) {
    MI[, j] <- NA
    MI[seq(nrow(MIl)), j] <- MIl[, j]
  }
  for (j in MIextraN) {
    MI[, j] <- NA
    MI[seq((nrow(MIl) + 1), nrow(MI)), j] <- MIn[, j]
  }

  MI[is.na(MI)] <- 0

  #Identify Columns with Periods
  findings2replaceIndex <- grep('.', colnames(MI), fixed = T)

  # Store Column Names with Periods
  f2replace <- colnames(MI)[findings2replaceIndex]

  #Identify Unique Column Names without Periods
  fn2replace <- unique(toupper(colnames(MI)[-findings2replaceIndex]))

  #Remove Specific Columns from Processing
  removeIndex <- which(fn2replace %in% c('INDST_TO',
                                         'STUDYID',
                                         'UNREMARKABLE',
                                         'THIKENING',
                                         'POSITIVE'))
  fn2replace <- fn2replace[-removeIndex]

  #Harmonize Synonym Columns
  for (finding in fn2replace) {
    synonyms <- grep(finding, f2replace, ignore.case = T, value = T)
    for (synonym in synonyms) {
      index <- which(MI[[synonym]] > 0)
      for (i in index) {
        if (MI[[synonym]][i] > MI[[finding]][i]) {
          MI[[finding]][i] <- MI[[synonym]][i]
        }
      }
    }
  }

  #Remove Synonym Columns
  MI <- MI[,-findings2replaceIndex]

  #return(list(LB,OM,MI))

  ###------------------------
  commonStudies <- Reduce(intersect, list(LB$STUDYID, OM$STUDYID, MI$STUDYID))
  extraDomains <- c('OM', 'MI')
  count <- 0
  for (study in commonStudies) {
    count <- count + 1
    newRow <- LB[which(LB$STUDYID == study),]
    for (domain in extraDomains) {
      domainData <- get(domain)
      studyIndex <- which(domainData$STUDYID == study)
      for (j in colnames(domainData)[3:ncol(domainData)]) {
        newRow[[j]] <- domainData[studyIndex, j]
      }
    }
    if (count == 1) {
      Data <- newRow
    } else {
      Data <- rbind(Data, newRow)
    }
  }


  removeIndex <- which(colnames(Data) %in% removeEndpoints)
  Data <- Data[, -removeIndex]

  if (Round == T) {
    zscoreIndex <- c(grep('avg_', colnames(Data)), grep('liver', colnames(Data)))
    for (i in zscoreIndex) {
      Data[, i] <- floor(Data[, i])
      maxIndex <- which(Data[, i] > 5)
      Data[maxIndex, i] <- 5
    }
    histoIndex <- which(substr(colnames(Data), 1, 1) %in% toupper(letters))
    histoIndex <- histoIndex[-1]
    for (i in histoIndex) {
      Data[, i] <- ceiling(Data[, i])
    }
  }

  columnSums <- sort(colSums(Data[,3:ncol(Data)], na.rm = T), decreasing = T)
  Data[,3:ncol(Data)] <- Data[, names(columnSums)]
  colnames(Data)[3:ncol(Data)] <- names(columnSums)



   #write.csv(Data, 'mergedData.csv', row.names = F)




print(rf)

  return(rf)
}


