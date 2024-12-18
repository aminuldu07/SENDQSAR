
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



get_col_harmonized_scores_df <- function(liver_score_data_frame,
                                         Round = FALSE){
#write.csv(Data, 'mergedData.csv', row.names = F)

  ###-----------column harmonization of "liver_scores"-------------
  liver_scores <- liver_score_data_frame
  # Replace spaces and commas in column names with dots
  colnames(liver_scores) <- gsub(' ', '.', colnames(liver_scores))
  colnames(liver_scores) <- gsub(',', '.', colnames(liver_scores))
  colnames(liver_scores) <- gsub('/', '.', colnames(liver_scores))

  liver_scores[is.na(liver_scores)] <- 0

  #Identify Columns with Periods
  findings2replaceIndex <- grep('.', colnames(liver_scores), fixed = T)

  #Identify Unique Column Names without Periods
  fn2replace <- unique(toupper(colnames(liver_scores)[-findings2replaceIndex]))

  # Store Column Names with Periods
  f2replace <- colnames(liver_scores)[findings2replaceIndex]

  #Remove Specific Columns from Processing
  removeIndex <- which(fn2replace %in% c('STUDYID',
                                         'UNREMARKABLE',
                                         'THIKENING',
                                         'POSITIVE'))

  fn2replace <- fn2replace[-removeIndex]

  #Harmonize Synonym Columns
  for (finding in fn2replace) {
    synonyms <- grep(finding, f2replace, ignore.case = T, value = T)
    for (synonym in synonyms) {
      index <- which(liver_scores[[synonym]] > 0)
      for (i in index) {
        if (liver_scores[[synonym]][i] > liver_scores[[finding]][i]) {
          liver_scores[[finding]][i] <- liver_scores[[synonym]][i]
        }
      }
    }
  }

  #Remove Synonym Columns
  liver_scores <- liver_scores[,-findings2replaceIndex]

 # rename the "liver_scores"
  Data <- liver_scores

  removeEndpoints <- c('Infiltrate', 'UNREMARKABLE', 'THIKENING', 'POSITIVE')


  #Data <- column_harmonized_liver_scores

  removeIndex <- which(colnames(Data) %in% removeEndpoints)

  Data <- Data[, -removeIndex]

  # Check if the "Round" condition is TRUE
  # zscoreIndex is basically rounding the livertobw and LB score columns
  # histoindex is basically rounding the MI score columns----------------
  if (Round == T) {
    # Identify columns where the name contains "avg_" or "liver"
    zscoreIndex <- c(grep('avg_', colnames(Data)), grep('liver', colnames(Data)))

    # Loop through each column identified in zscoreIndex
    for (i in zscoreIndex) {

      # Floor the values in the column (round down to the nearest integer)
      Data[, i] <- floor(Data[, i])

      # Find indices of values greater than 5 in the column
      maxIndex <- which(Data[, i] > 5)

      # Cap values at 5 for these indices
      Data[maxIndex, i] <- 5
    }
    # Identify columns where the first character of the name is an uppercase letter
    histoIndex <- which(substr(colnames(Data), 1, 1) %in% toupper(letters))

    # Exclude the first column from the selection
    histoIndex <- histoIndex[-1]

    # Loop through each column identified in histoIndex
    for (i in histoIndex) {
      # Ceiling the values in the column (round up to the nearest integer)
      Data[, i] <- ceiling(Data[, i])
    }
  }

  # Calculate the sum of values for each column in Data, starting from the 2nd column onwards
  # Exclude missing values (na.rm = TRUE) in the calculation
  columnSums <- sort(colSums(Data[,2:ncol(Data)], na.rm = T), decreasing = T) # This produced named attribute

  # Reorder the columns in Data (from the 2nd column onwards) based on the sorted column sums
  # This aligns the columns with higher sums (more "important" columns) to the leftmost positions
  Data[,2:ncol(Data)] <- Data[, names(columnSums)]

  # Update the column names in Data (from the 2nd column onwards) to match the reordered columns
  # This ensures that the column names are consistent with the new ordering
  colnames(Data)[2:ncol(Data)] <- names(columnSums)




  return(as.data.frame(Data))
}


