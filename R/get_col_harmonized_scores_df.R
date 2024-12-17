
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



get_col_harmonized_scores_df <- function(liver_score_data_frame){
#write.csv(Data, 'mergedData.csv', row.names = F)

  ###-----------column harmonization of "liver_scores"-------------

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
  column_harmonized_liver_scores <- liver_scores

  removeEndpoints <- c('Infiltrate', 'UNREMARKABLE', 'THIKENING', 'POSITIVE')


  Data <- column_harmonized_liver_scores

  removeIndex <- which(colnames(Data) %in% removeEndpoints)

  Data <- Data[, -removeIndex]

print(dim(liver_scores))

  return(column_harmonized_liver_scores)
}


