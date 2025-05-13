#' @title Generate Histogram or Bar Plot for Liver-Related Scores
#'
#'@description
#' This function generates a bar plot comparing liver-related findings to non-liver-related findings,
#' or returns processed data for further analysis. The function can fetch data from an SQLite database,
#' a provided XPT file, or simulate data if `fake_study` is set to TRUE.
#'
#' @param Data A data frame containing liver-related scores. If NULL, the function will attempt to
#'        generate or fetch the data from a database or file.
#' @param generateBarPlot A logical flag (default = FALSE). If TRUE, generates a bar plot. If FALSE,
#'        returns the processed data.
#' @param path_db A character string representing the path to the SQLite database. Required if
#'        `use_xpt_file` is FALSE or `fake_study` is FALSE.
#' @param rat_studies A logical flag (default = FALSE) to filter for rat studies when fetching data
#'        from the database.
#' @param studyid_metadata A data frame containing metadata associated with study IDs. Required when
#'        `fake_study` is FALSE and real data is fetched.
#' @param fake_study A logical flag (default = FALSE). If TRUE, the function simulates study data
#'        instead of fetching it from a database.
#' @param use_xpt_file A logical flag (default = FALSE). If TRUE, the function will use an XPT file
#'        to fetch data, instead of relying on the database.
#' @param Round A logical flag (default = FALSE). Whether to round the liver scores.
#' @param output_individual_scores A logical flag (default = TRUE). Whether to output individual
#'        scores or aggregated scores.
#' @param output_zscore_by_USUBJID A logical flag (default = FALSE). Whether to output z-scores
#'        by USUBJID (unique subject identifier).
#'
#' @return If `generateBarPlot = TRUE`, a `ggplot2` bar plot object is returned displaying the
#'         average scores for liver-related findings versus non-liver-related findings. If
#'         `generateBarPlot = FALSE`, a data frame (`plotData`) containing the calculated values
#'         for each finding, liver status (`LIVER`), and mean values (`Value`) is returned.
#'
#' @details
#' If no data is provided, the function attempts to fetch data from an SQLite database or simulate
#' data based on the `fake_study` flag. The function also supports the use of XPT files and allows
#' customization of study filtering through the `rat_studies` and `studyid_metadata` parameters.
#' When generating a plot, the function compares liver-related findings to other findings,
#' displaying the average scores for each finding in a bar plot.
#'
#' @examples
#' \dontrun{
#' # Example 1: Generate a bar plot with fake study data
#' #get_histogram_barplot(generateBarPlot = TRUE, fake_study = TRUE)
#'
#' # Example 2: Get processed data without generating a plot
#' #data <- get_histogram_barplot(generateBarPlot = FALSE, fake_study = FALSE, path_db = "path/to/db")
#' }
#'
#' @import ggplot2
#' @import DBI
#' @import RSQLite
#' @export


get_histogram_barplot_modular <- function(ml_formatted_scores_df =NULL,
                                  generateBarPlot= FALSE
                                  ){


  Data <- ml_formatted_scores_df

  #---------------------------------------------------------------------------
  #Check if data is a valid data frame
  if (!is.data.frame(Data)) {
    stop("The input data must be a data frame.")
  }
  #---------------------------------------------------------------------------

  ##---------------
  if (generateBarPlot == T) {

    Finding <- NULL
    LIVER <- NULL
    Value <- NULL
    for (finding in colnames(Data)[3:ncol(Data)]) {

      Finding <- c(Finding, finding)
      LIVER <- c(LIVER, 'Y')
      Value <- c(Value, mean(Data[which(Data$Target_Organ == "Liver"), finding], na.rm = T))

      Finding <- c(Finding, finding)
      LIVER <- c(LIVER, 'N')
      Value <- c(Value, mean(Data[which(Data$Target_Organ != "Liver"), finding], na.rm = T))

    }

    plotData <- as.data.frame(cbind(Finding, LIVER, Value))
    plotData$LIVER <- factor(plotData$LIVER)
    plotData$Finding <- factor(plotData$Finding)
    plotData$Value = as.numeric(plotData$Value)
    # plotData$Value = log(as.numeric(plotData$Value), base = 10)

    ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~check and double check
    p <- ggplot2::ggplot(plotData, ggplot2::aes(x = Finding, y = Value, fill = LIVER)) +
      ggplot2::geom_bar(stat="identity", position = 'dodge') +
      ggplot2::theme(text = ggplot2::element_text(size = 20),
                     axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggplot2::ylab('Average Score')

    print(p)
  }

  if (generateBarPlot) {
    return(p)  # Return the generated plot object
  } else {
    return( plotData)  # Return the processed data
  }



}
