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


get_histogram_barplot <- function(Data =NULL,
                                  generateBarPlot= FALSE,
                                  path_db=FALSE,
                                  rat_studies=FALSE,
                                  studyid_metadata,
                                  fake_study = FALSE,
                                  use_xpt_file = FALSE,
                                  Round = FALSE,
                                  output_individual_scores = TRUE,
                                  utput_zscore_by_USUBJID = FALSE){


  Data <- Data

  # process the database to get the "studyid_metadata"------------
  if (is.null(studyid_metadata)) {

    repeat_dose_parallel_studyids <- get_repeat_dose_parallel_studyids(path_db,
                                                                       rat_studies = FALSE)
    repeat_dose_parallel_studyids$Target_Organ <- NA
    studyid_metadata <- repeat_dose_parallel_studyids
    #studyid_metadata <- input_scores_df[,1:2]
    #studyid_metadata$Target_Organ <- NA
    #studyid_metadata <- studyid_metadata[,c("STUDYID", "Target_Organ")]
    n_rows <- nrow(studyid_metadata)
    half_n <- ceiling(n_rows / 2)
    studyid_metadata$Target_Organ <- c(rep("Liver", half_n),
                                       rep("not_Liver", n_rows - half_n))

  }

  studyid_metadata <- studyid_metadata

  # Generate data if not provided
  if (is.null(Data)) {

    if(use_xpt_file){

      studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)

    } else {

      if (fake_study) {
        # Helper function to fetch data from SQLite database
        fetch_domain_data <- function(db_connection, domain_name) {
          # Convert domain name to uppercase
          domain_name <- toupper(domain_name)
          # Create SQL query statement
          query_statement <- paste0('SELECT * FROM ', domain_name)
          # Execute query and fetch the data
          query_result <- DBI::dbGetQuery(db_connection, statement = query_statement)
          # Return the result
          query_result
        }
        # Establish a connection to the SQLite database
        db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path_db)

        # Fetch data for required domains
        dm <- fetch_domain_data(db_connection, 'dm')

        # Close the database connection
        DBI::dbDisconnect(db_connection)

        # Filter the fake data for the "rat_studies"
        if(rat_studies){

          #--------------------------------------------------------------------
          #-----------we can set logic here for rat studies in "fake data"----
          #--------------------------------------------------------------------
          studyid_or_studyids <- studyid_or_studyids

        } else {

          # get the studyids from the dm table
        studyid_or_studyids <- as.vector(unique(dm$STUDYID)) # unique STUDYIDS from DM table

        }


      } else {
        # For the real data in SQLite database
        # filter for the repeat-dose and parallel studyids

        # 'parallel_repeatdose_df' is a data frame with one column "STUDYID"
        parallel_repeatdose_df <- get_repeat_dose_parallel_studyids(path_db=path_db,
                                                                    rat_studies = rat_studies)
        # Now, filter the "studyid_or_studyids" for the studyids
        # present in the "studyid_metadata

        studyid_or_studyids <- parallel_repeatdose_df[parallel_repeatdose_df$STUDYID %in% studyid_metadata$STUDYID,  ]

      }
    }


    calculated_liver_scores <- get_liver_om_lb_mi_tox_score_list(studyid_or_studyids = studyid_or_studyids,
                                                                 path_db = path_db,
                                                                 fake_study = fake_study,
                                                                 use_xpt_file = use_xpt_file,
                                                                 output_individual_scores = TRUE,
                                                                 output_zscore_by_USUBJID = FALSE)

    # Harmonize the column
    column_harmonized_liverscr_df <- get_col_harmonized_scores_df(liver_score_data_frame = calculated_liver_scores,
                                                                  Round = Round)

    # Need to add the "Target_Organ" column to the Data

    Data_target_organ <- dplyr::inner_join(studyid_metadata, column_harmonized_liverscr_df, by = "STUDYID")

    Data <- Data_target_organ
    #---------------------------------------------------------------------
  }


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
