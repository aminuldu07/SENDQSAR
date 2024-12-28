
get_histogram_barplot <- function(Data =NULL,
                           generateBarPlot= FALSE,
                           path_db=FALSE,
                           rat_studies=FALSE,
                           studyid_metadata,
                           fake_study = FALSE,
                           use_xpt_file = FALSE,
                           Round = FALSE,
                           output_individual_scores = TRUE,
                           output_zscore_by_USUBJID = FALSE){

browser()
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

        # get the studyids from the dm table
        studyid_or_studyids <- as.vector(unique(dm$STUDYID)) # unique STUDYIDS from DM table

        # Filter the fake data for the "rat_studies"
        if(rat_studies){

          studyid_or_studyids <- studyid_or_studyids
        }

        #--------------------------------------------------------------------
        #-----------we can set logic here for rat studies in "fake data"----
        #--------------------------------------------------------------------

      } else {
        # For the real data in sqlite database
        # filter for the repeat-dose and parallel studyids

        studyid_or_studyids <- get_repeat_dose_parallel_studyids(path_db=path_db,
                                                                 rat_studies = rat_studies)

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

    Data <- column_harmonized_liverscr_df
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
      Value <- c(Value, mean(Data[which(Data$indst_TO == "Liver"), finding], na.rm = T))

      Finding <- c(Finding, finding)
      LIVER <- c(LIVER, 'N')
      Value <- c(Value, mean(Data[which(Data$indst_TO != "Liver"), finding], na.rm = T))

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


return()


}
