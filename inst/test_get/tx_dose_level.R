rm(list = ls())
devtools::load_all()

path_db = 'C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db'

studyids <- c("876", "5003635")

# tx_dose_level_count <- data.frame(STUDYID= NA,
#                                   dose_level=NA)

tx_dose_level_count <- list()

for (studyid in studyids) {
  print(studyid)
  studyid <- studyid

  # Define a function to query the database by domain
  fetch_domain_data <- function(db_connection, domain_name, studyid) {
    domain_name <- toupper(domain_name)
    query_statement <- paste0('SELECT * FROM ', domain_name, " WHERE STUDYID = :x")
    query_result <- DBI::dbGetQuery(db_connection, statement = query_statement, params = list(x = studyid))
    query_result
  }

  # Establish a connection to the SQLite database
  db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path_db)

  #Pull relevant domain data for each domain
  tx <- fetch_domain_data(db_connection, 'tx', studyid)

  # Close the database connection
  DBI::dbDisconnect(db_connection)


  #----get-the-vehicle-for-cleaned_compiledata-

  # get the dose level from the tx table
  Dose_Level_df <- data.frame(STUDYID = character(),
                              Dose_Level = character(),
                              Dose_Units = character(),
                              SETCD = character())

  unique_setcd_in_tx <- unique(tx$SETCD)

  for (trtm_setcd in unique_setcd_in_tx) {

    # Filter `tx` table for the current SETCD
    tx_trtm_setcd <- tx [tx$SETCD == trtm_setcd, ]

    # Extract dose level and dose units
    dose_level <- tx_trtm_setcd[tx_trtm_setcd$TXPARMCD == "TRTDOS", "TXVAL"]
    dose_units <- tx_trtm_setcd[tx_trtm_setcd$TXPARMCD == "TRTDOSU", "TXVAL"]

    # Get the unique treatment SETCD for the current group
    treatment_setcd <- unique(tx_trtm_setcd$SETCD)

    # Create a data frame for the current treatment group
    dose_level_df <- data.frame(STUDYID = unique(tx$STUDYID),
                                Dose_Level = dose_level,
                                Dose_Units =  dose_units ,
                                SETCD = treatment_setcd) #,
    #stringsAsFactors = FALSE)

    # Append the current data to the main data frame
    Dose_Level_df <- rbind(Dose_Level_df, dose_level_df)

  }


  Dose_Level_df <- Dose_Level_df[!duplicated(Dose_Level_df$Dose_Level),]

  result_df <- Dose_Level_df %>%
    dplyr::group_by(STUDYID) %>%
    dplyr::summarize(
      dose_level_count = dplyr::n_distinct(Dose_Level),
      setcd_count = dplyr::n_distinct(SETCD)
    ) %>%
    dplyr::ungroup()

  tx_dose_level_count <- dplyr::bind_rows(tx_dose_level_count, result_df )

}

