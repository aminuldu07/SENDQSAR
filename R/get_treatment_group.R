


get_treatment_group <- function(studyid = NULL,
                         path_db,
                         fake_study=FALSE,
                         use_xpt_file = FALSE,
                         master_compiledata = NULL,
                         return_individual_scores = FALSE,
                         return_zscore_by_USUBJID = FALSE) {

  studyid <- as.character(studyid)
  path <- path_db

  # Helper function to fetch data from SQLite database
  fetch_domain_data <- function(db_connection, domain_name, studyid) {
    domain_name <- toupper(domain_name)
    query_statement <- paste0('SELECT * FROM ', domain_name, " WHERE STUDYID = :x")
    query_result <- DBI::dbGetQuery(db_connection, statement = query_statement, params = list(x = studyid))
    query_result
  }

  # GET THE REQUIRED DOMAIN DATA
  if (use_xpt_file) {
    # Read data from .xpt files
    mi <- haven::read_xpt(fs::path(path, 'mi.xpt'))

    dm <- haven::read_xpt(fs::path(path,'dm.xpt'))

  } else {
    # Establish a connection to the SQLite database
    db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)

    # Fetch data for required domains
    tx <- fetch_domain_data(db_connection, 'tx', studyid)
    ts <- fetch_domain_data(db_connection, 'ts', studyid)
    ds <- fetch_domain_data(db_connection, 'ds', studyid)
    dm <- fetch_domain_data(db_connection, 'dm', studyid)
    pc <- fetch_domain_data(db_connection, 'pc', studyid)
    pooldef <- fetch_domain_data(db_connection, 'pooldef', studyid)

    # Close the database connection
    DBI::dbDisconnect(db_connection)
  }


  cat("The dimension of 'dm' domain is:", dim(dm), "\n")

  cat("The dimension of 'mi' domain is:", dim(mi), "\n")
}
