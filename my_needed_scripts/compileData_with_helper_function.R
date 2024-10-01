get_compile_data <- function(studyid = NULL,
                             path_db,
                             fake_study = FALSE,
                             use_xpt_file = FALSE) {

  # Helper function to fetch data from SQLite database
  fetch_domain_data <- function(db_connection, domain_name, studyid) {
    domain_name <- toupper(domain_name)
    query_statement <- paste0('SELECT * FROM ', domain_name, " WHERE STUDYID = :x")
    query_result <- DBI::dbGetQuery(db_connection, statement = query_statement, params = list(x = studyid))
    query_result
  }

  # Helper function to read data from .xpt files
  read_xpt_data <- function(path, domain_name) {
    domain_data <- haven::read_xpt(fs::path(path, paste0(domain_name, '.xpt')))
    data.table::setDT(domain_data)
    return(domain_data)
  }

  studyid <- as.character(studyid)
  path <- path_db

  if (fake_study & !use_xpt_file) {
    # Establish a connection to the SQLite database
    db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)

    # Fetch data for required domains
    dm <- fetch_domain_data(db_connection, 'dm', studyid)
    ts <- fetch_domain_data(db_connection, 'ts', studyid)

    # Close the database connection
    DBI::dbDisconnect(db_connection)

  } else if (fake_study & use_xpt_file) {
    # Read data from .xpt files
    dm <- read_xpt_data(path, 'dm')
    ts <- read_xpt_data(path, 'ts')

  } else if (!fake_study & !use_xpt_file) {
    # Establish a connection to the SQLite database
    db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)

    # Fetch data for required domains
    bw <- fetch_domain_data(db_connection, 'bw', studyid)
    dm <- fetch_domain_data(db_connection, 'dm', studyid)
    ds <- fetch_domain_data(db_connection, 'ds', studyid)
    ts <- fetch_domain_data(db_connection, 'ts', studyid)
    tx <- fetch_domain_data(db_connection, 'tx', studyid)
    pp <- fetch_domain_data(db_connection, 'pp', studyid)
    pooldef <- fetch_domain_data(db_connection, 'pooldef', studyid)

    # Close the database connection
    DBI::dbDisconnect(db_connection)

  } else if (!fake_study & use_xpt_file) {
    # Read data from .xpt files
    bw <- read_xpt_data(path, 'bw')
    dm <- read_xpt_data(path, 'dm')
    ds <- read_xpt_data(path, 'ds')
    ts <- read_xpt_data(path, 'ts')
    tx <- read_xpt_data(path, 'tx')
    pp <- read_xpt_data(path, 'pp')
    pooldef <- read_xpt_data(path, 'pooldef')
  }

  # Process 'dm' data if it exists
  if (exists("dm")) {
    species <- ts$TSVAL[which(ts$TSPARMCD == 'SPECIES')]

    # Select specific columns from dm
    dm <- dm[, c('STUDYID', 'USUBJID', 'SPECIES', 'SEX', 'ARMCD', 'ARM', 'SETCD')]

    # Modify and filter 'dm' data
    dm <- dm %>%
      dplyr::mutate(Species = SPECIES) %>%   # Add or update the Species column
      dplyr::select(-SPECIES, -ARMCD) %>%    # Remove SPECIES and ARMCD columns
      dplyr::rename(ARMCD = ARM) %>%         # Rename ARM to ARMCD (if needed)
      dplyr::select("STUDYID", "USUBJID", "Species", "SEX", "ARMCD", "SETCD") %>%
      dplyr::mutate(ARMCD = dplyr::if_else(ARMCD == 'Control', 'vehicle', ARMCD)) %>%
      dplyr::filter(ARMCD %in% c("vehicle", "HD"))

    # Convert to data frame before returning
    data.table::setDF(dm)
    return(dm)
  }
}

# Usage Example
result <- get_compile_data(studyid = '12345', path_db = 'path/to/database', fake_study = TRUE, use_xpt_file = FALSE)
