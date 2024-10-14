
###########################BW_testing_with_fake_liver_data---------
rm(list = ls())
devtools::load_all(".")

# Set the main directory
main_dir <- "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_liver_/"

# List all subdirectories
subdirs <- list.dirs(main_dir, full.names = TRUE, recursive = FALSE)

# Store all subdirectory paths in a new vector
selected_studies <- subdirs

get_om_lb_mi_scores <- get_liver_om_lb_mi_tox_score_list(selected_studies = selected_studies,
                                                          path_db = main_dir,
                                                          fake_study = TRUE,
                                                          use_xpt_file = TRUE,
                                                          multiple_xpt_folder = TRUE,
                                                          output_individual_scores = FALSE,
                                                          output_zscore_by_USUBJID = FALSE)

###########################BW_testing_with_fake_not_liver_data------------------
rm(list = ls())
devtools::load_all(".")

# Set the main directory
main_dir <- "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_not_liver_/"

# List all subdirectories
subdirs <- list.dirs(main_dir, full.names = TRUE, recursive = FALSE)

# Store all subdirectory paths in a new vector
selected_studies <- subdirs

get_om_lb_mi_scores <- get_liver_om_lb_mi_tox_score_list(selected_studies = selected_studies,
                                                         path_db = main_dir,
                                                         fake_study = TRUE,
                                                         use_xpt_file = TRUE,
                                                         multiple_xpt_folder = TRUE,
                                                         output_individual_scores = FALSE,
                                                         output_zscore_by_USUBJID = FALSE)

###################################################
#"C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_liver_/FAKE10663"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###########################BW_testing_with_database_liver_data---------
rm(list = ls())
devtools::load_all(".")

# Set the main directory
path_db <- 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db'

# Define a function to query the database by domain
path <- path_db

# Define a function to query the database by table (domain)
fetch_domain_data <- function(db_connection, domain_name) {
  domain_name <- toupper(domain_name)  # Ensure domain name is in uppercase
  query_statement <- paste0('SELECT * FROM ', domain_name)  # Query entire table
  query_result <- DBI::dbGetQuery(db_connection, query_statement)
  return(query_result)
}

# Establish a connection to the SQLite database
db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)

# Fetch data for the desired domain (table name)
table_name <- 'dm'  # Replace with any table name you want to query
DM <- fetch_domain_data(db_connection, table_name)

# Close the database connection
DBI::dbDisconnect(db_connection)

# Store all subdirectory paths in a new vector
selected_studies <- as.vector(unique(DM$STUDYID))

get_om_lb_mi_scores <- get_liver_om_lb_mi_tox_score_list(selected_studies = selected_studies,
                                                         path_db = path_db,
                                                         fake_study = TRUE,
                                                         use_xpt_file = FALSE,
                                                         multiple_xpt_folder = FALSE,
                                                         output_individual_scores = TRUE,
                                                         output_zscore_by_USUBJID = FALSE)

###########################BW_testing_with_datanbase_not_liver_data------------------
rm(list = ls())
devtools::load_all(".")

# Set the main directory

# Set the main directory
path_db <- 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/fake_not_liver_update.db'

# Define a function to query the database by domain
path <- path_db

# Define a function to query the database by table (domain)
fetch_domain_data <- function(db_connection, domain_name) {
  domain_name <- toupper(domain_name)  # Ensure domain name is in uppercase
  query_statement <- paste0('SELECT * FROM ', domain_name)  # Query entire table
  query_result <- DBI::dbGetQuery(db_connection, query_statement)
  return(query_result)
}

# Establish a connection to the SQLite database
db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)

# Fetch data for the desired domain (table name)
table_name <- 'dm'  # Replace with any table name you want to query
DM <- fetch_domain_data(db_connection, table_name)

# Close the database connection
DBI::dbDisconnect(db_connection)

# Store all subdirectory paths in a new vector
selected_studies <- as.vector(unique(DM$STUDYID))

nl_get_om_lb_mi_scores <- get_liver_om_lb_mi_tox_score_list(selected_studies = selected_studies,
                                                         path_db = path_db,
                                                         fake_study = TRUE,
                                                         use_xpt_file = FALSE,
                                                         multiple_xpt_folder = FALSE,
                                                         output_individual_scores = FALSE,
                                                         output_zscore_by_USUBJID = FALSE)


###################################################
