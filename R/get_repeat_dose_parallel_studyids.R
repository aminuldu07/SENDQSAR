#' @title Get Repeat Dose Parallel Study IDs
#'
#'@description
#' This function retrieves study IDs from a database that correspond to parallel-design studies involving repeat-dose toxicity.
#' It optionally filters the studies for rat species.
#'
#' @param path_db A character string representing the file path to the SQLite database. This is a required parameter.
#' @param rat_studies A logical flag indicating whether to filter the studies for rats only. Defaults to `FALSE`.
#'
#' @return A Data frame having the STUDYID that meet the specified criteria. This includes:
#'   \itemize{
#'     \item STUDYIDs that match both the parallel design and repeat-dose toxicity criteria.
#'     \item Optionally, STUDYIDs that match rat species if `rat_studies = TRUE`.
#'   }
#'
#' @examples
#' \dontrun{
#'   # Example without filtering for rat studies
#'   parallel_repeat_dose_df <- get_repeat_dose_parallel_studyids(path_db = "path/to/database.sqlite")
#'
#'   # Example with filtering for rat studies
#'   rat_parallel_repeat_dose_df <- get_repeat_dose_parallel_studyids(path_db = "path/to/database.sqlite", rat_studies = TRUE)
#' }
#'
#' @export
#'
get_repeat_dose_parallel_studyids <- function (path_db,
                                               rat_studies = FALSE) {

   # Check if the database file exists
  if (!file.exists(path_db)) {
    stop("Database file not found at the specified path!")
  }

  # # Create a connection to the database
  # dbtoken <- dbConnect(SQLite(), dbname = dbPath)
  #
  # # Verify connection
  # if (dbIsValid(dbtoken)) {
  #   message("Database connection established successfully.")
  # } else {
  #   stop("Failed to establish a database connection.")
  # }
#Database Load
dbtoken <- sendigR::initEnvironment(dbType = 'sqlite',
                                    dbPath = path_db,
                                    dbCreate = FALSE)

parallel_StudyID <- sendigR::getStudiesSDESIGN(dbtoken, studyDesignFilter = "PARALLEL")


# Filtering the Repeat dose Studies
repeat_dose_STUDYIDs <- sendigR::genericQuery(dbtoken, queryString = "SELECT DISTINCT STUDYID
                             FROM ts
                             WHERE TSPARMCD = 'SSTYP'
                             AND TSVAL IN ('REPEAT DOSE TOXICITY', 'REPEAT-DOSE TOXICITY', 'Repeat-Dose Toxicity', 'Repeat Dose Toxicity')",
                                              queryParams = NULL)

# COMMON STUDYIDs from PARALLEL STUDYIDs and repeat_dose_STUDYIDs.....
parallel_repeat_dose_intersect <- intersect(parallel_StudyID$STUDYID,repeat_dose_STUDYIDs$STUDYID)

# converting "parallel_repeat_dose_intersect" to a data frame
parallel_repeat_dose_intersec_df <- data.frame(STUDYID = parallel_repeat_dose_intersect)

# # convert to a vector( selected_studies should be always vector)
# parallel_repeat_dose_studyid_or_studyids <- as.vector(parallel_repeat_dose_intersec_df$STUDYID)
#
# studyid_or_studyids <- parallel_repeat_dose_studyid_or_studyids
parallel_repeat_dose_df <- parallel_repeat_dose_intersec_df

# Filter for the rat studies
if (rat_studies){
  # get the studies for the rat only species
  rat_STUDYID_ts_species <- sendigR::genericQuery(dbtoken, queryString = "SELECT STUDYID, TSPARMCD, TSVAL
                             FROM ts
                             WHERE TSPARMCD = 'SPECIES' AND UPPER(TSVAL) LIKE '%RAT%'", queryParams = NULL)
  # Filter the "rat_STUDYID_ts_species" for the PARALLEL STUDYIDs and repeat_dose_STUDYIDs
  rat_parallel_repeat_dose_df <- rat_STUDYID_ts_species[rat_STUDYID_ts_species$STUDYID %in% parallel_repeat_dose_intersec_df$STUDYID, ]
  # select the "STUDYID" column only
  rat_parallel_repeat_dose_df  <- rat_parallel_repeat_dose_df[, "STUDYID", drop = FALSE]

  # rat_parallel_repeat_dose_studyid_or_studyids <- as.vector(rat_parallel_repeat_dose_df$STUDYID)
  #
  # studyid_or_studyids <- rat_parallel_repeat_dose_studyid_or_studyids
}

if(rat_studies) {

  return(rat_parallel_repeat_dose_df)

} else {

  return(parallel_repeat_dose_df)

}

}
