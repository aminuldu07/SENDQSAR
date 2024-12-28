
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

# convert to a vector( selected_studies should be always vector)
parallel_repeat_dose_studyid_or_studyids <- as.vector(parallel_repeat_dose_intersec_df$STUDYID)

studyid_or_studyids <- parallel_repeat_dose_studyid_or_studyids

# Filter for the rat studies
if (rat_studies){
  # get the studies for the rat only species
  rat_STUDYID_ts_species <- sendigR::genericQuery(dbtoken, queryString = "SELECT STUDYID, TSPARMCD, TSVAL
                             FROM ts
                             WHERE TSPARMCD = 'SPECIES' AND UPPER(TSVAL) LIKE '%RAT%'", queryParams = NULL)
  # Filter the "rat_STUDYID_ts_species" for the PARALLEL STUDYIDs and repeat_dose_STUDYIDs
  rat_parallel_repeat_dose_df <- rat_STUDYID_ts_species[rat_STUDYID_ts_species$STUDYID %in% parallel_repeat_dose_intersec_df$STUDYID, ]

  rat_parallel_repeat_dose_studyid_or_studyids <- as.vector(rat_parallel_repeat_dose_df$STUDYID)

  studyid_or_studyids <- rat_parallel_repeat_dose_studyid_or_studyids
}

return(studyid_or_studyids)

}
