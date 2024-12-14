
get_repeat_dose_parallel_studyids <- function (databasetype = NULL ) {


# #Database Load
# dbtoken <- sendigR::initEnvironment(dbType = 'sqlite',
#                                     dbPath = "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db",
#                                     dbCreate = FALSE)
#
# parallel_StudyID <- sendigR::getStudiesSDESIGN(dbtoken, studyDesignFilter = "PARALLEL")
#
#
# # Filtering the Repeat dose Studies
# repeat_dose_STUDYIDs <- sendigR::genericQuery(dbtoken, queryString = "SELECT DISTINCT STUDYID
#                              FROM ts
#                              WHERE TSPARMCD = 'SSTYP'
#                              AND TSVAL IN ('REPEAT DOSE TOXICITY', 'REPEAT-DOSE TOXICITY', 'Repeat-Dose Toxicity', 'Repeat Dose Toxicity')",
#                                               queryParams = NULL)
#
# # COMMON STUDYIDs from PARALLEL STUDYIDs and repeat_dose_STUDYIDs.....
# parallel_repeat_dose_intersect <- intersect(parallel_StudyID$STUDYID,repeat_dose_STUDYIDs$STUDYID)
#
# # converting "parallel_repeat_dose_intersect" to a data frame
# parallel_repeat_dose_intersec_df <- data.frame(STUDYID = parallel_repeat_dose_intersect)
#
# # convert to a vector( selected_studies should be always vector)
# #selected_studies <- as.vector(parallel_repeat_dose_intersec_df$STUDYID)
#
# # get the studies for the rat only species
# rat_STUDYID_ts_species <- sendigR::genericQuery(dbtoken, queryString = "SELECT STUDYID, TSPARMCD, TSVAL
#                              FROM ts
#                              WHERE TSPARMCD = 'SPECIES' AND UPPER(TSVAL) LIKE '%RAT%'", queryParams = NULL)
#
# selected_studies <- as.vector(rat_STUDYID_ts_species$STUDYID)
# #selected_studies <- c("2170016", "1021-9743")
# #selected_studies <- c("2170016")
# #selected_studies  <- c("8514252")
#
# path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db'
# start_time <- Sys.time()
# allscore <- get_liver_om_lb_mi_tox_score_list(selected_studies,
#                                               path_db,
#                                               fake_study = FALSE,
#                                               output_individual_scores = TRUE)
# end_time <- Sys.time()
# time_taken <- end_time - start_time
# print(time_taken)
# #####
#
}
