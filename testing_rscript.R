rm(list = ls())
#setwd("C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/send-summarizer")
devtools::load_all(".")

R_SQL_compile_data <- get_compile_data(studyid='876', path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db', fake_study = FALSE,
                                      use_xpt_file = FALSE)


R_XPT_compile_data <- get_compile_data(studyid= NULL, path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/real_xpt_dir/IND051292_1017-3581', fake_study = FALSE,
                                      use_xpt_file = TRUE)


fake_SQL_compile_data <- get_compile_data(studyid = '10663',
                                      path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db',
                                      fake_study = TRUE,
                                      use_xpt_file = FALSE)


fake_XPT_compile_data <- get_compile_data(studyid = NULL,
                                           path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_liver_/FAKE10663',
                                           fake_study = TRUE,
                                           use_xpt_file = TRUE)



#dbtoken

#########
dbtoken <- sendigR::initEnvironment(dbType = 'sqlite',
                                    dbPath = "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db",
                                    dbCreate = FALSE)
selected_studies <- c("2170016")
path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db'
all_score_testing <- get_liver_om_lb_mi_tox_score_list(selected_studies,
                                                       path_db,
                                                       fake_study = FALSE,
                                                       SCORE_IN_LIST_FORMAT = FALSE)
######
rm(list = ls())
#setwd("C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/send-summarizer")
devtools::load_all(".")
studyid <- c("2170016")
path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db'
#livertobw <- all_score_testing[["master_liverToBW"]]
#error_df <- all_score_testing[["master_error_df"]]
mi_score <- get_mi_score (studyid,
                          path_db,
                          fake_study=FALSE,
                          master_compiledata = NULL,
                          return_individual_scores = FALSE)

bw_score <- get_bw_score(studyid,
                         path_db,
                         fake_study = FALSE,
                         master_compiledata = NULL,
                         return_individual_scores = TRUE)






livertobw_score <- get_liver_livertobw_score(studyid,
                                            path_db,
                                            fake_study = FALSE,
                                            master_compiledata = NULL,
                                            bwzscore_BW = NULL,
                                            return_individual_scores = FALSE)


lb_score <- get_lb_score(studyid,
                         path_db,
                         fake_study= FALSE,
                         master_compiledata = NULL,
                         return_individual_scores = FALSE)

#####

rm(list = ls())
#setwd("C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/send-summarizer")
devtools::load_all(".")

#Database Load
dbtoken <- sendigR::initEnvironment(dbType = 'sqlite',
                                    dbPath = "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db",
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
#selected_studies <- as.vector(parallel_repeat_dose_intersec_df$STUDYID)

# get the studies for the rat only species
rat_STUDYID_ts_species <- sendigR::genericQuery(dbtoken, queryString = "SELECT STUDYID, TSPARMCD, TSVAL
                             FROM ts
                             WHERE TSPARMCD = 'SPECIES' AND UPPER(TSVAL) LIKE '%RAT%'", queryParams = NULL)

selected_studies <- as.vector(rat_STUDYID_ts_species$STUDYID)
#selected_studies <- c("2170016", "1021-9743")
#selected_studies <- c("2170016")
#selected_studies  <- c("8514252")

path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db'
start_time <- Sys.time()
allscore <- get_liver_om_lb_mi_tox_score_list(selected_studies,
                                              path_db,
                                              fake_study = FALSE,
                                              output_individual_scores = TRUE)
end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)
#####

####bw##score testing
######
rm(list = ls())
devtools::load_all(".")
studyid = '10663'
path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db'

fake_bw <- get_bw_score(studyid,
                        path_db,
                        fake_study = TRUE,
                        master_compiledata = NULL,
                        return_individual_scores = FALSE,
                        use_xpt_file = FALSE)

fake_bwliverwRatio <- get_liver_livertobw_score(studyid,
                                       path_db,
                                       fake_study = TRUE,
                                       master_compiledata = NULL,
                                       bwzscore_BW = NULL,
                                       return_individual_scores = TRUE)



fake_lb_score <- get_lb_score (studyid,
                                          path_db,
                                          fake_study= TRUE,
                                          master_compiledata = NULL,
                                          return_individual_scores = TRUE)
fake_mi_score <- get_mi_score (studyid,
                                          path_db,
                                          fake_study=TRUE,
                                          master_compiledata = NULL,
                                          return_individual_scores = TRUE)

### xpt--file input system testing
end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)
#####

####bw##score testing
######
rm(list = ls())
devtools::load_all(".")

path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_liver_/FAKE10663'
#
# fake_xpt_compiltdata <- get_compile_data(studyid = NULL,
#                                                 path_db,
#                                                 fake_study = TRUE,
#                                                 use_xpt_file = TRUE)
#
#
# studyid = '10663'
# path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db'
# fake_compiledata <- get_compile_data(studyid,
#                                                  path_db,
#                                                  fake_study = TRUE,
#                                                  use_xpt_file = FALSE)






fake_bw_xpt <- get_bw_score(studyid = NULL,
                        path_db,
                        fake_study = TRUE,
                        master_compiledata = NULL,
                        return_individual_scores = FALSE,
                        use_xpt_file = TRUE)














