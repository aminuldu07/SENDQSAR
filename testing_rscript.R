rm(list = ls())
#setwd("C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/send-summarizer")
devtools::load_all(".")

# compile_data <- get_compile_data(studyid='876', path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db',
#                                  fake_study=FALSE)
#
# fake_compile_data <- get_compile_data(studyid='10663', path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db',
#                                       fake_study = TRUE)
#Database Load
dbtoken <- sendigR::initEnvironment(dbType = 'sqlite',
                                    dbPath = "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db",
                                    dbCreate = FALSE)
selected_studies <- c("2170016")
path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db'
all_score_testing <- get_liver_om_lb_mi_tox_score_list(selected_studies,
                                                       path_db,
                                                       fake_study = FALSE,
                                                       SCORE_IN_LIST_FORMAT = FALSE)

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
rm(list = ls())
#setwd("C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/send-summarizer")
devtools::load_all(".")
selected_studies <- c("2170016")
path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db'
allscore <- get_liver_om_lb_mi_tox_score_list(selected_studies,
                                               path_db,
                                               fake_study = FALSE,
                                               #master_compiledata = NULL,
                                               #bwzscore_BW = NULL,
                                               output_individual_scores = FALSE)

#####
# Select the PARALLEL STUDY from the DATABASE
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
#####
# # GET the studyidli
# #selected_studies <- c("20098018")
# path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db'
# path <- path_db
# con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
#
#   # function for domain
#   con_db <- function(domain){
#     domain <- toupper(domain)
#     stat <- paste0('SELECT * FROM ', domain) #, " WHERE STUDYID = (:x)")
#     domain <- DBI::dbGetQuery(con,
#                               statement = stat,
#                               #params=list(x=studyid
#                               params= NULL)
#     domain
#   }
#   #Pull "ts" domain data for each domain-for-error-log
#   mi <- con_db('mi')
#
# unique_studyid <- data.frame(STUDYID = unique(bw$STUDYID))#, drop = FALSE)
#
#
# path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db'
#selected_studies <- as.vector(rat_STUDYID_ts_species$STUDYID)
#####

