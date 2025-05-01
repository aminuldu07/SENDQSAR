rm(list = ls())
devtools::load_all(".")

# #Database Load
# dbtoken <- sendigR::initEnvironment(dbType = 'sqlite',
#                                     dbPath = "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db",
#                                     dbCreate = FALSE)


# Call the function for "REAL SEND SQLite database"
#path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/SEND_PUB_DB/public_SEND.db'
#studyid_or_studyids  <- c("01337003")
#studyid_or_studyids  <- c("5003635", "2170016")
#studyid_or_studyids  <- c("PDS2014", "Study ID")

path_db = 'C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db'

studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/sqlite_20Liver_20not_liver.csv")

studyid_or_studyids <-  studyid_metadata[ ,"STUDYID"]

R_SQL_om_lb_mi_CD <- get_liver_om_lb_mi_tox_score_list(studyid_or_studyids = studyid_or_studyids,
                                                       path_db = path_db,
                                                       fake_study = FALSE,
                                                       use_xpt_file = FALSE,
                                                       output_individual_scores = TRUE,
                                                       output_zscore_by_USUBJID = FALSE)

# Call the function for " REAL SEND XPT data"..# For multiple xpt folder with real data
rm(list = ls())
devtools::load_all(".")
#path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/real_xpt_dir'
# pulic worked - pointCross_S, PDS
path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/SEND_PUB_DB/working'

studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)

R_XPT_om_lb_mi_CD <- get_liver_om_lb_mi_tox_score_list  (studyid_or_studyids = studyid_or_studyids,
                                                         path_db = path_db,
                                                         fake_study = FALSE,
                                                         use_xpt_file = TRUE,
                                                         #multiple_xpt_folder = TRUE,
                                                         output_individual_scores = FALSE,
                                                         output_zscore_by_USUBJID = TRUE)

# For multiple xpt folder with fake data
rm(list = ls())
devtools::load_all(".")
path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/pfda_challenge_data'
studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)

fake_XPT_om_lb_mi_CD <- get_liver_om_lb_mi_tox_score_list  (studyid_or_studyids = studyid_or_studyids,
                                                         path_db = path_db,
                                                         fake_study = TRUE,
                                                         use_xpt_file = TRUE,
                                                         #multiple_xpt_folder = TRUE,
                                                         output_individual_scores = FALSE,
                                                         output_zscore_by_USUBJID = FALSE)



# Call the function for "FAKE/Synthetic SEND SQLite database"
# selected_studies <- c("10663")
# fake_SQL_om_lb_mi_CD <- get_liver_om_lb_mi_tox_score_list  (selected_studies = selected_studies,
#                                                          path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db',
#                                                          fake_study = FALSE,
#                                                          use_xpt_file = FALSE,
#                                                          multiple_xpt_folder = FALSE,
#                                                          output_individual_scores = FALSE,
#                                                          output_zscore_by_USUBJID = FALSE)
#
# fake_XPT_om_lb_mi_CD <- get_liver_om_lb_mi_tox_score_list  (selected_studies = NULL,
#                                                          path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_liver_/FAKE10663',
#                                                          fake_study = FALSE,
#                                                          use_xpt_file = FALSE,
#                                                          multiple_xpt_folder = FALSE,
#                                                          output_individual_scores = FALSE,
#                                                          output_zscore_by_USUBJID = FALSE)
