rm(list = ls())
devtools::load_all(".")

path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db'

#studyid_or_studyids  <- c("2170016")
#studyid_or_studyids  <- c( "5003635", "2170016", "876") #"5003635",
studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/sqlite_20Liver_20not_liver.csv")
studyid_or_studyids <- studyid_metadata$STUDYID
indiv_score_om_lb_mi <- get_indiv_score_om_lb_mi_domain_df(studyid_or_studyids = studyid_or_studyids,
                                                       path_db = path_db,
                                                       fake_study = FALSE,
                                                       use_xpt_file = FALSE,
                                                       all_lb_TESTCD_score = TRUE,
                                                       output_individual_scores = TRUE,
                                                       output_zscore_by_USUBJID = FALSE
                                                       )


# # For multiple xpt folder with real data
# rm(list = ls())
# devtools::load_all(".")
# path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/real_xpt_dir'
# studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)
#
# R_XPT_om_lb_mi_CD <- get_liver_om_lb_mi_tox_score_list  (studyid_or_studyids = studyid_or_studyids,
#                                                          path_db = path_db,
#                                                          fake_study = FALSE,
#                                                          use_xpt_file = TRUE,
#                                                          #multiple_xpt_folder = TRUE,
#                                                          output_individual_scores = FALSE,
#                                                          output_zscore_by_USUBJID = FALSE)
#
# # For multiple xpt folder with fake data
# rm(list = ls())
# devtools::load_all(".")
# path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/pfda_challenge_data'
# studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)
#
# fake_XPT_om_lb_mi_CD <- get_liver_om_lb_mi_tox_score_list  (studyid_or_studyids = studyid_or_studyids,
#                                                          path_db = path_db,
#                                                          fake_study = TRUE,
#                                                          use_xpt_file = TRUE,
#                                                          #multiple_xpt_folder = TRUE,
#                                                          output_individual_scores = FALSE,
#                                                          output_zscore_by_USUBJID = FALSE)
#
#
#
#
#
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
