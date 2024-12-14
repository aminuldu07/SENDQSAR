rm(list = ls())
devtools::load_all(".")

# #Database Load
# dbtoken <- sendigR::initEnvironment(dbType = 'sqlite',
#                                     dbPath = "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db",
#                                     dbCreate = FALSE)


path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db'

#selected_studies <- c("2170016")



R_SQL_om_lb_mi_CD <- get_liver_om_lb_mi_tox_score_list  (selected_studies = selected_studies,
                                               path_db = path_db,
                                               fake_study = FALSE,
                                               use_xpt_file = FALSE,
                                               multiple_xpt_folder = FALSE,
                                               output_individual_scores = FALSE,
                                               output_zscore_by_USUBJID = FALSE)

# For multiple xpt folder

selected_studies <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)

R_XPT_om_lb_mi_CD <- get_liver_om_lb_mi_tox_score_list  (selected_studies = selected_studies,
                                                         path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/real_xpt_dir/IND051292_1017-3581',
                                                         fake_study = FALSE,
                                                         use_xpt_file = FALSE,
                                                         multiple_xpt_folder = FALSE,
                                                         output_individual_scores = FALSE,
                                                         output_zscore_by_USUBJID = FALSE)


selected_studies <- c("10663")
fake_SQL_om_lb_mi_CD <- get_liver_om_lb_mi_tox_score_list  (selected_studies = selected_studies,
                                                         path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db',
                                                         fake_study = FALSE,
                                                         use_xpt_file = FALSE,
                                                         multiple_xpt_folder = FALSE,
                                                         output_individual_scores = FALSE,
                                                         output_zscore_by_USUBJID = FALSE)

fake_XPT_om_lb_mi_CD <- get_liver_om_lb_mi_tox_score_list  (selected_studies = NULL,
                                                         path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_liver_/FAKE10663',
                                                         fake_study = FALSE,
                                                         use_xpt_file = FALSE,
                                                         multiple_xpt_folder = FALSE,
                                                         output_individual_scores = FALSE,
                                                         output_zscore_by_USUBJID = FALSE)
