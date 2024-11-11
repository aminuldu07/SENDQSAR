rm(list = ls())
devtools::load_all(".")
path_db <- "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/2023-2024_projects/FAKE_DATABASES/all_fakedata_liver_"

selected_studies <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)


get_multiple_xpt <- get_liver_om_lb_mi_tox_score_list (selected_studies = selected_studies,
                                               path_db = path_db,
                                               fake_study = TRUE,
                                               use_xpt_file = TRUE,
                                               multiple_xpt_folder = TRUE,
                                               output_individual_scores = FALSE,
                                               output_zscore_by_USUBJID = FALSE)
