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



#######################for multiple folders#######################
###########################BW_testing_with_fake_liver_data---------

###########################BW_testing_with_fake_not_liver_data------------------

###################################################

#"C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_liver_/FAKE10663"
