######single_xpt_folder_testing--------------------------------
rm(list = ls())
devtools::load_all(".")

fake_SQL_lb_score <- get_lb_score(studyid = '5003635',
                                  path_db = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db",
                                  fake_study = FALSE,
                                  use_xpt_file = FALSE,
                                  master_compiledata = NULL,
                                  return_individual_scores = FALSE,
                                  return_zscore_by_USUBJID = FALSE)


fake_T_xpt_T_lb_score = get_lb_score(studyid=NULL,
                                         path_db="C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/2023-2024_projects/FAKE_DATABASES/single_fake_xpt_folder/FAKE28738",
                                         fake_study=TRUE,
                                         use_xpt_file=TRUE,
                                         master_compiledata=NULL,
                                         return_individual_scores=FALSE,
                                         return_zscore_by_USUBJID=FALSE)




R_XPT_lb_score <- get_lb_score(studyid = NULL,
                                       path_db,
                                       fake_study = FALSE,
                                       use_xpt_file = FALSE,
                                       master_compiledata = NULL,
                                       return_individual_scores = FALSE,
                                       return_zscore_by_USUBJID = FALSE)





fake_XPT_lb_score <- get_lb_score(studyid = NULL,
                                          path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_liver_/FAKE10663',
                                          fake_study = TRUE,
                                          use_xpt_file = TRUE)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_liver_/FAKE10663'
path_db = "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_liver_/FAKE42143"

fake_lb_xpt <- get_lb_score(studyid = NULL,
                            path_db,
                            fake_study = TRUE,
                            use_xpt_file = TRUE,
                            master_compiledata = NULL,
                            return_individual_scores = TRUE,
                            return_zscore_by_USUBJID = FALSE)


########################for multiple folders#######################
###########################BW_testing_with_fake_liver_data---------
rm(list = ls())
devtools::load_all(".")

# Set the main directory
main_dir <- "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_liver_/"

# List all subdirectories
subdirs <- list.dirs(main_dir, full.names = TRUE, recursive = FALSE)

# Initialize an empty list to store results and an empty error data frame
fake_xpt_results <- list()
master_error_df <- data.frame(STUDYID = character(),
                              Block = character(),
                              ErrorMessage = character(),
                              stringsAsFactors = FALSE)

# Loop through each subdirectory and process it
for (subdir in subdirs) {
  print(subdir)

  tryCatch({
    # Call the function with the current subdirectory
    fake_bw_xpt <- get_bw_score(studyid = NULL,
                                path_db = subdir,
                                fake_study = TRUE,
                                use_xpt_file = TRUE,
                                master_compiledata = NULL,
                                return_individual_scores = FALSE,
                                return_zscore_by_USUBJID = TRUE)

    # Store the result in the list
    fake_xpt_results <- append(fake_xpt_results, list(fake_bw_xpt))

  }, error = function(e) {
    # Handling errors
    message("Error in FOUR_Liver_Score: ", e$message)

  })
}


###########################BW_testing_with_fake_not_liver_data------------------
rm(list = ls())
devtools::load_all(".")

# Set the main directory
main_dir <- "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_not_liver_/"

#FAKE32670", FAKE72332", FAKE72027", FAKE77053", FAKE78993"
# List all subdirectories
subdirs <- list.dirs(main_dir, full.names = TRUE, recursive = FALSE)
#subdirs <- "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_not_liver_/FAKE32670"

# Initialize an empty list to store results and an empty error data frame
fake_xpt_not_liver_results <- list()
master_error_df <- data.frame(STUDYID = character(),
                              Block = character(),
                              ErrorMessage = character(),
                              stringsAsFactors = FALSE)

# Loop through each subdirectory and process it
for (subdir in subdirs) {
  print(subdir)

  tryCatch({
    # Call the function with the current subdirectory
    fake_nl_bw_xpt <- get_bw_score(studyid = NULL,
                                path_db = subdir,
                                fake_study = TRUE,
                                use_xpt_file = TRUE,
                                master_compiledata = NULL,
                                return_individual_scores = FALSE,
                                return_zscore_by_USUBJID = TRUE)

    # Store the result in the list
    fake_xpt_not_liver_results <- append(fake_xpt_not_liver_results, list(fake_nl_bw_xpt))

  }, error = function(e) {
    # Handling errors
    message("Error in FOUR_Liver_Score: ", e$message)

  })
}


