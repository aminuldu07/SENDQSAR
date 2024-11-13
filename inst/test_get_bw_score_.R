m(list = ls())

##### Aminul islam prodhan

#setwd("C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/send-summarizer")
devtools::load_all(".")

##### compile_data_----------------------------
# R_SQL_compile_data <- get_compile_data(studyid='5003635',
#                                        path_db='C:\\Users\\MdAminulIsla.Prodhan\\OneDrive - FDA\\Documents\\TestDB.db',
#                                        fake_study = FALSE,
#                                        use_xpt_file = FALSE)

fake_T_xpt_T_compile_data = get_bw_score(studyid=NULL,
                                         path_db="C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/2023-2024_projects/FAKE_DATABASES/single_fake_xpt_folder/FAKE28738",
                                         fake_study=TRUE,
                                         use_xpt_file=TRUE,
                                         master_compiledata=NULL,
                                         return_individual_scores=FALSE,
                                         return_zscore_by_USUBJID=FALSE)




R_XPT_compile_data <- get_compile_data(studyid = NULL,
                                       path_db,
                                       fake_study = FALSE,
                                       use_xpt_file = FALSE,
                                       master_compiledata = NULL,
                                       return_individual_scores = FALSE,
                                       return_zscore_by_USUBJID = FALSE)


fake_SQL_compile_data <- get_compile_data(studyid = '10663',
                                          path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db',
                                          fake_study = TRUE,
                                          use_xpt_file = FALSE)


fake_XPT_compile_data <- get_compile_data(studyid = NULL,
                                          path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_liver_/FAKE10663',
                                          fake_study = TRUE,
                                          use_xpt_file = TRUE)
