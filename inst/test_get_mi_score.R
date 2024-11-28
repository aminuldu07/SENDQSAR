######single_xpt_folder_testing--------------------------------
rm(list = ls())
devtools::load_all(".")

fake_T_xpt_F_mi_score <- get_mi_score(studyid = '28738',
                                  path_db = 'C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/2023-2024_projects/FAKE_DATABASES/fake_merged_liver_not_liver.db',
                                  fake_study = TRUE,
                                  use_xpt_file = FALSE,
                                  master_compiledata = NULL,
                                  return_individual_scores = FALSE,
                                  return_zscore_by_USUBJID = FALSE)


fake_T_xpt_T_mi_score = get_mi_score(studyid=NULL,
                                         path_db="C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/2023-2024_projects/FAKE_DATABASES/single_fake_xpt_folder/FAKE28738",
                                         fake_study=TRUE,
                                         use_xpt_file=TRUE,
                                         master_compiledata=NULL,
                                         return_individual_scores=FALSE,
                                         return_zscore_by_USUBJID=FALSE)




real_sqlite_mi_score <- get_mi_score(studyid="5003635",
                                     path_db="C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/TestDB.db",
                                     fake_study=FALSE,
                                     use_xpt_file=FALSE,
                                     master_compiledata=NULL,
                                     return_individual_scores=FALSE,
                                     return_zscore_by_USUBJID=FALSE)




#rm(list = ls())
# devtools::load_all(".")
reat_XPT_mi_score <- get_mi_score(studyid=NULL,
                                  #path_db="C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/2023-2024_projects/FAKE_DATABASES/real_xpt_dir/IND051292_1017-3581",
                                  path_db="C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/2023-2024_projects/FAKE_DATABASES/real_xpt_dir/IND051292_1017-3581",
                                  fake_study=FALSE,
                                  use_xpt_file=TRUE,
                                  master_compiledata=NULL,
                                  return_individual_scores=FALSE,
                                  return_zscore_by_USUBJID=FALSE)





