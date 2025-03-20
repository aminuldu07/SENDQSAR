######single_xpt_folder_testing--------------------------------
rm(list = ls())
devtools::load_all(".")

fake_T_xpt_F_mi_score <- get_mi_score(studyid = '28738',
                                  path_db = 'C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_merged_liver_not_liver.db',
                                  fake_study = TRUE,
                                  use_xpt_file = FALSE,
                                  master_compiledata = NULL,
                                  return_individual_scores = FALSE,
                                  return_zscore_by_USUBJID = FALSE)


fake_T_xpt_T_mi_score = get_mi_score(studyid=NULL,
                                         path_db="C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/single_fake_xpt_folder/FAKE28738",
                                         fake_study=TRUE,
                                         use_xpt_file=TRUE,
                                         master_compiledata=NULL,
                                         return_individual_scores=FALSE,
                                         return_zscore_by_USUBJID=FALSE)




real_sqlite_mi_score <- get_mi_score(studyid="20098018",
                                     path_db="C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db",
                                     fake_study=FALSE,
                                     use_xpt_file=FALSE,
                                     master_compiledata=NULL,
                                     return_individual_scores=FALSE,
                                     return_zscore_by_USUBJID=FALSE)




#rm(list = ls())
# devtools::load_all(".")
reat_XPT_mi_score <- get_mi_score(studyid=NULL,
                                  #path_db="C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/2023-2024_projects/FAKE_DATABASES/real_xpt_dir/IND051292_1017-3581",
                                  path_db="C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/real_xpt_dir/IND051292_1017-3581",
                                  fake_study=FALSE,
                                  use_xpt_file=TRUE,
                                  master_compiledata=NULL,
                                  return_individual_scores=FALSE,
                                  return_zscore_by_USUBJID=FALSE)



# #####----mi-score--calculation-------------------------
#
# rm(list = ls())
# devtools::load_all(".")
# mi_R_SQL_mi_zscore <- get_mi_score(studyid='A2018086-T002-01',
#                                    path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db',
#                                    fake_study = FALSE,
#                                    use_xpt_file = FALSE,
#                                    master_compiledata = NULL,
#                                    return_individual_scores = TRUE)
#
#
# mi_R_XPT_mi_zscore <- get_mi_score (studyid= NULL,
#                                     path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/real_xpt_dir/IND051292_1017-3581',
#                                     fake_study = FALSE,
#                                     use_xpt_file = TRUE,
#                                     master_compiledata = NULL,
#                                     return_individual_scores = TRUE)
#
#
# mi_fake_SQL_mi_zscore <- get_mi_score (studyid = '10663',
#                                        path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db',
#                                        fake_study = TRUE,
#                                        use_xpt_file = FALSE,
#                                        master_compiledata = NULL,
#                                        return_individual_scores = TRUE)
#
#
# mi_fake_XPT_mi_zscore <- get_mi_score (studyid = NULL,
#                                        path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/all_fakedata_liver_/FAKE10663',
#                                        fake_study = TRUE,
#                                        use_xpt_file = TRUE,
#                                        master_compiledata = NULL,
#                                        return_individual_scores = TRUE)
#

