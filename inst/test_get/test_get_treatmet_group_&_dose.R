rm(list = ls())
#setwd("C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/send-summarizer")
devtools::load_all(".")

# Call the function for SEND SQLite database
db_path = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db"
real_sqlite_treatment_group = get_treatment_group_amin(studyid="5003635", #5003635",
                                                  path_db=db_path,
                                                  fake_study=FALSE,
                                                  use_xpt_file=FALSE)



rm(list = ls())
devtools::load_all(".")
# Call the function for SEND SQLite database
db_path = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db"
real_sqlite_treatment_group = get_treatment_group_myl(studyid="5003635", #5003635",
                                                       path_db=db_path,
                                                       fake_study=FALSE,
                                                       use_xpt_file=FALSE,
                                                       master_compiledata=NULL,
                                                       return_individual_scores=FALSE,
                                                       return_zscore_by_USUBJID=FALSE)


####





















# Call the function
db_path = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/2023-2024_projects/FAKE_DATABASES/fake_merged_liver_not_liver.db"
fake_T_xpt_F_bw_score = get_bw_score(studyid="28738",
                                     path_db=db_path,
                                     fake_study=TRUE,
                                     use_xpt_file=FALSE,
                                     master_compiledata=NULL,
                                     return_individual_scores=FALSE,
                                     return_zscore_by_USUBJID=FALSE)

# Call the function
db_path = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/2023-2024_projects/FAKE_DATABASES/single_fake_xpt_folder/FAKE28738"
fake_T_xpt_T_bw_score = get_bw_score(studyid=NULL,
                                     path_db=db_path,
                                     fake_study=TRUE,
                                     use_xpt_file=TRUE,
                                     master_compiledata=NULL,
                                     return_individual_scores=FALSE,
                                     return_zscore_by_USUBJID=FALSE)


# Call the function for SEND SQLite database
rm(list = ls())
devtools::load_all(".")
db_path = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db"
real_sqlite_treatment_group = get_treatment_group_amin(studyid="876", #5003635","876"
                                                  path_db=db_path,
                                                  fake_study=FALSE,
                                                  use_xpt_file=FALSE,
                                                  master_compiledata=NULL,
                                                  return_individual_scores=FALSE,
                                                  return_zscore_by_USUBJID=FALSE)




get# Call the function for SEND XPT data
db_path = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/2023-2024_projects/FAKE_DATABASES/real_xpt_dir/IND051292_1017-3581"
real_XPT_bw_score = get_bw_score(studyid=NULL,
                                 path_db=db_path,
                                 fake_study=FALSE,
                                 use_xpt_file=TRUE,
                                 master_compiledata=NULL,
                                 return_individual_scores=FALSE,
                                 return_zscore_by_USUBJID=FALSE)
