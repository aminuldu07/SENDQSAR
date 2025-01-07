
rm(list = ls())
#setwd("C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/send-summarizer")
devtools::load_all(".")

# Call the function for SEND SQLite database
db_path = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db"
dosed_leveled_score  = get_dose_level_score (studyid="5003635", #5003635",
                                             path_db=db_path,
                                             fake_study=FALSE,
                                              use_xpt_file=FALSE,
                                              master_compiledata = NULL)
