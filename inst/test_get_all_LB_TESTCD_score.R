rm(list = ls())
devtools::load_all(".")

# Call the function for SEND SQLite database
db_path = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db"
sqlite_real_all_TESTCD_score <- get_all_lb_TESTCD_zscore (studyid = "5003635",
                                                                     path_db = db_path,
                                                                     fake_study= FALSE,
                                                                     use_xpt_file = FALSE,
                                                                     master_compiledata = NULL,
                                                                     return_individual_scores = TRUE)
