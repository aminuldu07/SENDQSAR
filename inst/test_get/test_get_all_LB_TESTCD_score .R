rm(list = ls())
devtools::load_all()

path_db = "/shared/OND_PharmTox/SEND_DATASETS/DB/DataCentral/DataCentral.db"
unq_Liver40_unq_not_Liver168 <- read.csv("unq_Liver40_unq_not_Liver168.csv")

rf_param_list  <- get_rf_input_param_list_output_cv_imp(path_db=path_db,
                                                        rat_studies=FALSE,
                                                        studyid_metadata=unq_Liver40_unq_not_Liver168,
                                                        fake_study = FALSE,
                                                        use_xpt_file = FALSE,
                                                        Round = TRUE,
                                                        Impute = FALSE,
                                                        reps=1,
                                                        holdback=0.25,
                                                        Undersample = TRUE,
                                                        hyperparameter_tuning = FALSE,
                                                        error_correction_method = 'None', # = must be 'Flip' or "Prune' or 'None'
                                                        best.m = NULL, #rf mytr parameter
                                                        testReps=2, # at least 2
                                                        indeterminateUpper=0.75,
                                                        indeterminateLower=0.25,
                                                        Type=1,
                                                        nTopImportance=20)


rm(list = ls())
devtools::load_all(".")

# Call the function for SEND SQLite database
db_path = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db"
studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/sqlite_20Liver_20not_liver.csv")

sqlite_real_all_TESTCD_score <- get_all_lb_TESTCD_zscore (studyid = "5003635", #
                                                          path_db = db_path,
                                                          fake_study= FALSE,
                                                          use_xpt_file = FALSE,
                                                          master_compiledata = NULL,
                                                          return_individual_scores = TRUE)
