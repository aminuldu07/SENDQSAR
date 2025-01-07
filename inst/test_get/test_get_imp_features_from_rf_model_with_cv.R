rm(list = ls())
devtools::load_all(".")

# Initialize a connection to the SQLite database
path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_merged_liver_not_liver.db'

#path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_xpt'
#studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)
#path_db = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db"
studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_80_MD.csv",
                            header = TRUE, sep = ",", stringsAsFactors = FALSE)

#----------------------------------------------------
# For this function we need ml format data
#----------------------------------------------------
Data <- get_Data_formatted_for_ml(path_db=path_db,
                                  rat_studies=FALSE,
                                  studyid_metadata=studyid_metadata,
                                  fake_study = TRUE,
                                  use_xpt_file = FALSE,
                                  Round = TRUE,
                                  Impute = TRUE,
                                  reps=1,
                                  holdback=0.25,
                                  Undersample =TRUE,
                                  hyperparameter_tuning = FALSE,
                                  error_correction_method = 'None') # = must be 'Flip' or "Prune' or 'None'



gini_imp <- get_imp_features_from_rf_model_with_cv(Data=Data, #scores_df
                                                   Undersample = FALSE,
                                                    best.m = NULL, # any numeric value or call function to get it
                                                    testReps=2, # testRps must be at least 2;
                                                    Type=1,
                                                    nTopImportance=20)





