rm(list = ls())
devtools::load_all(".")

# Initialize a connection to the SQLite database
path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_merged_liver_not_liver.db'

studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/sqlite_20Liver_20not_liver.csv")


#path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_xpt'
#studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)
#path_db = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db"
studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_80_MD.csv",
                            header = TRUE, sep = ",", stringsAsFactors = FALSE)

# ----------------------------------------------------
# For this function we need ml format data
#----------------------------------------------------
prediction_plot <- get_prediction_plot(Data=NULL,
                                  path_db=path_db,
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
                                  error_correction_method = 'None', # = must be 'Flip' or "Prune' or 'None'
                                  testReps=3)


# simple_rf_model <- get_rf_model_with_cv(Data = Data,
#                                         Undersample = FALSE,
#                                         best.m = NULL, # any numeric value or call function to get it
#                                         testReps=2, # testRps must be at least 2;
#                                         Type=1)



# rf_with_intermediate <- get_imp_features_from_rf_model_with_cv(scores_df=Data,
#                                                    Undersample = TRUE,
#                                                    best.m = 4, # any numeric value or call function to get it
#                                                    testReps=2, # testRps must be at least 2;
#                                                    indeterminateUpper=0.75,
#                                                    indeterminateLower=0.25,
#                                                    Type=1,
#                                                    nTopImportance=20)








#rf_model <- get_random_forest_model_amin2(Data=rf_Data)



# # Create a connection to the database
# dbtoken <- DBI::dbConnect(RSQLite::SQLite(), dbname = path_db)
#
# # Retrieve the STUDYID column from the dm table
# query <- "SELECT STUDYID FROM dm"
# studyid_data <- DBI::dbGetQuery(dbtoken, query)
#
# # Extract unique STUDYID values
# unique_studyids <- unique(studyid_data$STUDYID)
#
# # Disconnect from the database
# DBI::dbDisconnect(dbtoken)
#
# studyid_or_studyids <- unique_studyids

#studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)

# #@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# rm(list = ls())
# devtools::load_all(".")
# path_db <- "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_merged_liver_not_liver.db"
# studyid_metadata_path <- "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_80_MD.csv"
#
# rfData_and_best_m <- get_rfData_and_best_m(
#   path_db = path_db,
#   studyid_metadata_path = studyid_metadata_path,
#   fake_study = TRUE,
#   Round = TRUE,
#   Undersample = TRUE
# )
