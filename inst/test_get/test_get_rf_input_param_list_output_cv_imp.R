rm(list = ls())
devtools::load_all(".")

#path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_xpt'
#path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_merged_liver_not_liver.db'


#studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)
# studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_80_MD.csv",
#                            header = TRUE, sep = ",", stringsAsFactors = FALSE)
#
# perfoerem <- get_random_forest_model_performance(path_db=path_db,
#                                                  studyid_metadata=studyid_metadata,
#                                                 fake_study = TRUE,
#                                                 use_xpt_file = FALSE)
path_db = 'C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db'

studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/sqlite_20Liver_20not_liver.csv")

#studyids <-  c ("2801-001", "8385518", "1517-036","130-1145", "FY19-023","10749")

#studyid_metadata1  <- studyid_metadata [studyid_metadata$STUDYID %in% studyids, ]

output_cv_imp <- get_rf_input_param_list_output_cv_imp( path_db=path_db,
                                                        rat_studies=FALSE,
                                                        studyid_metadata=studyid_metadata,
                                                        fake_study = TRUE,
                                                        use_xpt_file = FALSE,
                                                        Round = TRUE,
                                                        Impute = TRUE,
                                                        reps=1,
                                                        holdback=0.25,
                                                        Undersample = TRUE,
                                                        hyperparameter_tuning = FALSE, # best.m == 4, if hyperparameter_tuning = FALSE
                                                        error_correction_method = 'None',
                                                        testReps = 5,
                                                        indeterminateUpper = .75,
                                                        indeterminateLower = .25,
                                                        Type = 1,
                                                        nTopImportance = 20)

