rm(list = ls())
devtools::load_all(".")

#path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db'
path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_merged_liver_not_liver.db'

studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_80_MD.csv",
                             header = TRUE, sep = ",", stringsAsFactors = FALSE)

#studyid_or_studyids  <- c("2170016", "876")

get_auc_curve_plot <- get_auc_curve_with_rf_model(Data = NULL, # Input data frame for training
                                    path_db=path_db, # Path to the SQLite database
                                    rat_studies=FALSE,
                                    studyid_metadata=studyid_metadata,
                                    fake_study = TRUE, # Whether to use fake study IDs
                                    use_xpt_file = FALSE,
                                    Round = TRUE, # Whether to round numerical values
                                    Impute = TRUE,
                                    best.m = NULL, # The 'mtry' hyperparameter for Random Forest
                                    reps=2, # from 0 to any numeric number
                                    holdback=0.25, # either 1 or fraction value like 0.75 etc.
                                    Undersample = FALSE,
                                    hyperparameter_tuning = FALSE,
                                    error_correction_method = "None",# # Choose: "Flip" or "Prune" or "None"
                                    output_individual_scores = TRUE,
                                    output_zscore_by_USUBJID = FALSE)

# # #path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_xpt'
# # path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_merged_liver_not_liver.db'
# #
# #
# # #studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)
# # studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_80_MD.csv",
# #                            header = TRUE, sep = ",", stringsAsFactors = FALSE)
# #
#
#
# # Initialize a connection to the SQLite database
# path_db <- "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db"
#
#
# rdpst <- get_repeat_dose_parallel_studyids (path_db=path_db,
#                                             rat_studies = FALSE)







#---------------------------------------------
#---------------------------------------------------------------------------
# Check if rfData is NULL, calculate rfData
# if (is.null(rfData) || is.null(best.m)) {
#   if (is.null(path_db) || is.null(studyid_metadata_path)) {
#     stop("Both 'path_db' and 'studyid_metadata_path' must be provided if 'rfData' or 'best.m' is NULL.")
#   }
#
#   # Generate rfData and best.m using get_rfData_and_best_m
#   rfData_and_best_m <- get_rfData_and_best_m(
#     path_db = path_db,
#     studyid_metadata_path = studyid_metadata_path,
#     fake_study = fake_study,
#     Round = Round,
#     Undersample = Undersample
#   )
#
#   rfData <- rfData_and_best_m[["rfData"]]
#   best.m <- rfData_and_best_m[["best.m"]]
# }
