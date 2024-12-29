rm(list = ls())
devtools::load_all(".")

# Initialize a connection to the SQLite database
#path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_merged_liver_not_liver.db'

#path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_xpt'

#studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)

path_db = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db"

# studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_80_MD.csv",
#                             header = TRUE, sep = ",", stringsAsFactors = FALSE)

# ----------------------------------------------------
# For this function we need ml format data
#----------------------------------------------------
Data <- get_Data_formatted_for_ml(path_db=path_db,
                                      rat_studies=TRUE,
                                      studyid_metadata=NULL,
                                      fake_study = FALSE,
                                      use_xpt_file = FALSE,
                                      Round = TRUE,
                                      Impute = TRUE,
                                      reps=1,
                                      holdback=0.25,
                                      Undersample =TRUE,
                                      hyperparameter_tuning = FALSE,
                                      error_correction_method = 'None' # = must be 'Flip' or "Prune' or 'None'
                                      )

