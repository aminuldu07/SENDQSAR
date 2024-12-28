# rm(list = ls())
# devtools::load_all(".")

# Initialize a connection to the SQLite database
#path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_merged_liver_not_liver.db'



get_random_forest_model_performance <- function(path_db,
                                                studyid_or_studyids,
                                                fake_80_medata
                                                ){


#path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_xpt'
#studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)

#path_db = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db"

fake80_liver_scores <- get_liver_om_lb_mi_tox_score_list(studyid_or_studyids = studyid_or_studyids,
                                                         path_db = path_db,
                                                         fake_study = TRUE,
                                                         use_xpt_file = TRUE,
                                                         #multiple_xpt_folder = TRUE,
                                                         output_individual_scores = TRUE,
                                                         output_zscore_by_USUBJID = FALSE)

# Harmonize the column

column_harmonized_liverscr_df <- get_col_harmonized_scores_df(liver_score_data_frame = fake80_liver_scores,
                                                              Round = TRUE)

#Data <- column_harmonized_liverscr_df

# histogram <- make_histogram(Data =Data,
#                            Round=TRUE,
#                            generateBarPlot= TRUE)

# add "studyid" description in Data data frame
# Should be two column
# First column "STUDYID"
# Second column "indst_To"
# Read the STUDYID metadata csv file
# fake_80_medata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_80_MD.csv",
#                            header = TRUE, sep = ",", stringsAsFactors = FALSE)


rfData_and_best_m <- prepare_data_and_tune_hyperparameters( scores_df = column_harmonized_liverscr_df,
                                                            studyid_metadata = fake_80_medata,
                                                            Impute = TRUE,
                                                            Round = TRUE,
                                                            reps=1,
                                                            holdback=0.25,
                                                            Undersample = TRUE,
                                                            hyperparameter_tuning = FALSE,
                                                            error_correction_method = 'None')


train_and_evaluate_rf_model <- train_eval_rf_with_cv_imp(scores_df = column_harmonized_liverscr_df,
                                                         studyid_metadata = fake_80_medata,
                                                         Impute = TRUE,
                                                         Round = TRUE,
                                                         reps=1,
                                                         holdback = 0.25,
                                                         Undersample = TRUE,
                                                         hyperparameter_tuning = FALSE,
                                                         error_correction_method = 'None',
                                                         best.m = NULL,
                                                         testReps = 5,
                                                         indeterminateUpper = .75,
                                                         indeterminateLower = .25,
                                                         Type = 1,
                                                         nTopImportance = 20)


}
