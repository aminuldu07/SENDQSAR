rm(list = ls())
devtools::load_all(".")

#-------------------------------------------------------------------------------
#---------------------------For-real-data---------------------------------------
#-------------------------------------------------------------------------------
#path_db = 'C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db'
#studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/sqlite_20Liver_20not_liver.csv")

##-----SQLite-----------------------
R_sqlite_path_db = 'C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/twenty8.db'
R_sqlite_studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/twenty8_Liver_not_Liver.csv")
R_sqlite_studyid_or_studyids <-as.vector(R_sqlite_studyid_metadata[,"STUDYID"])

#-------------------------- f1------------------------------------------------------------
# Call the function for "REAL SEND SQLite database"
R_SQL_compile_data <- get_compile_data(studyid='8382298',
                                       path_db = R_sqlite_path_db,
                                       fake_study = FALSE,
                                       use_xpt_file = FALSE)



#-------------------------- f2------------------------------------------------------------
R_sqlite_bw_score = get_bw_score(studyid="8382298",
                                    path_db=R_sqlite_path_db,
                                    fake_study=FALSE,
                                    use_xpt_file=FALSE,
                                    master_compiledata=NULL,
                                    return_individual_scores=FALSE,
                                    return_zscore_by_USUBJID=TRUE)


#-------------------------- f3------------------------------------------------------------
real_sqlite_livertobw_score = get_livertobw_score(studyid="8382298",
                                                  path_db= R_sqlite_path_db,
                                                  fake_study=FALSE,
                                                  use_xpt_file=FALSE,
                                                  master_compiledata=NULL,
                                                  bwzscore_BW = NULL,
                                                  return_individual_scores=FALSE,
                                                  return_zscore_by_USUBJID=FALSE)


#-------------------------- f4------------------------------------------------------------
R_SQL_lb_score <- get_lb_score(studyid = '8382298',
                                  path_db = R_sqlite_path_db,
                                  fake_study = FALSE,
                                  use_xpt_file = FALSE,
                                  master_compiledata = NULL,
                                  return_individual_scores = FALSE,
                                  return_zscore_by_USUBJID = TRUE)



#-------------------------- f5------------------------------------------------------------
real_sqlite_mi_score <- get_mi_score(studyid="8382298",
                                     path_db= R_sqlite_path_db,
                                     fake_study=FALSE,
                                     use_xpt_file=FALSE,
                                     master_compiledata=NULL,
                                     return_individual_scores=FALSE,
                                     return_zscore_by_USUBJID=FALSE)


#-------------------------- f6------------------------------------------------------------
# studyid_or_studyids <- c("R22-S229-RD","03061-22043",
#                          "923-0012-TX",
#                          "02081-22026","ZYT-774", "T2109511", "1877RD3", "8004042")  # issues with 1877RD3, T2109511

# get score in a list format
R_SQL_list_scores_om_lb_mi <- get_liver_om_lb_mi_tox_score_list(studyid_or_studyids = R_sqlite_studyid_or_studyids,
                                                       path_db = R_sqlite_path_db,
                                                       fake_study = FALSE,
                                                       use_xpt_file = FALSE,
                                                       output_individual_scores = TRUE,
                                                       output_zscore_by_USUBJID = FALSE)



#-------------------------- f7------------------------------------------------------------
column_harmonized_df <- get_col_harmonized_scores_df(liver_score_data_frame = R_SQL_list_scores_om_lb_mi,
                                         Round = FALSE)


#-------------------------- f8------------------------------------------------------------
#studyid_metadata5 <- studyid_metadata [studyid_metadata$STUDYID %in% studyid_or_studyids , ]

ml_data_with_tuned_hyperparameters <- get_ml_data_and_tuned_hyperparameters(column_harmonized_df = column_harmonized_df,
                                                  studyid_metadata = R_sqlite_studyid_metadata,
                                                  Impute = FALSE,
                                                  Round =FALSE,
                                                  reps = 2, # from 0 to any numeric number
                                                  holdback = 0.1, # either 1 or fraction value like 0.75 etc.
                                                  Undersample = FALSE,
                                                  hyperparameter_tuning = FALSE,
                                                  error_correction_method = "None") #{ #Default to "None"; #options: "Flip", "Prune", "None"


#--------------------------f9------------------------------------------------------------
# Build the ML model
# Get the ML formatted data as a variable from the list stored in "Data_ML_fomatted"
Data <- ml_data_with_tuned_hyperparameters[["rfData"]]
best.m <- ml_data_with_tuned_hyperparameters[["best.m"]]



simple_rf_model <- get_rf_model_with_cv(ml_formatted_scores_df = Data,
                                        Undersample = FALSE,
                                        best.m = NULL, # any numeric value or call function to get it
                                        testReps=2, # testRps must be at least 2;
                                        Type=1)


#--------------------------f10------------------------------------------------------------
zone_exclusioned_rf_model <- get_zone_exclusioned_rf_model_with_cv(   ml_formatted_scores_df= Data,
                                                                      Undersample = FALSE,
                                                                      best.m = NULL, # any numeric value or call function to get it
                                                                      testReps=2, # testRps must be at least 2;
                                                                      indeterminateUpper=0.75,
                                                                      indeterminateLower=0.25,
                                                                      Type=1)

#--------------------------f11------------------------------------------------------------

imp_features <- get_imp_features_from_rf_model_with_cv(  ml_formatted_scores_df = Data,
                                                         Undersample = FALSE,
                                                         best.m = NULL, # any numeric value or call function to get it
                                                         testReps=2, # testRps must be at least 2;
                                                         Type=1,
                                                         nTopImportance=10)

#--------------------------f12------------------------------------------------------------

modular_auc <- get_auc_curve_with_rf_model_modular(ml_formatted_scores_df=Data,
                                                 best.m = best.m # The 'mtry' hyperparameter for Random Forest
                                                )


auc_curve <- get_auc_curve_with_rf_model(ml_formatted_scores_df = NULL,
                                         path_db = R_sqlite_path_db, # Path to the SQLite database
                                         rat_studies=FALSE,
                                         studyid_metadata=NULL,
                                         #studyid_metadata = R_sqlite_studyid_metadata,
                                         fake_study = FALSE, # Whether to use fake study IDs
                                         use_xpt_file = FALSE,
                                         Round = FALSE, # Whether to round numerical values
                                         Impute = FALSE,
                                         best.m = NULL, # The 'mtry' hyperparameter for Random Forest
                                         reps=1, # from 0 to any numeric number
                                         holdback=0.1, # either 1 or fraction value like 0.75 etc.
                                         Undersample = FALSE,
                                         hyperparameter_tuning = FALSE,
                                         error_correction_method = "None",# # Choose: "Flip" or "Prune" or "None"
                                         output_individual_scores = TRUE,
                                         output_zscore_by_USUBJID = FALSE)

#--------------------------f13------------------------------------------------------------
histogram <- get_histogram_barplot(ml_formatted_scores_df =Data,
                                   generateBarPlot= TRUE,
                                   path_db= R_sqlite_path_db,
                                   rat_studies=FALSE,
                                   studyid_metadata = R_sqlite_studyid_metadata,
                                   fake_study = FALSE,
                                   use_xpt_file = FALSE,
                                   Round = FALSE,
                                   output_individual_scores = TRUE,
                                   utput_zscore_by_USUBJID = FALSE)

#--------------------------f14------------------------------------------------------------
get_reprtree <- get_reprtree_from_rf_model( ml_formatted_scores_df=Data,
                                            path_db = R_sqlite_path_db,
                                            rat_studies=FALSE,
                                            studyid_metadata = R_sqlite_studyid_metadata,
                                            fake_study = FALSE,
                                            use_xpt_file = FALSE,
                                            Round = FALSE,
                                            Impute = FALSE,
                                            reps=1,
                                            holdback=0.1,
                                            Undersample = FALSE,
                                            hyperparameter_tuning = FALSE,
                                            error_correction_method = "None",
                                            best.m = best.m)

#--------------------------f15------------------------------------------------------------
prediciton_plot <- get_prediction_plot( ml_formatted_scores_df=Data,
                                        path_db = R_sqlite_path_db,
                                        rat_studies=FALSE,
                                        studyid_metadata = R_sqlite_studyid_metadata,
                                        fake_study = FALSE,
                                        use_xpt_file = FALSE,
                                        Round = FALSE,
                                        Impute = FALSE,
                                        reps=1,
                                        holdback=0.1,
                                        Undersample = FALSE,
                                        hyperparameter_tuning = FALSE,
                                        error_correction_method = "None",
                                        best.m = best.m,
                                        testReps=2)

#--------------------------f16------------------------------------------------------------
ML_formatted_data <- get_Data_formatted_for_ml_and_best.m(path_db = R_sqlite_path_db,
                                                 rat_studies=FALSE,
                                                 studyid_metadata = R_sqlite_studyid_metadata,
                                                 fake_study = FALSE,
                                                 use_xpt_file = FALSE,
                                                 Round = FALSE,
                                                 Impute = FALSE,
                                                 reps=1,
                                                 holdback=0.1,
                                                 Undersample = FALSE,
                                                 hyperparameter_tuning = FALSE,
                                                 error_correction_method = "None" # = must be 'Flip' or "Prune' or 'None'
                                                )


ml_formatted_data <- get_Data_formatted_for_ml_and_best.m( path_db = R_sqlite_path_db,
                                                           rat_studies=FALSE,
                                                           studyid_metadata = R_sqlite_studyid_metadata,
                                                           fake_study = FALSE,
                                                           use_xpt_file = TRUE,
                                                           Round = FALSE,
                                                           Impute = FALSE,
                                                           reps=1,
                                                           holdback=0.1,
                                                           Undersample = FALSE,
                                                           hyperparameter_tuning = FALSE,
                                                           error_correction_method = "None" # = must be 'Flip' or "Prune' or 'None'
)

#--------------------------f17------------------------------------------------------------
output_cv_imp <- get_rf_input_param_list_output_cv_imp( path_db = R_sqlite_path_db,
                                                        rat_studies=FALSE,
                                                        studyid_metadata=R_sqlite_studyid_metadata,
                                                        fake_study = FALSE,
                                                        use_xpt_file = FALSE,
                                                        Round = FALSE,
                                                        Impute = FALSE,
                                                        reps=1,
                                                        holdback=0.1,
                                                        Undersample = FALSE,
                                                        hyperparameter_tuning = FALSE,
                                                        error_correction_method='None', # = must be 'Flip' or "Prune' or 'None'
                                                        best.m = NULL, #rf mytr parameter
                                                        testReps=2, # at least 2
                                                        indeterminateUpper=0.75,
                                                        indeterminateLower=0.25,
                                                        Type=1,
                                                        nTopImportance=10
                                                        )


#--------------------------f18------------------------------------------------------------
zone_exclusioned_rf_model <- get_zone_exclusioned_rf_model_with_cv( ml_formatted_scores_df=Data, #scores_df
                                                                    Undersample = FALSE,
                                                                    best.m = best.m, # any numeric value or call function to get it
                                                                    testReps=2, # testRps must be at least 2;
                                                                    Type=1)





#--------------------------f19------------------------------------------------------------

zone_exclusioned_rf_model_cv_imp <- get_zone_exclusioned_rf_model_cv_imp(  ml_formatted_scores_df,
                                                                           Undersample = FALSE,
                                                                           best.m = NULL, # any numeric value or call function to get it
                                                                           testReps, # testRps must be at least 2;
                                                                           indeterminateUpper,
                                                                           indeterminateLower,
                                                                           Type,
                                                                           nTopImportance)







