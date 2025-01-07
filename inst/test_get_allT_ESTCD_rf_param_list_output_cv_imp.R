rm(list = ls())
devtools::load_all(".")

path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db'

#studyid_or_studyids  <- c("2170016")
#studyid_or_studyids  <- c( "5003635", "2170016", "876") #"5003635",
studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/sqlite_20Liver_20not_liver.csv")
#studyid_or_studyids <- studyid_metadata$STUDYID
# indiv_score_om_lb_mi <- get_indiv_score_om_lb_mi_domain_df(studyid_or_studyids = studyid_or_studyids,
#                                                            path_db = path_db,
#                                                            fake_study = FALSE,
#                                                            use_xpt_file = FALSE,
#                                                            output_individual_scores = TRUE,
#                                                            output_zscore_by_USUBJID = FALSE,
#                                                            all_lb_TESTCD_score = TRUE)


all_TESTCD_rf <-  get_all_TESTCD_rf_input_param_list_output_cv_imp(path_db=path_db,
                                                             rat_studies=FALSE,
                                                             studyid_metadata=studyid_metadata,
                                                             fake_study = FALSE,
                                                             use_xpt_file = FALSE,
                                                             all_lb_TESTCD_score=TRUE,
                                                             Round = TRUE,
                                                             Impute = TRUE,
                                                             reps=1,
                                                             holdback=0.25,
                                                             Undersample = TRUE,
                                                             hyperparameter_tuning = FALSE,
                                                             error_correction_method = 'None', # = must be 'Flip' or "Prune' or 'None'
                                                             best.m = NULL, #rf mytr parameter
                                                             testReps =2 , # at least 2
                                                             indeterminateUpper = 0.75,
                                                             indeterminateLower = 0.25,
                                                             Type = 1,
                                                             nTopImportance=20)
