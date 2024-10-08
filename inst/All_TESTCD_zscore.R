rm(list = ls())
devtools::load_all(".")

all_testcd_zscore <- get_all_lb_TESTCD_zscore(studyid = '2170016',
                                  path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db',
                                  fake_study= FALSE,
                                  use_xpt_file = FALSE,
                                  master_compiledata = NULL,
                                  return_individual_scores = TRUE)
