rm(list = ls())
#library(SENDQSAR)

##### Aminul islam prodhan

#setwd("C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/send-summarizer")
devtools::load_all()

##### compile_data_----------------------------
# Call the function for "REAL SEND SQLite database"
R_SQL_compile_data <- get_compile_data(studyid='5003635',
                                       path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db',
                                       fake_study = FALSE,
                                       use_xpt_file = FALSE)


# Call the function for " REAL SEND XPT data"
R_XPT_compile_data <- get_compile_data(studyid= NULL,
                                       path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/SEND_PUB_DB/send/instem',
                                       fake_study = FALSE,
                                       use_xpt_file = TRUE)


# Call the function for "FAKE/Synthetic SEND SQLite database"
fake_SQL_compile_data <- get_compile_data(studyid = '10663',
                                          path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/Documents/DATABASES/liver_1.db',
                                          fake_study = TRUE,
                                          use_xpt_file = FALSE)

#Call the function for "FAKE/Synthetic SEND XPT data"
fake_XPT_compile_data <- get_compile_data(studyid = NULL,
                                          path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/Documents/DATABASES/all_fakedata_liver_/FAKE10663',
                                          fake_study = TRUE,
                                          use_xpt_file = TRUE)

