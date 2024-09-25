rm(list = ls())
#setwd("C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/send-summarizer")
devtools::load_all(".")

compile_data <- get_compile_data(studyid='876', path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db',
                                 fake_study=FALSE)

fake_compile_data <- get_compile_data(studyid='10663', path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db',
                                      fake_study = TRUE)
