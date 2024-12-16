rm(list = ls())
devtools::load_all(".")

path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db'

#studyid_or_studyids  <- c("2170016", "876")

rdpsids <- get_repeat_dose_parallel_studyids (dbPath=path_db,
                                               rat_studies = FALSE)
