rm(list = ls())
devtools::load_all(".")

path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_xpt'
studyid_or_studyids <- list.dirs(path_db , full.names = TRUE, recursive = FALSE)
fake_80_medata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/fake_80_MD.csv",
                           header = TRUE, sep = ",", stringsAsFactors = FALSE)

perfoerem <- get_random_forest_model_performance(path_db=path_db,
                                                studyid_or_studyids=studyid_or_studyids,
                                                fake_80_medata=fake_80_medata)
