rm(list = ls())
devtools::load_all(".")

path_db='C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db'

#studyid_or_studyids  <- c("2170016", "876")

studyid_or_studyids <- get_repeat_dose_parallel_studyids(path_db=path_db,
                                                          rat_studies = TRUE)
# rm(list = ls())
# devtools::load_all(".")
#
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
