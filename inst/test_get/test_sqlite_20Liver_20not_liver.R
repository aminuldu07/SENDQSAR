rm(list = ls())
devtools::load_all(".")

# Call the function for SEND SQLite database
path_db = "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/TestDB.db"
studyid_metadata <- read.csv("C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/sqlite_20Liver_20not_liver.csv")


repeat_dose_parallel_rat <- get_repeat_dose_parallel_studyids(path_db=path_db,
                                               rat_studies = TRUE)



#------------------------------------------------------------------------------
#------------------studyid_metadata---creation---------------------------------
#-----------------------------------------------------------------------------
# Randomly select 40 rows from "repeat_dose_parallel_rat"
# and creaet "sqlite_20Liver_20not_liver"

# Randomly select 40 rows from the repeat_dose_parallel_rat data frame
set.seed(123) # Set seed for reproducibility
sqlite_20Liver_20not_liver <- repeat_dose_parallel_rat[sample(nrow(repeat_dose_parallel_rat), 40), ]

sqlite_20Liver_20not_liver$Target_Organ <- NA

sqlite_20Liver_20not_liver <- sqlite_20Liver_20not_liver[ ,c("STUDYID", "Target_Organ")]

# assign "Target_Organ" column values randomly
# randomly 50% of the value is Liver and rest are not_Liver
set.seed(123)  # Set seed for reproducibility
rows_number <- nrow(sqlite_20Liver_20not_liver)  # Number of rows

# Randomly sample 50% for "Liver" and rest for "not_Liver"
sqlite_20Liver_20not_liver$Target_Organ <- sample(c("Liver", "not_Liver"), size = rows_number, replace = TRUE, prob = c(0.5, 0.5))

#write.csv(sqlite_20Liver_20not_liver, "sqlite_20Liver_20not_liver.csv", row.names = FALSE)

