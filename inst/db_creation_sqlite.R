rm(list = ls())

dbPath <- "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/pfda_challenge_data/FAKE49237/FAKE49237.db"
# #Database Load
dbtoken <- sendigR::initEnvironment(dbType = 'sqlite',
                                    dbPath = dbPath,
                                    dbCreate = TRUE)
