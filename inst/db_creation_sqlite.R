rm(list = ls())

dbPath <- "C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/pfda_challenge_data/FAKE49237.db"


# The database is created by the call of  initEnvironment with the parameter dbCreate = TRUE:
dbToken <- sendigR::initEnvironment(dbType = 'sqlite',
                                    dbPath = dbPath,
                                    dbCreate = FALSE)

# The tables must be created before any study can be imported
#sendigR::dbCreateSchema(dbToken)


status <- sendigR::dbImportOneStudy(dbToken,
                                    'C:/Users/MdAminulIsla.Prodhan/OneDrive - FDA/Documents/DATABASES/pfda_challenge_data/FAKE49237_xpt',

                                    overWrite = FALSE,
                                    checkRequiredVars = TRUE)


# Create a set of indexes to increase query performance for the data extraction functions
# - they may be created before of after import of data
sendigR::dbCreateteIndexes(dbToken)
