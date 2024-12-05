


get_treatment_group_amin <- function(studyid = NULL,
                                path_db,
                                fake_study=FALSE,
                                use_xpt_file = FALSE,
                                master_compiledata = NULL,
                                return_individual_scores = FALSE,
                                return_zscore_by_USUBJID = FALSE) {

  list_return <- list() # Initialize the result list
  four <- c()  # To store studies with exactly 4 treatment groups
  study <- studyid
  studyid <- as.character(studyid)
  path <- path_db

  # Helper function to fetch data from SQLite database
  fetch_domain_data <- function(db_connection, domain_name, studyid) {
    domain_name <- toupper(domain_name)
    query_statement <- paste0('SELECT * FROM ', domain_name, " WHERE STUDYID = :x")
    query_result <- DBI::dbGetQuery(db_connection, statement = query_statement, params = list(x = studyid))
    query_result
  }

  # GET THE REQUIRED DOMAIN DATA
  if (use_xpt_file) {
    # Read data from .xpt files
    mi <- haven::read_xpt(fs::path(path, 'mi.xpt'))

    dm <- haven::read_xpt(fs::path(path,'dm.xpt'))

  } else {
    # Establish a connection to the SQLite database
    db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)

    # Fetch data for required domains
    tx <- fetch_domain_data(db_connection, 'tx', studyid)
    ts <- fetch_domain_data(db_connection, 'ts', studyid)
    ds <- fetch_domain_data(db_connection, 'ds', studyid)
    dm <- fetch_domain_data(db_connection, 'dm', studyid)
    pc <- fetch_domain_data(db_connection, 'pc', studyid)
    pp <- fetch_domain_data(db_connection, 'pp', studyid)
    pooldef <- fetch_domain_data(db_connection, 'pooldef', studyid)

    # Close the database connection
    DBI::dbDisconnect(db_connection)
  }

  # Identify unique treatment groups (SETCD) from DM
  # and study species from ts
  number_of_setcd <- unique(dm[['SETCD']])
  print(number_of_setcd)

  # get the species from the tsparmcd
  #st_species <- ts[TSPARMCD=='SPECIES'][, TSVAL]
  st_species <- ts[ts$TSPARMCD=='SPECIES',"TSVAL"]

  # # Add species and treatment groups to the results
  # list_return[[study]][['species']] <- st_species
  #
  # list_return[[study]][['setcd']] <- number_of_setcd

  #recovery_group <- c()
  recovery_group <- c() # Recovery groups

  #treatment_group<- c()
  treatment_group <- c() # Treatment groups

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(length(st_species)!= 0) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # if species is 'RAT' , get the tk and non tk group
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Identify recovery and treatment groups in non-TK groups
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(st_species =="RAT") {

      # Initialize an empty data frame to store the results
      tK_animals_df <- data.frame(PP_PoolID = character(),
                                  STUDYID = character(),
                                  USUBJID = character(),
                                  POOLID = character(),
                                  stringsAsFactors = FALSE)

      # Create TK individuals for "Rat" studies
      # [Retrieve unique pool IDs (TKPools) from pp table]
      TKPools <- unique(pp$POOLID) # why not pooldef poold,

      # Check if TKPools is not empty
      if (length(TKPools) > 0) {
        # For each pool ID in TKPools, retrieve corresponding rows from pooldef table
        for (pool_id in TKPools) {
          # pooldef_data == unique pp$POOLID
          pooldef_data <- pooldef[pooldef$POOLID == pool_id, ] # maching unique POOLID of pooldef and pp

          # Create a temporary data frame if pooldef_data is not empty
          if (nrow(pooldef_data) > 0) {
            temp_df <- data.frame(PP_PoolID = pool_id,
                                  STUDYID = pooldef_data$STUDYID,
                                  USUBJID = pooldef_data$USUBJID,
                                  POOLID = pooldef_data$POOLID,
                                  stringsAsFactors = FALSE)

            # Append the temporary data frame to the results data frame
            tK_animals_df <- rbind(tK_animals_df, temp_df)
          }
        }

      }


      # filter dm sujid by the temp_df for getting the setcd
      setcd_dm_tk_less <- dm[dm$USUBJID %in% tK_animals_df$USUBJID, c ("STUDYID", "USUBJID", "SETCD")]

      tk_group <- unique(setcd_dm_tk_less$SETCD)
      not_tk_group <- number_of_setcd[which(!number_of_setcd %in% tk_group)]


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Identify recovery and treatment groups in non-TK groups
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if(length(not_tk_group) > 0) {

        for (setcd in 1:length(not_tk_group)){
          set_cd <- not_tk_group[setcd]
          #subjid <- unique(dm[SETCD==set_cd, USUBJID])
          subjid <- unique(dm[dm$SETCD==set_cd, c("USUBJID")])
          #dsdecod <- tolower(unique(ds[USUBJID %in% subjid, DSDECOD]))
          dsdecod <- tolower(unique(ds[ds$USUBJID %in% subjid, "DSDECOD"]))
          if(tolower("RECOVERY SACRIFICE") %in% dsdecod) {
            recovery_group <- c(recovery_group, set_cd)
          } else if (any(tolower(dsdecod) %in% c( "terminal sacrifice",
                                          "moribund sacrifice",
                                          "removed from study alive",
                                          "non-moribund sacrifice" ))) {
            treatment_group <- c(treatment_group, set_cd)
          }


        }


      }
    } else {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # if species not RAT , get the tk and non tk group
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Identify recovery and treatment groups in non-TK groups
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      not_tk <- number_of_setcd
      for (i in 1:length(not_tk)){
        set_cd <- not_tk[i]
        #subjid <- unique(dm[SETCD==set_cd, USUBJID])

        subjid <- unique(dm[dm$SETCD==set_cd, "USUBJID"], drop = FALSE)
        #dsdecod <- tolower(unique(ds[USUBJID %in% subjid, DSDECOD]))
        dsdecod <- tolower(unique(ds[ds$USUBJID %in% subjid, "DSDECOD"], drop = FALSE))
        if(tolower("RECOVERY SACRIFICE") %in% dsdecod) {
          ## print(paste0(set_cd, ' : in recovery'))
          # get the recovery group
          recovery_group <- c(recovery_group, set_cd)
        } else if (any(tolower(dsdecod) %in% c("TERMINAL SACRIFICE",
                                                'MORIBUND SACRIFICE',
                                                'REMOVED FROM STUDY ALIVE',
                                                'NON-MORIBUND SACRIFICE'))){
          # get the treatment group
          treatment_group<- c(treatment_group, set_cd)
        }
      }

    }

  } else {
      # If species is = zero
      # create an empty data frame

    recovery_group <- c()
    treatment_group <- c()

  }

  return(list(recovery_group_setcd = recovery_group,
                   treatment_group_setcd =treatment_group))

  }








