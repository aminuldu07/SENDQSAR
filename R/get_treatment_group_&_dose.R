


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

  not_RAT_recovery_group <- c()

  #treatment_group<- c()
  treatment_group <- c() # Treatment groups

  not_RAT_treatment_group <- c()

  Dose_Level_df <- data.frame(STUDYID = character(),
                              Dose_Level = character(),
                              Dose_Units = character(),
                              Treatment_SETCD = character())#,
  #stringsAsFactors = FALSE))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(length(st_species)!= 0) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # if species is 'RAT' , get the tk and non tk group
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Identify recovery and treatment groups in non-TK groups
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if(st_species =="RAT" && "MOUSE") {

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
      # Identify recovery and treatment groups in "non-TK groups"
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if(length(not_tk_group) > 0) {

        for (setcd in 1:length(not_tk_group)){

          set_cd <- not_tk_group[setcd]
          #subjid <- unique(dm[SETCD==set_cd, USUBJID])
          subjid <- unique(dm[dm$SETCD==set_cd, c("USUBJID")])
          #dsdecod <- tolower(unique(ds[USUBJID %in% subjid, DSDECOD]))
          dsdecod <- tolower(unique(ds[ds$USUBJID %in% subjid, "DSDECOD"])) # "ds" does not have SETCT, do had to use USUBJID matching from "dm"

          # Group assignments
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

      print(recovery_group)
      print(treatment_group)
       #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # get dose information for the treatment group
      # Iterate over each treatment group to extract dose information
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for (trtm_setcd in treatment_group) {

        # Filter `tx` table for the current SETCD
        tx_trtm_setcd <- tx [tx$SETCD == trtm_setcd, ]

        # Extract dose level and dose units
        dose_level <- tx_trtm_setcd[tx_trtm_setcd$TXPARMCD == "TRTDOS", "TXVAL"]
        dose_units <- tx_trtm_setcd[tx_trtm_setcd$TXPARMCD == "TRTDOSU", "TXVAL"]

        # Get the unique treatment SETCD for the current group
        treatment_setcd <- unique(tx_trtm_setcd$SETCD)

        # Create a data frame for the current treatment group
        dose_level_df <- data.frame(STUDYID = unique(tx$STUDYID),
                                    Dose_Level = dose_level,
                                    Dose_Units =  dose_units ,
                                    Treatment_SETCD = treatment_setcd) #,
        #stringsAsFactors = FALSE)

        # Append the current data to the main data frame
        Dose_Level_df <- rbind(Dose_Level_df, dose_level_df)

      }

       } else {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # if species not RAT , get the tk and non tk group
      # Non-RAT species does not have tk animals ?????????????????????????????
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Identify recovery and treatment groups in non-TK groups
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      not_RAT_not_tk <- number_of_setcd

      for (i in 1:length(not_RAT_not_tk)){
        not_RAT_set_cd <- not_RAT_not_tk[i]

        #subjid <- unique(dm[SETCD==set_cd, USUBJID])
        #not_RAT_subjid <- unique(dm[dm$SETCD==set_cd, "USUBJID"])

        # Extract unique subjects and DSDECOD values
        not_RAT_subjid <- unique(dm[dm$SETCD==not_RAT_set_cd, "USUBJID"])
        #dsdecod <- tolower(unique(ds[USUBJID %in% subjid, DSDECOD]))
        not_RAT_dsdecod <- tolower(unique(ds[ds$USUBJID %in% not_RAT_subjid, "DSDECOD"]))

        # Group assignments
        if(tolower("RECOVERY SACRIFICE") %in% not_RAT_dsdecod) {
          ## print(paste0(set_cd, ' : in recovery'))
          # get the recovery group

          not_RAT_recovery_group <- c(not_RAT_recovery_group, not_RAT_set_cd)
          print(recovery_group)

        } else if (any(tolower(not_RAT_dsdecod) %in% c("terminal sacrifice",
                                                'moribund sacrifice',
                                                'removed from study alive',
                                                'removed from study alive'))){
          # get the treatment group
          not_RAT_treatment_group <- c(not_RAT_treatment_group, not_RAT_set_cd)
        }
      }
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


      print( not_RAT_recovery_group)
      print( not_RAT_treatment_group)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # get dose information for the treatment group
      # Iterate over each treatment group to extract dose information
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      for (not_RAT_trtm_setcd in not_RAT_treatment_group) {

        # Filter `tx` table for the current SETCD
        not_RAT_tx_trtm_setcd <- tx [tx$SETCD == not_RAT_trtm_setcd, ]

        # Extract dose level and dose units
        not_RAT_dose_level <- not_RAT_tx_trtm_setcd[not_RAT_tx_trtm_setcd$TXPARMCD == "TRTDOS", "TXVAL"]
        not_RAT_dose_units <- not_RAT_tx_trtm_setcd[not_RAT_tx_trtm_setcd$TXPARMCD == "TRTDOSU", "TXVAL"]

        # Get the unique treatment SETCD for the current group
        not_RAT_treatment_setcd <- unique(not_RAT_tx_trtm_setcd$SETCD)

        # Create a data frame for the current treatment group
        not_RAT_dose_level_df <- data.frame(STUDYID = unique(tx$STUDYID),
                                    Dose_Level = not_RAT_dose_level,
                                    Dose_Units =  not_RAT_dose_units ,
                                    Treatment_SETCD = not_RAT_treatment_setcd) #,
        #stringsAsFactors = FALSE)

        # Append the current data to the main data frame
        Dose_Level_df <- rbind(Dose_Level_df, not_RAT_dose_level_df)

      }

    #}

   }

  } else {
      # If species is = zero
      # create an empty data frame

    recovery_group <- c()
    treatment_group <- c()

  }

  if (st_species == "RAT") {
  # return(list(Dose_Level_df = Dose_Level_df,
  #             recovery_group_setcd = recovery_group,
  #             treatment_group_setcd =treatment_group))
    return(Dose_Level_df)
  } else {
    # return(list(Dose_Level_df = Dose_Level_df,
    #             recovery_group_setcd = not_RAT_recovery_group,
    #             treatment_group_setcd = not_RAT_treatment_group))
    return(Dose_Level_df)

  }
}






