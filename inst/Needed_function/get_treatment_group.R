


get_treatment_group_myl <- function(studyid = NULL,
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
    pooldef <- fetch_domain_data(db_connection, 'pooldef', studyid)

    # Close the database connection
    DBI::dbDisconnect(db_connection)
  }

  # Identify unique treatment groups (SETCD) and study species
  number_of_setcd <- unique(dm[['SETCD']])
  print(number_of_setcd)

  # get the species from the tsparmcd
  #st_species <- ts[TSPARMCD=='SPECIES'][, TSVAL]
  st_species <- ts[ts$TSPARMCD=='SPECIES',"TSVAL"]

  # Add species and treatment groups to the results
  list_return[[study]][['species']] <- st_species

  list_return[[study]][['setcd']] <- number_of_setcd

  #recovery_group <- c()
  recovery_group <- c() # Recovery groups

  #treatment_group<- c()
  treatment_group <- c() # Treatment groups

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(length(st_species)!= 0) {
    # processing RAT species~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Handle "RAT" species specifically
    #if(st_species =="RAT") {
    if(st_species %in% c("RAT","MOUSE")) {
      # see if tkdesc in txparmcd
      parmcd <- unique(tx[['TXPARMCD']])
      if('TKDESC' %in% parmcd){
        tkdesc_in_parmcd <- TRUE
      } else {

        tkdesc_in_parmcd <- FALSE
      }
      ## tkdesc_in_parmcd
      # If TKDESC exists,
      # classify groups based on TK~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if(tkdesc_in_parmcd) {

        #unq_tkdesc <- unique(tx[TXPARMCD=='TKDESC',TXVAL])
        unq_tkdesc <- unique(tx[tx$TXPARMCD=='TKDESC',"TXVAL"], drop = FALSE)
        #unq_tkdesc <- unique(tx[tx$TXPARMCD=='TKDESC',"TXVAL)", drop = FALSE])

        if (length(unq_tkdesc) > 0) {
          if('TK' %in% unq_tkdesc) {
            tk_group <- unique(tx[TXPARMCD=='TKDESC' & TXVAL=='TK',  SETCD])

            ## print('tkin parmcd')
            ## print(tk)
          }
          not_tk <- number_of_setcd[which(!number_of_setcd %in% tk_group)]
        } else {
          # if, length(unq_tkdesc) = 0
          # If unq_tkdesc has no elements, ~~~~~~~~~~~~~~~~~~~~~~~~~~
          # Assign all elements in number_of_setcd to not_tk
          not_tk <- number_of_setcd
        }
      } else {
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # If TKDESC doesn't exists in parmcd in tx domai ~~~~~~~~~~~~~~~~~~~~~
        # Classify groups without TKDESC based on usubjid data~~~~~~~~~~~~~~~~
        #~~~~~~~~~using "PC" domain data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #when tkdes is not in parmcd at tx
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
browser()
        tk_group <- c()

        number_of_setcd <- c("1TK")

        for(i in 1:length(number_of_setcd)){
          set_cd  <- number_of_setcd[i]
          #subjid <- unique(dm[SETCD==set_cd, USUBJID])
          subjid <- unique(dm[dm$SETCD == set_cd, "USUBJID"])

          if(pc$USUBJID[1]!='') {
            uniq_pc_subj <- unique(pc$USUBJID)
            pc_sub <- 'not_empty'
          } else {
            uniq_pool <- unique(pc$POOLID)
            ## pooldef <- df_domain$pooldef
            pool_sub <- pooldef[POOLID %in% uniq_pool, USUBJID]
            pc_sub <- 'empty'
          }

          if(pc_sub=='not_empty'){
            if(any(subjid %in% uniq_pc_subj)){
              ## print(paste0(set_cd, ' : in TK'))
              tk_group <- c(tk_group, set_cd)
            }
          } else if (pc_sub=='empty'){
            if(any(subjid %in% pool_sub)){
              ## print(paste0(set_cd, ' : in TK and pool'))
              tk_group <- c(tk_group, set_cd)
            }
          }

        }
        browser()
        not_tk <- number_of_setcd[which(!number_of_setcd %in% tk_group)]
      }
      ## number_of_setcd
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Identify recovery and treatment groups in non-TK groups
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if(length(not_tk) > 0) {
        for (i in 1:length(not_tk)){
          set_cd <- not_tk[i]
          subjid <- unique(dm[SETCD==set_cd, USUBJID])
          dsdecod <- tolower(unique(ds[USUBJID %in% subjid, DSDECOD]))
          if(tolower("RECOVERY SACRIFICE") %in% dsdecod) {
            recovery_group <- c(recovery_group, set_cd)
          } else if (tolower("TERMINAL SACRIFICE") %in% dsdecod){
            treatment_group<- c(treatment_group, set_cd)
          }
        }
      }

      list_return[[study]][['TK_group']] <- tk_group
    } else {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # processing non-RAT species~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
        } else if (tolower(any(c("TERMINAL SACRIFICE",
                           'MORIBUND SACRIFICE',
                           'REMOVED FROM STUDY ALIVE',
                           'NON-MORIBUND SACRIFICE'))) %in% dsdecod){
          # get the treatment group
          treatment_group<- c(treatment_group, set_cd)
        }
      }
    }
  }
browser()
  # Add study to "four" list if exactly 4 treatment groups exist
  if( length(treatment_group) == 4)

    four <- c(four,study)
    ## print(four)
  }




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
















































# print(trtm_group)
  # list_return[[study]][['treatment_group']] <- trtm_group
  # list_return[[study]][['recovery_group']] <- recv_group
#}

# list_return[['four_trtm_group']] <- four
# list_return

###################
# Step-2 :: # REMOVE THE RECOVERY ANIMALS from "CompileData"...<>"Recovery
#  animals" cleaning.. using "DS domain"

# # filter for specific "DSDECOD" values...( Keep the mentioned four ) ...
# filtered_ds <- ds %>%
#   dplyr::filter(DSDECOD %in% c('TERMINAL SACRIFICE',
#                                'MORIBUND SACRIFICE',
#                                'REMOVED FROM STUDY ALIVE',
#                                'NON-MORIBUND SACRIFICE'))
###################################################################
