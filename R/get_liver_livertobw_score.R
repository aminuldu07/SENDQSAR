
get_liver_livertobw_score <- function (studyid,
                                       path_db,
                                       fake_study = FALSE,
                                       use_xpt_file = FALSE,
                                       master_compiledata = NULL,
                                       bwzscore_BW = NULL,
                                       return_individual_scores = FALSE){

#' @~~~~~~~get_liver_livertobw_scor--bwzscore_BW-SHOULD-ALWAYS-BE-IN-LIST-FORMAT
  studyid <- as.character(studyid)
  path <- path_db

  # Helper function to fetch data from SQLite database
  fetch_domain_data <- function(db_connection, domain_name, studyid) {
    domain_name <- toupper(domain_name)
    query_statement <- paste0('SELECT * FROM ', domain_name, " WHERE STUDYID = :x")
    query_result <- DBI::dbGetQuery(db_connection, statement = query_statement, params = list(x = studyid))
    query_result
  }

  #Helper function to read data from .xpt files
  read_xpt_data <- function(path, domain_name) {
    domain_data <- haven::read_xpt(fs::path(path, paste0(domain_name, '.xpt')))
    data.table::setDT(domain_data)
    return(domain_data)
  }
  # con_db <- function(domain){
  #   domain <- toupper(domain)
  #   stat <- paste0('SELECT * FROM ', domain, " WHERE STUDYID = (:x)")
  #   domain <- DBI::dbGetQuery(con,
  #                             statement = stat,
  #                             params=list(x=studyid))
  # }

  # GET THE REQUIRED DOMAIN DATA

  if (fake_study == TRUE && use_xpt_file == FALSE){

    # Establish a connection to the SQLite database
    db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)

    # Fetch data for required domains
    om <- fetch_domain_data(db_connection, 'om', studyid)

    # Close the database connection
    DBI::dbDisconnect(db_connection)

    # om <- con_db('om')
    # data.table::setDT(om)
    # Select specific columns from dm
    om <- om[,c('USUBJID',"OMSPEC" ,"OMSTRESN", "OMTEST")]

  } else if (fake_study == TRUE && use_xpt_file == TRUE){

    # Read data from .xpt files
    om <- read_xpt_data(path, 'om')
    # Select specific columns from dm
    om <- om[,c('USUBJID',"OMSPEC" ,"OMSTRESN", "OMTEST")]

  } else if (fake_study == FALSE && use_xpt_file == FALSE) {
    # Establish a connection to the SQLite database
    db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
    # Fetch data for required domains
    om <- fetch_domain_data(db_connection, 'om', studyid)

    # Close the database connection
    DBI::dbDisconnect(db_connection)

    # Select specific columns from dm
    om <- om[,c('USUBJID',"OMSPEC" ,"OMSTRESN", "OMTEST")]

  }else if (fake_study == FALSE && use_xpt_file == TRUE) {

    # Read data from .xpt files
    om <- read_xpt_data(path, 'om')
    #om <- haven::read_xpt(fs::path(path,'om.xpt'))
    # Select specific columns from dm
    om <- om[,c('USUBJID',"OMSPEC" ,"OMSTRESN", "OMTEST")]
  }

  #' @~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
           # get THE COMPILE DATA IF NOT PRESENT
    #@~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     #<><><><><<><><>... Remove TK animals and Recovery animals....
    #' #' @get-master-compile-data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#     if (is.null(master_compiledata) && fake_study == TRUE && use_xpt_file == FALSE) {
#       # Call the master_compiledata function to generate the data frame for fake study
#       master_compiledata <- get_compile_data(studyid, path_db, fake_study = TRUE, use_xpt_file = FALSE)
#
#     } else if (is.null(master_compiledata) && fake_study == TRUE && use_xpt_file == TRUE) {
#       # Call the master_compiledata function to generate the data frame for fake study using xpt file
#       master_compiledata <- get_compile_data(studyid, path_db, fake_study = TRUE, use_xpt_file = TRUE)
#
#     } else if (is.null(master_compiledata) && fake_study == FALSE && use_xpt_file == FALSE) {
#
#       master_compiledata <- get_compile_data(studyid, path_db, fake_study = FALSE, use_xpt_file = FALSE)
#
#     } else if (is.null(master_compiledata) && fake_study == FALSE && use_xpt_file == TRUE) {
#
#       # Call the master_compiledata function for real study using xpt file
#       master_compiledata <- get_compile_data(studyid, path_db, fake_study = FALSE, use_xpt_file = TRUE)
#     }

  #' @~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #
   #' #' @get-master-compile-data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (is.null(master_compiledata) && fake_study == TRUE && use_xpt_file == FALSE) {
    # Call the master_compiledata function to generate the data frame for fake study
    master_compiledata <- get_compile_data(studyid, path_db, fake_study = fake_study, use_xpt_file = use_xpt_file)

  } else if (is.null(master_compiledata) && fake_study == TRUE && use_xpt_file == TRUE) {
    # Call the master_compiledata function to generate the data frame for fake study using xpt file
    master_compiledata <- get_compile_data(studyid, path_db, fake_study = fake_study, use_xpt_file = use_xpt_file)

  } else if (is.null(master_compiledata) && fake_study == FALSE && use_xpt_file == FALSE) {

    master_compiledata <- get_compile_data(studyid, path_db, fake_study = fake_study, use_xpt_file = use_xpt_file)

  } else if (is.null(master_compiledata) && fake_study == fake_study && use_xpt_file == TRUE) {

    # Call the master_compiledata function for real study using xpt file
    master_compiledata <- get_compile_data(studyid, path_db, fake_study = fake_study, use_xpt_file = use_xpt_file)
  }

  #' @~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Check if bwzscore_BW is NULL
  if (is.null(bwzscore_BW) && fake_study == TRUE && use_xpt_file == FALSE) {
    if (return_individual_scores) {
    bwzscore_BW <-  get_bw_score (studyid,
                                  path_db,
                                  fake_study = TRUE,
                                  use_xpt_file = FALSE,
                                  master_compiledata,
                                  return_individual_scores = TRUE)
    } else {
      bwzscore_BW <-  get_bw_score (studyid,
                                    path_db,
                                    fake_study = TRUE,
                                    use_xpt_file = FALSE,
                                    master_compiledata,
                                    return_individual_scores = TRUE)

    }


  } else if (is.null(bwzscore_BW) && fake_study == TRUE && use_xpt_file == TRUE) {

    if (return_individual_scores) {
    bwzscore_BW <-  get_bw_score (studyid = NULL,
                                  path_db,
                                  fake_study = TRUE,
                                  use_xpt_file = TRUE,
                                  master_compiledata,
                                  return_individual_scores = TRUE)
    } else {

      bwzscore_BW <-  get_bw_score (studyid = NULL,
                                    path_db,
                                    fake_study = TRUE,
                                    use_xpt_file = TRUE,
                                    master_compiledata,
                                    return_individual_scores = TRUE)

    }

  } else if (is.null(bwzscore_BW) && fake_study == FALSE && use_xpt_file == FALSE) {
    if (return_individual_scores) {
    bwzscore_BW <-  get_bw_score (studyid,
                                  path_db,
                                  fake_study = FALSE,
                                  use_xpt_file = FALSE,
                                  master_compiledata,
                                  return_individual_scores = TRUE)
    } else {

      bwzscore_BW <-  get_bw_score (studyid,
                                    path_db,
                                    fake_study = FALSE,
                                    use_xpt_file = FALSE,
                                    master_compiledata,
                                    return_individual_scores = TRUE)
    }

  } else if (is.null(bwzscore_BW) && fake_study == FALSE && use_xpt_file == TRUE) {
    if (return_individual_scores) {
    bwzscore_BW <-  get_bw_score (studyid = NULL,
                                  path_db,
                                  fake_study = FALSE,
                                  use_xpt_file = TRUE,
                                  master_compiledata,
                                  return_individual_scores = TRUE)
    } else {
      bwzscore_BW <-  get_bw_score (studyid = NULL,
                                    path_db,
                                    fake_study = FALSE,
                                    use_xpt_file = TRUE,
                                    master_compiledata,
                                    return_individual_scores = TRUE)
    }
  }



  # # Check if bwzscore_BW is NULL
  # if (is.null(bwzscore_BW) && fake_study == TRUE && use_xpt_file == FALSE) {
  #     bwzscore_BW <-  get_bw_score (studyid,
  #                                   path_db,
  #                                   fake_study = fake_study,
  #                                   use_xpt_file = use_xpt_file,
  #                                   master_compiledata = master_compiledata,
  #                                   return_individual_scores = TRUE)
  #
  #
  # } else if (is.null(bwzscore_BW) && fake_study == TRUE && use_xpt_file == TRUE) {
  #
  #     bwzscore_BW <-  get_bw_score (studyid = NULL,
  #                                   path_db,
  #                                   fake_study = fake_study,
  #                                   use_xpt_file = use_xpt_file,
  #                                   master_compiledata = master_compiledata,
  #                                   return_individual_scores = TRUE)
  #
  # } else if (is.null(bwzscore_BW) && fake_study == FALSE && use_xpt_file == FALSE) {
  #
  #     bwzscore_BW <-  get_bw_score (studyid,
  #                                   path_db,
  #                                   fake_study = fake_study ,
  #                                   use_xpt_file = use_xpt_file,
  #                                   master_compiledata = master_compiledata,
  #                                   return_individual_scores = TRUE)
  #
  # } else if (is.null(bwzscore_BW) && fake_study == FALSE && use_xpt_file == TRUE) {
  #     bwzscore_BW <-  get_bw_score (studyid = NULL,
  #                                   path_db,
  #                                   fake_study = fake_study,
  #                                   use_xpt_file = use_xpt_file,
  #                                   master_compiledata= master_compiledata,
  #                                   return_individual_scores = TRUE)
  # }
  #
  #
  #







# check for return_individual_scores


  # # Check if bwzscore_BW is NULL
  # if (is.null(bwzscore_BW) & fake_study == FALSE) {
  #   # Call the master_compiledata function to generate the data frame
  #   bwzscore_BW <-  get_bw_score (studyid,
  #                              path_db,
  #                              fake_study = FALSE,
  #                              master_compiledata = NULL,
  #                              return_individual_scores = TRUE)
  #
  # } else if (is.null(bwzscore_BW) & fake_study == TRUE) {
  #   # Call the master_compiledata function to generate the data frame
  #   bwzscore_BW <-  get_bw_score (studyid,
  #                                 path_db,
  #                                 fake_study = TRUE,
  #                                 master_compiledata = NULL,
  #                                 return_individual_scores = TRUE)
  # }

  # Initialize data frames to store the OrganWeights_Liver data
  OrganWeights_Liver <- data.frame(USUBJID = character(0), OMSPEC = character(0), OMSTRESN = numeric(0), OMTEST = character(0))

  # Extract data for the current STUDYID
  StudyData_current_liver <- om

  # Pull index of the LIVER data
  Studyidx_liver <- which(stringr::str_detect(StudyData_current_liver$OMSPEC, "LIVER"))

  # Pull relevant OM Data for LIVER
  OMD_liver <- StudyData_current_liver[Studyidx_liver, c("USUBJID", "OMSPEC", "OMSTRESN", "OMTEST")]

  # Append to the OrganWeights_Liver  data frame
  OrganWeights_Liver <- rbind(OrganWeights_Liver, OMD_liver)

  # Filter the OrganWeights_Liver data frame
  OrganWeights_Liver_Weight <- OrganWeights_Liver %>%
    dplyr::filter(OMTEST == "Weight")

  # Filter the OrganWeights_Liver_Weight data frame and select specific columns ("USUBJID", "OMSTRESN")
  OrganWeights_Liver_Weight_Selected_Col <- OrganWeights_Liver_Weight %>%
    dplyr::filter(OMTEST == "Weight") %>%
    dplyr::select(USUBJID, OMSTRESN)

  #<><><><>... Remove TK animals and Recovery animals from "OrganWeights_Liver_Weight_Selected_Col"..<><><>....
  #<><><><><><><><> master_compiledataaa is free of TK animals and Recovery animals<><><><><><><><><><><><><><>
  # Filter the data frame for removing recovery and TK animals.....................................

  #' #' @get-master-compile-data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #'
  #' if (is.null(master_compiledata) & fake_study == TRUE) {
  #'   # Call the master_compiledata function to generate the data frame for fake study
  #'   master_compiledata <- get_compile_data(studyid, path_db, fake_study = TRUE)
  #' } else if (is.null(master_compiledata) & fake_study == FALSE) {
  #'   # Call the master_compiledata function to generate the data frame for real study
  #'   master_compiledata <- get_compile_data(studyid, path_db, fake_study = FALSE)
  #' } else {
  #'   # If master_compiledata is already set, no action needed
  #'   master_compiledata = master_compiledata
  #' }


  # Filtering the tk animals and the recovery animals
  OrganWeights_Liver_filtered <- OrganWeights_Liver_Weight_Selected_Col %>%
    dplyr::filter(USUBJID %in% master_compiledata$USUBJID)

  # Perform a left join to match USUBJID and get ARMCD
  OrganWeights_Liver_with_ARMCD <- OrganWeights_Liver_filtered %>%
    dplyr::left_join(master_compiledata %>%
                       dplyr::select(STUDYID, USUBJID, ARMCD), by = "USUBJID")


  # Add "BodyWeight" data to the "OrganWeights_Liver_with_ARMCD" data frame
  OrganWeights_Liver_to_BWeight <- OrganWeights_Liver_with_ARMCD %>%
    dplyr::left_join(bwzscore_BW %>%
    dplyr::select(USUBJID, finalbodyweight), by = "USUBJID") %>%
    dplyr::mutate(liverToBW = OMSTRESN / finalbodyweight)

  # "liver_organ to BodyWeight" zscore calcualtion.............................................................
  # Create the "LiverZSCORE" column :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  liver_zscore_df <- OrganWeights_Liver_to_BWeight %>%
    dplyr::group_by(STUDYID) %>%
    # Replace Inf and -Inf with NA in liverToBW
    dplyr::mutate(liverToBW = replace(liverToBW, is.infinite(liverToBW), NA)) %>%
    # Calculate mean and standard deviation for "vehicle" ARMCD
    dplyr::mutate(
      mean_vehicle_liverToBW = mean(liverToBW[ARMCD == "vehicle"], na.rm = TRUE),
      sd_vehicle_liverToBW = sd(liverToBW[ARMCD == "vehicle"], na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%

    # Calculate z-score
    dplyr::mutate(
      liverToBW_zscore = (liverToBW - mean_vehicle_liverToBW) / sd_vehicle_liverToBW
    ) %>%
    # Optionally remove the mean_vehicle and sd_vehicle columns
    dplyr::select(-mean_vehicle_liverToBW, -sd_vehicle_liverToBW) %>%
    dplyr::select(STUDYID, USUBJID,liverToBW_zscore, ARMCD) %>%
    # Convert z-score to its absolute value
    dplyr::mutate(liverToBW_zscore = abs(liverToBW_zscore))

  # Filter and select specific columns
  HD_liver_zscore <- liver_zscore_df %>%
    dplyr::filter(ARMCD == "HD") %>%
    dplyr::select(STUDYID, USUBJID,  ARMCD, liverToBW_zscore)

  if (return_individual_scores) {

    HD_liver_zscore_df <- HD_liver_zscore %>%
      dplyr::group_by(STUDYID) %>%
      dplyr::mutate(liverToBW_zscore = replace(liverToBW_zscore,
                                               is.infinite(liverToBW_zscore), NA)) %>%
      dplyr::summarize(avg_liverToBW_zscore = mean(liverToBW_zscore, na.rm = TRUE))%>%
      dplyr::mutate(avg_liverToBW_zscore = abs(avg_liverToBW_zscore))  %>%
      dplyr::select(STUDYID, avg_liverToBW_zscore) %>%
      dplyr::mutate(avg_liverToBW_zscore = ifelse(avg_liverToBW_zscore >= 3, 3,
                                                  ifelse(avg_liverToBW_zscore >= 2, 2,
                                                         ifelse(avg_liverToBW_zscore >= 1, 1, 0))))


  } else {
    # Create final_liverToBW_df for the current STUDYID by averaging..................................
    averaged_liverToBW_df  <- HD_liver_zscore %>%
      dplyr::group_by(STUDYID) %>%
      dplyr::mutate(liverToBW_zscore = replace(liverToBW_zscore,
                                               is.infinite(liverToBW_zscore), NA)) %>%
      dplyr::summarize(avg_liverToBW_zscore = mean(liverToBW_zscore, na.rm = TRUE))%>%
      dplyr::mutate(avg_liverToBW_zscore = abs(avg_liverToBW_zscore))  %>%
      dplyr::select(STUDYID, avg_liverToBW_zscore) %>%
      dplyr::mutate(avg_liverToBW_zscore = ifelse(avg_liverToBW_zscore >= 3, 3,
                                                  ifelse(avg_liverToBW_zscore >= 2, 2,
                                                         ifelse(avg_liverToBW_zscore >= 1, 1, 0))))
  }


#return(final_liverToBW_df)
  # Return based on return_individual_scores
  if (return_individual_scores) {
    return(HD_liver_zscore_df)
  } else {
    return(averaged_liverToBW_df)
  }

}
