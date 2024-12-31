get_liver_om_lb_mi_tox_score_list <- function (studyid_or_studyids = FALSE,
                                               path_db,
                                               fake_study = FALSE,
                                               use_xpt_file = FALSE,
                                               # multiple_xpt_folder = FALSE,
                                               output_individual_scores = FALSE,
                                               output_zscore_by_USUBJID = FALSE) {

  # "multiple_xpt_folder" argument control the studyid/xpt folder directory
  # Enforce mutual exclusivity: If both are TRUE, throw an error
  if (output_individual_scores && output_zscore_by_USUBJID) {
    stop("Error: Both 'return_individual_scores' and 'output_zscore_by_USUBJID' cannot be TRUE at the same time.")
  }
  if(output_individual_scores ) {
  #initialize requires several data frame and list container
  } else if (output_zscore_by_USUBJID){
    #initialize requires several data frame and list container
  } else {
    #initialize requires several data frame and list container
  }
  # iterate over studyid or each xpt folder
  #for (studyid in selected_studies){
  for (studyid in studyid_or_studyids ){

    #if( multiple_xpt_folder == TRUE) {
    if(use_xpt_file == TRUE) {

      path_db <- studyid # giving the path of the xpt folder

      print(path_db)
    }

    # Initialize a flag variable at the start of each iteration
    first_block_success <- TRUE

    # First Block with its own tryCatch for master_compiledata~~~~~~~~~~~~~~~~~~
    tryCatch({

      # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
      studyid <- if (use_xpt_file) NULL else studyid

      # if use_xpt_file = TRUE,studyid should be NULL..........................
      # Call "get_liver_compiledata" function to get the master_compiledata
      output_get_compile_data <- get_compile_data(studyid = studyid ,
                                                  path_db = path_db, # problem is here
                                                  fake_study = fake_study,
                                                  use_xpt_file = use_xpt_file)

      # GET the  "master_compiledata" -data frame- from the output of the --
      master_compiledata <- output_get_compile_data

      # Create a copy of master_compiledata for the diagnostic purpose
      master_compiledata_copy <- master_compiledata
    },
    # Handle the errors
    error = function(e) {
      master_error_df <<- rbind(master_error_df, error_block1)

      # Set the flag to FALSE to indicate the first block failed
      first_block_success <<- FALSE

    })

    # Check the flag to decide whether to proceed to the next iteration of the loop
    if (!first_block_success) {

      # Append STUDYID  to the error_studies list
      Error_studies <- c(Error_studies, studyid)

      next
    }

    #-----------------end of master_compiledata calculation----------------------
    #This block for "Adding a new row for the current STUDYID in FOUR_Liver_Score"
    tryCatch({


      if (!output_individual_scores && !output_zscore_by_USUBJID) {
        new_row_in_four_liver_scr_avg <- data.frame(STUDYID = unique(master_compiledata$STUDYID),
                                                    BWZSCORE_avg = NA,
                                                    liverToBW_avg = NA,
                                                    LB_score_avg = NA,
                                                    MI_score_avg = NA)
        FOUR_Liver_Score_avg <- rbind(FOUR_Liver_Score_avg, new_row_in_four_liver_scr_avg)
      }

    }, error = function(e) { # error handling properly

    })
    #-----------------END--of ---score_accumulation_df----------------------------
    #------------------Calculation_of--BodyWeight_zScore--------------------------

    tryCatch({
      if(output_individual_scores){
        # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
        studyid <- if (use_xpt_file) NULL else studyid

        bwzscore_BW <- get_bw_score (studyid = studyid,
                                     path_db = path_db,
                                     fake_study = fake_study,
                                     use_xpt_file = use_xpt_file,
                                     master_compiledata = master_compiledata,
                                     return_individual_scores = TRUE,
                                     return_zscore_by_USUBJID = FALSE)

      } else if (output_zscore_by_USUBJID) {

        # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
        studyid <- if (use_xpt_file) NULL else studyid
        BW_zscore_by_USUBJID_HD <-get_bw_score(studyid = studyid,
                                               path_db = path_db,
                                               fake_study = fake_study,
                                               use_xpt_file = use_xpt_file,
                                               master_compiledata = master_compiledata,
                                               return_individual_scores = FALSE,
                                               return_zscore_by_USUBJID = TRUE)

        BW_zscore_by_USUBJID_HD <- as.data.frame(BW_zscore_by_USUBJID_HD)

      } else {
        # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
        studyid <- if (use_xpt_file) NULL else studyid


        averaged_HD_BWzScore <- get_bw_score (studyid = studyid,
                                              path_db = path_db,
                                              fake_study = fake_study,
                                              use_xpt_file = use_xpt_file,
                                              master_compiledata = master_compiledata ,
                                              return_individual_scores = FALSE,
                                              return_zscore_by_USUBJID = FALSE)
        print(averaged_HD_BWzScore)
        # # Add the liverToBW_zscore to "FOUR_Liver_Score" data frame............

        # Extract the liverToBW value for the current STUDYID from liverToBW_df
        calculated_BWzScore_value <-  averaged_HD_BWzScore$BWZSCORE_avg[averaged_HD_BWzScore$STUDYID == unique(master_compiledata$STUDYID)]
        #calculated_liverToBW_value <- liverToBW_df$liverToBW_avg

        # Update the liverToBW value in FOUR_Liver_Score_avg for the current STUDYID
        FOUR_Liver_Score_avg$BWZSCORE_avg[FOUR_Liver_Score_avg$STUDYID == unique(master_compiledata$STUDYID)] <- calculated_BWzScore_value

      }
    }, error = function(e) {
      #hanfle error properly

    })
    #---------------------------"OM_DATA"-(Liver_Organ to Body Weight zScore)-------
    tryCatch({
      # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
      studyid <- if (use_xpt_file) NULL else studyid

      if(output_individual_scores){
        # when, output_individual_scores == TRUE
        # bwzscore_BW  need to be calculated, so we don't need to calcualte
        # here
        HD_liver_zscore_df <- get_livertobw_score (studyid = studyid,
                                                   path_db = path_db,
                                                   fake_study = fake_study,
                                                   use_xpt_file = use_xpt_file,
                                                   master_compiledata = master_compiledata,
                                                   bwzscore_BW = bwzscore_BW,
                                                   return_individual_scores = TRUE,
                                                   return_zscore_by_USUBJID = FALSE)

        HD_liver_zscore_df <- as.data.frame(HD_liver_zscore_df)
        master_liverToBW <- rbind(master_liverToBW, HD_liver_zscore_df )

      } else if (output_zscore_by_USUBJID) {

        # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
        studyid <- if (use_xpt_file) NULL else studyid

        bwzscore_BW <- get_bw_score (studyid = studyid,
                                     path_db = path_db,
                                     fake_study = fake_study,
                                     use_xpt_file = use_xpt_file,
                                     master_compiledata = master_compiledata,
                                     return_individual_scores = TRUE,
                                     return_zscore_by_USUBJID = FALSE)
        liverTOBW_zscore_by_USUBJID_HD <- get_livertobw_score (studyid = studyid,
                                                               path_db = path_db ,
                                                               fake_study = fake_study,
                                                               use_xpt_file = use_xpt_file,
                                                               master_compiledata = master_compiledata,
                                                               bwzscore_BW = bwzscore_BW,
                                                               return_individual_scores = FALSE,
                                                               return_zscore_by_USUBJID = TRUE)

        liverTOBW_zscore_by_USUBJID_HD <- as.data.frame(liverTOBW_zscore_by_USUBJID_HD)
        liverTOBW_study_identifier <- unique(liverTOBW_zscore_by_USUBJID_HD$STUDYID)
        # Use the study_identifier as the list index
        master_liverToBW[[as.character(liverTOBW_study_identifier)]] <- liverTOBW_zscore_by_USUBJID_HD

      } else {

        # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
        studyid <- if (use_xpt_file) NULL else studyid

        # if (is.null(bwzscore_BW)) {
        bwzscore_BW <- get_bw_score (studyid = studyid,
                                     path_db = path_db,
                                     fake_study = fake_study,
                                     use_xpt_file = use_xpt_file,
                                     master_compiledata = master_compiledata,
                                     return_individual_scores = TRUE,
                                     return_zscore_by_USUBJID = FALSE)

        averaged_liverToBW_df <- get_livertobw_score (studyid = studyid,
                                                      path_db = path_db,
                                                      fake_study = fake_study,
                                                      use_xpt_file = use_xpt_file,
                                                      master_compiledata = master_compiledata,
                                                      bwzscore_BW = bwzscore_BW ,
                                                      return_individual_scores = FALSE,
                                                      return_zscore_by_USUBJID = FALSE)

        # Create "liverToBW_df" for FOUR_Liver_Score_avg
        liverToBW_df <- averaged_liverToBW_df  %>%
          dplyr::rename(liverToBW_avg = avg_liverToBW_zscore)

        # Extract the liverToBW value for the current STUDYID from liverToBW_df
        calculated_liverToBW_value <- liverToBW_df$liverToBW_avg[liverToBW_df$STUDYID == unique(master_compiledata$STUDYID)]
        #calculated_liverToBW_value <- liverToBW_df$liverToBW_avg
        # Update the liverToBW value in FOUR_Liver_Score_avg for the current STUDYID
        print(calculated_liverToBW_value)
        FOUR_Liver_Score_avg$liverToBW_avg[FOUR_Liver_Score_avg$STUDYID == unique(master_compiledata$STUDYID)] <- calculated_liverToBW_value

      }
    }, error = function(e) {
      #handle error properly
    })

    #<><><><><><><><><><><><><><><><><><>"""LB"""" zscoring <><><><><><><><><><><>
    tryCatch({
      # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
      studyid <- if (use_xpt_file) NULL else studyid
      if(output_individual_scores){
        master_lb_scores <- get_lb_score(studyid = studyid,
                                         path_db = path_db,
                                         fake_study= fake_study,
                                         use_xpt_file = use_xpt_file,
                                         master_compiledata = master_compiledata,
                                         return_individual_scores = TRUE,
                                         return_zscore_by_USUBJID = FALSE)

        master_lb_score_six <- rbind(master_lb_score_six , master_lb_scores)

      } else if (output_zscore_by_USUBJID) {
        # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
        studyid <- if (use_xpt_file) NULL else studyid
        LB_zscore_by_USUBJID_HD <- get_lb_score(studyid = studyid,
                                                path_db = path_db,
                                                fake_study= fake_study,
                                                use_xpt_file = use_xpt_file,
                                                master_compiledata = master_compiledata,
                                                return_individual_scores = FALSE,
                                                return_zscore_by_USUBJID = TRUE)

        lb_study_identifier <- unique(LB_zscore_by_USUBJID_HD$STUDYID)
        # append to the master data frame list
        # Use the study_identifier as the list index
        master_lb_score[[as.character(lb_study_identifier)]] <- LB_zscore_by_USUBJID_HD
      } else {
        # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
        studyid <- if (use_xpt_file) NULL else studyid

        averaged_LB_score <- get_lb_score(studyid = studyid,
                                          path_db = path_db,
                                          fake_study= fake_study,
                                          use_xpt_file = use_xpt_file ,
                                          master_compiledata = master_compiledata,
                                          return_individual_scores = FALSE,
                                          return_zscore_by_USUBJID = FALSE)
        # Extract the LB_score value for the current STUDYID from LB_df
        calculated_LB_value <- averaged_LB_score$LB_score_avg[ averaged_LB_score$STUDYID == unique(master_compiledata$STUDYID)]

        # Update the LB_score value in FOUR_Liver_Score for the current STUDYID
        FOUR_Liver_Score_avg$LB_score_avg[FOUR_Liver_Score_avg$STUDYID == unique(master_compiledata$STUDYID)] <- calculated_LB_value

      }
    }, error = function(e) {
      # handle error properly
    })
    #<><><><><><><><><><><><><><><><><><>"""MI"""" zscoring <><><><><><><><><><><>
    tryCatch({

      if(output_individual_scores ){
        # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
        studyid <- if (use_xpt_file) NULL else studyid
        mi_score_final_list_df <- get_mi_score(studyid = studyid,
                                               path_db = path_db,
                                               fake_study = fake_study,
                                               use_xpt_file = use_xpt_file,
                                               master_compiledata = master_compiledata ,
                                               return_individual_scores = TRUE,
                                               return_zscore_by_USUBJID = FALSE)
        master_mi_df <- dplyr::bind_rows(master_mi_df, mi_score_final_list_df)

      } else if (output_zscore_by_USUBJID) {
        studyid <- if (use_xpt_file) NULL else studyid
        MI_score_by_USUBJID_HD <-get_mi_score(studyid = studyid,
                                              path_db = path_db,
                                              fake_study = fake_study,
                                              use_xpt_file = use_xpt_file,
                                              master_compiledata = master_compiledata ,
                                              return_individual_scores = FALSE,
                                              return_zscore_by_USUBJID = TRUE)

        mi_study_identifier <- unique(MI_score_by_USUBJID_HD$STUDYID)
        master_mi_score[[as.character(mi_study_identifier)]] <- MI_score_by_USUBJID_HD

      } else{
        # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
        studyid <- if (use_xpt_file) NULL else studyid

        averaged_MI_score <- get_mi_score(studyid = studyid,
                                          path_db = path_db,
                                          fake_study = fake_study,
                                          use_xpt_file = use_xpt_file,
                                          master_compiledata = master_compiledata ,
                                          return_individual_scores = FALSE,
                                          return_zscore_by_USUBJID = FALSE)
        # Extract the "LB_score"MI_score_avg" value for the current STUDYID from
        calculated_MI_value <- averaged_MI_score$MI_score_avg[averaged_MI_score$STUDYID == unique(master_compiledata$STUDYID)]

        # Update the LB_score value in FOUR_Liver_Score for the current STUDYID
        FOUR_Liver_Score_avg$MI_score_avg[FOUR_Liver_Score_avg$STUDYID == unique(master_compiledata$STUDYID)] <- calculated_MI_value

      }
    }, error = function(e) {
      # handle error properly
    })
  }
  if (output_individual_scores) {
    # Perform the merge using full_join to keep all rows from each data frame
    combined_output_individual_scores <- master_liverToBW %>%
      dplyr::full_join(master_lb_score_six, by = "STUDYID") %>%
      dplyr::full_join(master_mi_df, by = "STUDYID")
  } else if (output_zscore_by_USUBJID ) {
    combined_liverToBW <- dplyr::bind_rows(master_liverToBW)
    combined_lb_score <- dplyr::bind_rows(master_lb_score)
    combined_mi_score <- dplyr::bind_rows(master_mi_score)
    # Merge the first two data frames (df_liverToBW and df_LB) on STUDYID and USUBJID
    combined_df <-  combined_liverToBW %>%
      dplyr::full_join(combined_lb_score, by = c("STUDYID", "USUBJID"))

    # Merge the result with the third data frame (df_mi) on STUDYID and USUBJID
    final_output_zscore_by_USUBJID <- combined_df %>%
      dplyr::full_join( combined_mi_score, by = c("STUDYID", "USUBJID"))
  } else {
    FOUR_Liver_Score_avg <- FOUR_Liver_Score_avg
    # Round all columns from the second column onward to two decimal places
    FOUR_Liver_Score_avg[, 2:ncol(FOUR_Liver_Score_avg)] <- round(FOUR_Liver_Score_avg[, 2:ncol(FOUR_Liver_Score_avg)], 2)
  }
  if (output_individual_scores) {
    return(combined_output_individual_scores)
  } else if(output_zscore_by_USUBJID) {
    return(final_output_zscore_by_USUBJID)
  } else {
    return(FOUR_Liver_Score_avg)
  }
}
