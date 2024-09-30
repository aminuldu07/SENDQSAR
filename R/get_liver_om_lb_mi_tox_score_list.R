



get_liver_om_lb_mi_tox_score_list <- function (selected_studies,
                                               path_db,
                                               fake_study = FALSE,
                                               use_xpt_file = FALSE,
                                               output_individual_scores = FALSE) {

if(output_individual_scores ) {
  # master liverToBW_df
  master_liverToBW <-  data.frame(STUDYID = NULL, avg_liverToBW_zscore = NULL)

  master_mi_df <- data.frame()

  # Master LB list
  master_lb_score_six <- data.frame(STUDYID = NULL, avg_alb_zscore = NULL, avg_ast_zscore = NULL, avg_alp_zscore = NULL,
                                    avg_alt_zscore = NULL, avg_bili_zscore = NULL, avg_ggt_zscore = NULL)

  # Create FOUR SCORE DATA FRAME for "LiverToBodyweight" , "LB" & "MI" Score
  FOUR_Liver_Score <-  data.frame(STUDYID = NA, liverToBW = NA, LB_score = NA, MI_score = NA)

  # Initialize an empty data frame to store the names of studies with errors
  Error_studies <- list()

  # Initialize the master error data frame to have the details of the errors
  #master_error_df <- data.frame(STUDYID = character() , Block = character(), ErrorMessage = character(), Time = POSIXct(), stringsAsFactors = FALSE)
  master_error_df <- data.frame(STUDYID = character() ,
                                Block = character(),
                                ErrorMessage = character(),
                                #Time = POSIXct(),
                                stringsAsFactors = FALSE)


} else {
  # Create FOUR SCORE DATA FRAME for "LiverToBodyweight" , "LB" & "MI" Score
  FOUR_Liver_Score_avg <-  data.frame(STUDYID = NA, BWzScore_avg = NA, liverToBW_avg = NA, LB_score_avg = NA, MI_score_avg = NA)

  # Initialize an empty data frame to store the names of studies with errors
  Error_studies <- list()

  # Initialize the master error data frame to have the details of the errors
  #master_error_df <- data.frame(STUDYID = character() , Block = character(), ErrorMessage = character(), Time = POSIXct(), stringsAsFactors = FALSE)
  master_error_df <- data.frame(STUDYID = character() ,
                                Block = character(),
                                ErrorMessage = character(),
                                #Time = POSIXct(),
                                stringsAsFactors = FALSE)
}

for (studyid in selected_studies){

  print(studyid)

  # Initialize a flag variable at the start of each iteration
  first_block_success <- TRUE

  # First Block with its own tryCatch for master_compiledata~~~~~~~~~~~~~~~~~~
  tryCatch({
    # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
    studyid <- if (use_xpt_file) NULL else studyid

     # if use_xpt_file = TRUE,studyid should be NULL..........................
    # Call "get_liver_compiledata" function to get the master_compiledata
    output_get_compile_data <- get_compile_data(studyid = studyid ,
                                                path_db = path_db,
                                                fake_study = fake_study,
                                                use_xpt_file = use_xpt_file)

    #return as.data.frame "master_compiledata"
    #master_compiledata

    # GET the  "master_compiledata" -data frame- from the output of the --
    master_compiledata <- output_get_compile_data

    # Create a copy of master_compiledata for the diagnostic purpose
    master_compiledata_copy <- master_compiledata

    }, error = function(e) {
    # Handling errors
    message("Error in BodyWeight Data Compilation calculation: ", e$message)

    # Log the error
    error_block1 <- data.frame(STUDYID = studyid,
                               Block = "compiledata",
                               ErrorMessage = e$message,
                               #Time = Sys.time(),
                               stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block1)
    #master_error_df <<- rbind(master_error_df, error_block1)

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

  #----------------------score_accumulation_df--------------------------------
  #This block for "Adding a new row for the current STUDYID in FOUR_Liver_Score"
  tryCatch({

    # Initialize the "FOUR_Liver_Score"
    # [[# Add a new row for the current STUDYID in FOUR_Liver_Score]]

    if(output_individual_scores){

    new_row_in_four_liver_scr <- data.frame(STUDYID = unique(master_compiledata$STUDYID),
                                            liverToBW = NA,
                                            LB_score = NA,
                                            MI_score = NA)

    FOUR_Liver_Score <- rbind(FOUR_Liver_Score, new_row_in_four_liver_scr)

    } else {
      new_row_in_four_liver_scr_avg <- data.frame(STUDYID = unique(master_compiledata$STUDYID),
                                              BWzScore_avg = NA,
                                              liverToBW_avg = NA,
                                              LB_score_avg = NA,
                                              MI_score_avg = NA)

      FOUR_Liver_Score_avg <- rbind(FOUR_Liver_Score_avg, new_row_in_four_liver_scr_avg)
    }

  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in FOUR_Liver_Score: ", e$message)

    # Log the error
    error_block_flscrdf <- data.frame(STUDYID = studyid,
                                      Block = "FOUR_Liver_Score",
                                      ErrorMessage = e$message,
                                      #Time = Sys.time(),
                                      stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block_flscrdf)

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
                                 master_compiledata = master_compiledata ,
                                 return_individual_scores = TRUE)
    } else {
      # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
      studyid <- if (use_xpt_file) NULL else studyid

      averaged_HD_BWzScore <- get_bw_score (studyid = studyid,
                                            path_db = path_db,
                                            fake_study = fake_study,
                                            use_xpt_file = use_xpt_file,
                                            master_compiledata = master_compiledata ,
                                            return_individual_scores = FALSE)

      # # Add the liverToBW_zscore to "FOUR_Liver_Score" data frame................

      # Extract the liverToBW value for the current STUDYID from liverToBW_df
      calculated_BWzScore_value <-  averaged_HD_BWzScore$BWzScore_avg[averaged_HD_BWzScore$STUDYID == unique(master_compiledata$STUDYID)]
      #calculated_liverToBW_value <- liverToBW_df$liverToBW_avg

      # Update the liverToBW value in FOUR_Liver_Score_avg for the current STUDYID
      FOUR_Liver_Score_avg$BWzScore_avg[FOUR_Liver_Score_avg$STUDYID == unique(master_compiledata$STUDYID)] <- calculated_BWzScore_value

    }

  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in BodyWeight_zScore calculation: ", e$message)

    # Log the error
    error_block2 <- data.frame(STUDYID = studyid,
                               Block = "BWZscore",
                               ErrorMessage = e$message,
                               #Time = Sys.time(),
                               stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block2)

  })

#---------------------------"OM_DATA"-(Liver_Organ to Body Weight zScore)-------
  tryCatch({
    # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
    studyid <- if (use_xpt_file) NULL else studyid

    if(output_individual_scores){
      HD_liver_zscore_df <- get_liver_livertobw_score (studyid = studyid,
                                                       path_db = path_db,
                                                       fake_study = fake_study,
                                                       use_xpt_file = use_xpt_file,
                                                       master_compiledata = master_compiledata,
                                                       bwzscore_BW = bwzscore_BW ,
                                                       return_individual_scores = TRUE)

      master_liverToBW <- rbind(master_liverToBW, HD_liver_zscore_df )

    } else {
      # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
      studyid <- if (use_xpt_file) NULL else studyid

      if (is.null(bwzscore_BW)) {
      bwzscore_BW <- get_bw_score (studyid = studyid,
                                   path_db = path_db,
                                   fake_study = fake_study,
                                   use_xpt_file = use_xpt_file,
                                   master_compiledata = master_compiledata ,
                                   return_individual_scores = TRUE)
      }
      averaged_liverToBW_df <- get_liver_livertobw_score (studyid, path_db,
                                                       fake_study = fake_study,
                                                       master_compiledata = master_compiledata,
                                                       bwzscore_BW = bwzscore_BW ,
                                                       return_individual_scores = FALSE)
      # {{{{...... N.B. Here is special case, if we use bwzscore_BW in else
      # condition from the previous step, it will provide
      # the  "1x2" STUDYID & avg_bwscore df but we need
      # full  bwzscore_BW. if NULL,  bwzscore_BW will be calcualted...}}}}


      # # Add the liverToBW_zscore to "FOUR_Liver_Score" data frame................
      # Create "liverToBW_df" for FOUR_Liver_Score_avg
      liverToBW_df <- averaged_liverToBW_df  %>%
        dplyr::rename(liverToBW_avg = avg_liverToBW_zscore)

            # Extract the liverToBW value for the current STUDYID from liverToBW_df
      calculated_liverToBW_value <- liverToBW_df$liverToBW_avg[liverToBW_df$STUDYID == unique(master_compiledata$STUDYID)]
      #calculated_liverToBW_value <- liverToBW_df$liverToBW_avg
      # Update the liverToBW value in FOUR_Liver_Score_avg for the current STUDYID
      print(calculated_liverToBW_value)
      FOUR_Liver_Score_avg$liverToBW_avg[FOUR_Liver_Score_avg$STUDYID == unique(master_compiledata$STUDYID)] <- calculated_liverToBW_value

      # add liverToBW_df to master_liverToBW
      #master_liverToBW <- dplyr::bind_rows(master_liverToBW, liverToBW_df)

    }

  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in Liver_Organ to Body Weight zScore: ", e$message)

    # Log the error
    error_block3 <- data.frame(STUDYID = studyid,
                               Block = "LiverToBW",
                               ErrorMessage = e$message,
                               #Time = Sys.time(),
                               stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block3)
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
                                     return_individual_scores = TRUE)


    master_lb_score_six <- rbind(master_lb_score_six , master_lb_scores)

    } else {
      # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
      studyid <- if (use_xpt_file) NULL else studyid

      averaged_LB_score <- get_lb_score(studyid = studyid,
                                        path_db = path_db,
                                        fake_study= fake_study,
                                        use_xpt_file = use_xpt_file ,
                                        master_compiledata = master_compiledata,
                                        return_individual_scores = FALSE)

        # Append the LB_zscore to the "FOUR_Liver_Score_avg" data frame
        # Extract the LB_score value for the current STUDYID from LB_df
        calculated_LB_value <- averaged_LB_score$LB_score_avg[ averaged_LB_score$STUDYID == unique(master_compiledata$STUDYID)]

        # Update the LB_score value in FOUR_Liver_Score for the current STUDYID
        FOUR_Liver_Score_avg$LB_score_avg[FOUR_Liver_Score_avg$STUDYID == unique(master_compiledata$STUDYID)] <- calculated_LB_value


        #master_lbxx_list[[j]] <- lb_score_final_list
        #master_lb_score_six <- rbind(master_lb_score_six , averaged_LB_score)

     }

  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in LB zscoring: ", e$message)

    # Log the error
    error_block4 <- data.frame(STUDYID = studyid, Block = "LB",
                               ErrorMessage = e$message,
                               #Time = Sys.time(),
                               stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block4)
  })

  #<><><><><><><><><><><><><><><><><><>"""MI"""" zscoring <><><><><><><><><><><>
  tryCatch({

    #mi_score_final_list <- get_liver_mi_score(j, dbtoken, ts, master_compiledata)

    #master_mixx_list[[j]] <- mi_score_final_list

   if(output_individual_scores ){

     # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
     studyid <- if (use_xpt_file) NULL else studyid

     mi_score_final_list_df <- get_mi_score(studyid = studyid,
                                            path_db = path_db,
                                            fake_study = fake_study,
                                            use_xpt_file = use_xpt_file,
                                            master_compiledata = master_compiledata ,
                                            return_individual_scores = TRUE)

     master_mi_df <- dplyr::bind_rows(master_mi_df, mi_score_final_list_df)

    #master_mi_df <- dplyr::bind_rows(master_mi_df, mi_score_final_list_df)

   } else {
     # Set 'studyid' to NULL if using an XPT file, otherwise keep the original value.
     studyid <- if (use_xpt_file) NULL else studyid

     averaged_MI_score <- get_mi_score(studyid = studyid,
                                       path_db = path_db,
                                       fake_study = fake_study,
                                       use_xpt_file = use_xpt_file,
                                       master_compiledata = master_compiledata ,
                                       return_individual_scores = FALSE)

     # Append the "MI_zscore"MI_score_avg" to the "FOUR_Liver_Score_avg" data frame
     # Extract the "LB_score"MI_score_avg" value for the current STUDYID from
     calculated_MI_value <- averaged_MI_score$MI_score_avg[averaged_MI_score$STUDYID == unique(master_compiledata$STUDYID)]

     # Update the LB_score value in FOUR_Liver_Score for the current STUDYID
     FOUR_Liver_Score_avg$MI_score_avg[FOUR_Liver_Score_avg$STUDYID == unique(master_compiledata$STUDYID)] <- calculated_MI_value

    }


  }, error = function(e) {
    # Handling errors of the secondary operation

    # Log the error
    error_block5 <- data.frame(STUDYID = studyid, Block = "MI",
                               ErrorMessage = e$message,
                               #Time = Sys.time(),
                               stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block5)

    # Create MI_final_score with NA values
    #return(data.frame(STUDYID = NA, avg_MI_score = NA))
  })

}


#####
# if (output_individual_scores ) {
#
# } else  {
#
#   FOUR_Liver_Score_avg <- FOUR_Liver_Score_avg [-1,]
#   # # Reassigned the variable
#   # liver_scored_Four_Liver_Score <- FOUR_Liver_Score
#   #
#   # # Create averaged_liver_score column for un_scored columns
#   # liver_scored_Four_Liver_Score <- liver_scored_Four_Liver_Score %>%
#   #   dplyr::mutate(averaged_liver_score = rowMeans(select(., liverToBW, LB_score, MI_score), na.rm = TRUE))
#   #
#   #
#   # # Create scored_averaged_liver_score column for scored columns
#   # liver_scored_Four_Liver_Score <- liver_scored_Four_Liver_Score %>%
#   #   dplyr::mutate(scored_averaged_liver_score = rowMeans(dplyr::select(.,MI_score, scored_liverToBW, scored_LBScore), na.rm = TRUE))
#   #
#   # # remove NAs from "scored_averaged_liver_score" column
#   # final_liver_scored_Four_Liver_Score <- liver_scored_Four_Liver_Score %>%
#   #                dplyr::filter(!is.na(scored_averaged_liver_score))
#   #.........................................................................................................................
# }
##########
   if (output_individual_scores) {
    return(list(master_liverToBW = master_liverToBW,
              master_lb_score_six = master_lb_score_six,
              master_mi_df  = master_mi_df,
              Error_studies =  Error_studies,
              master_error_df = master_error_df))
   } else {
   return(FOUR_Liver_Score_avg)
  }


}


