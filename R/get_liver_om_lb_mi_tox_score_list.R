## This code is for getting liver toxicity score from SEND Studyid

## -----------------------------------------------------------------------------
##   Date                     Programmer
## ----------   --------------------------------------------------------------
##   May-07-2024    Md MD Aminul Islam Prodhan (mdaminulislam.prodhan@fda.hhs.gov)

selected_studies <-  "studyid in selected_studies" #{slected studies should be a vector}


#get_liver_om_lb_mi_tox_score_list <- function (selected_studies, dbtoken ) {
get_liver_om_lb_mi_tox_score_list <- function (selected_studies,
                                               path_db,
                                               fake_study = FALSE,
                                               #master_compiledata = NULL,
                                               #bwzscore_BW = NULL,
                                               SCORE_IN_LIST_FORMAT = FALSE) {

# master liverToBW_df
master_liverToBW <-  data.frame(STUDYID = NULL, avg_liverToBW_zscore = NULL)

master_mi_df <- data.frame()

# Master LB list
master_lb_score_six <- data.frame(STUDYID = NULL, avg_alb_zscore = NULL, avg_ast_zscore = NULL, avg_alp_zscore = NULL,
                             avg_alt_zscore = NULL, avg_bili_zscore = NULL, avg_ggt_zscore = NULL)

# Create FOUR SCORE DATA FRAME for "LiverToBodyweight" , "LB" & "MI" Score
FOUR_Liver_Score <-  data.frame(STUDYID = NA, liverToBW = NA, LB_score = NA, MI_score = NA, scored_liverToBW = NA, scored_LBScore = NA)


# Initialize an empty data frame to store the names of studies with errors
Error_studies <- list()

# Initialize the master error data frame to have the details of the errors
#master_error_df <- data.frame(STUDYID = character() , Block = character(), ErrorMessage = character(), Time = POSIXct(), stringsAsFactors = FALSE)
master_error_df <- data.frame(STUDYID = character() ,
                              Block = character(),
                              ErrorMessage = character(),
                              #Time = POSIXct(),
                              stringsAsFactors = FALSE)
#for (j in selected_studies){

for (studyid in selected_studies){

  print(studyid)

  # Initialize a flag variable at the start of each iteration
  first_block_success <- TRUE

  # First Block with its own tryCatch for master_compiledata~~~~~~~~~~~~~~~~~~
  tryCatch({

    # Call "get_liver_compiledata" function to get the cleaned_compiledata
    output_get_compile_data <- get_compile_data(studyid, path_db, fake_study = FALSE) #return as.data.frame "master_compiledata"
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
    Error_studies <<- c(Error_studies, studyid)

    next
  }

  #-----------------end of master_compiledata calculation----------------------

  #----------------------score_accumulation_df--------------------------------
  #This block for "Adding a new row for the current STUDYID in FOUR_Liver_Score"
  tryCatch({

    # Initialize the "FOUR_Liver_Score"
    # [[# Add a new row for the current STUDYID in FOUR_Liver_Score]]


    new_row_in_four_liver_scr <- data.frame(STUDYID = unique(master_compiledata$STUDYID),
                                            liverToBW = NA,
                                            LB_score = NA,
                                            MI_score = NA,
                                            scored_liverToBW = NA,
                                            scored_LBScore = NA)

    FOUR_Liver_Score <- rbind(FOUR_Liver_Score, new_row_in_four_liver_scr)
    FOUR_Liver_Score <- FOUR_Liver_Score[-1,] # remove the first column

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

    # BWzScore_vehicle_plus_highdose <-  get_liver_bw_score(studyid, bw, ts,
    #                                                       master_compiledata,
    #                                                            tK_animals_df )
    #' @here-master_compiledata-can-be-worked-on-to-be-incorporated-in-argument
    bwzscore_BW <- get_bw_score (studyid, path_db, fake_study = FALSE, master_compiledata = master_compiledata , score_in_list_format = TRUE)


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
    if(SCORE_IN_LIST_FORMAT == FALSE){
    final_liverToBW_df <- get_liver_livertobw_score (studyid, path_db,
                                                     fake_study = FALSE,
                                                     master_compiledata = master_compiledata,
                                                     bwzscore_BW = bwzscore_BW ,
                                                     score_in_list_format = FALSE)

    print(str(final_liverToBW_df))

      # # Add the liverToBW_zscore to "FOUR_Liver_Score" data frame................
      # Create "liverToBW_df" for FOUR_Liver_Score
      liverToBW_df <- final_liverToBW_df %>%
                                           dplyr::rename(liverToBW = avg_liverToBW_zscore)

      # add liverToBW_df to master_liverToBW
      master_liverToBW <- dplyr::bind_rows(master_liverToBW, liverToBW_df)

      # Extract the liverToBW value for the current STUDYID from liverToBW_df
      calculated_liverToBW_value <- liverToBW_df$liverToBW[liverToBW_df$STUDYID == unique(master_compiledata$STUDYID)]

      # Update the liverToBW value in FOUR_Liver_Score for the current STUDYID
      FOUR_Liver_Score$liverToBW[FOUR_Liver_Score$STUDYID == unique(master_compiledata$STUDYID)] <- calculated_liverToBW_value

    } else {
      final_liverToBW_df <- get_liver_livertobw_score (studyid, path_db,
                                                       fake_study = FALSE,
                                                       master_compiledata = master_compiledata,
                                                       bwzscore_BW = bwzscore_BW ,
                                                       score_in_list_format = TRUE)

      #master_liverToBW <- rbind(master_liverToBW, final_liverToBW_df)

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
    if(SCORE_IN_LIST_FORMAT == FALSE){
    master_lb_scores <- get_lb_score(studyid,
                                     path_db,
                                     fake_study= FALSE,
                                     master_compiledata = master_compiledata,
                                     score_in_list_format = FALSE)

    #master_lbxx_list[[j]] <- lb_score_final_list
    master_lb_score_six <- rbind(master_lb_score_six ,master_lb_scores)

    } else {
      master_lb_scores <- get_lb_score(studyid,
                                       path_db,
                                       fake_study= FALSE,
                                       master_compiledata = master_compiledata,
                                       score_in_list_format = TRUE)

      #master_lbxx_list[[j]] <- lb_score_final_list
      master_lb_score_six <- rbind(master_lb_score_six ,master_lb_scores)

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
   if(SCORE_IN_LIST_FORMAT == FALSE){
    mi_score_final_list_df <- get_mi_score(studyid,
                               path_db,
                               fake_study = FALSE,
                               master_compiledata = master_compiledata ,
                               score_in_list_format = FALSE)

    print(mi_score_final_list_df)
    print("this part is okay")

    #master_mi_df <- dplyr::bind_rows(master_mi_df, mi_score_final_list_df)

   } else {
      mi_score_final_list_df <- get_mi_score(studyid,
                                             path_db,
                                             fake_study = FALSE,
                                             master_compiledata = master_compiledata ,
                                             score_in_list_format = TRUE)

      master_mi_df <- dplyr::bind_rows(master_mi_df, mi_score_final_list_df)

    }
    #master_mi_df <- rbind(master_mi_df, mi_score_final_list_df)

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
# write.csv(master_liverToBW, "dc_7396_master_livertobw_scores_df.csv", row.names = FALSE)
# write.csv(master_lb_score_six, "dc_7396_master_lb_scores_df.csv", row.names = FALSE)
# write.csv(master_mi_df , "dc_7396_master_mi_scores_df.csv", row.names = FALSE)

# Debugging: list variables in the function environment
#print(ls(envir = environment()))


return(list(master_liverToBW = master_liverToBW,
            master_lb_score_six = master_lb_score_six,
            master_mi_df  = master_mi_df,
            master_error_df = master_error_df
            ))

}


