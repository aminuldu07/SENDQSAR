rm(list = ls())
devtools::load_all(".")

###---------
all_testcd_zscore <- get_all_lb_TESTCD_zscore(studyid = '2170016',
                                  path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db',
                                  fake_study= FALSE,
                                  use_xpt_file = FALSE,
                                  master_compiledata = NULL,
                                  return_individual_scores = TRUE)
# Calculate row means from the second to the last column
avg_all_testcd_zscore <- all_testcd_zscore %>%
  dplyr::mutate(avg_all_zscore = rowMeans(.[, 2:ncol(.)], na.rm = TRUE))
# Print the avg_all_zscore column
print(avg_all_testcd_zscore$avg_all_zscore)

# Calculate row means from the second to the last column, ignoring zeros
avg2_all_testcd_zscore <- all_testcd_zscore %>%
  dplyr::mutate(
    avg_all_zscore = rowMeans(
      dplyr::select(., 2:ncol(.)) %>%
        dplyr::mutate(across(everything(), ~ ifelse(. == 0, NA, .))),  # Replace 0 with NA
      na.rm = TRUE
    )
  )

# Print the avg_all_zscore column
print(avg_all_testcd_zscore$avg_all_zscore)
print(avg2_all_testcd_zscore$avg_all_zscore)


# Calculate row means from the second to the last column
solo_all_testcd_zscore <- final_results_solo %>%
  dplyr::mutate(solo_avg_all_zscore = rowMeans(.[, 2:ncol(.)], na.rm = TRUE))

# Calculate row means from the second to the last column, ignoring zeros
solo2_all_testcd_zscore <- final_results_solo %>%
  dplyr::mutate(
    solo_avg_all_zscore = rowMeans(
      dplyr::select(., 2:ncol(.)) %>%
        dplyr::mutate(across(everything(), ~ ifelse(. == 0, NA, .))),  # Replace 0 with NA
      na.rm = TRUE
    )
  )
print(solo_all_testcd_zscore$solo_avg_all_zscore)
print(solo2_all_testcd_zscore$solo_avg_all_zscore)


averaged_all_testcd_zscore <- get_all_lb_TESTCD_zscore(studyid = '2170016',
                                              path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db',
                                              fake_study= FALSE,
                                              use_xpt_file = FALSE,
                                              master_compiledata = NULL,
                                              return_individual_scores = FALSE)





#########################################
#studyids <- c('2170016', '876')
# Define the study IDs
studyids <- c('2170016','20234696')

# Initialize an empty data frame to store the final results
final_testresults <- NULL

# Loop through each study ID
for (studyid in studyids) {
  # Get the z-score data for the current study ID
all_testcd_zscore <- get_all_lb_TESTCD_zscore(studyid = studyid ,
                                              path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db',
                                              fake_study= FALSE,
                                              use_xpt_file = FALSE,
                                              master_compiledata = NULL,
                                              return_individual_scores = TRUE)

# Combine the results from each iteration
final_testresults <- dplyr::bind_rows(final_testresults,all_testcd_zscore )
}
