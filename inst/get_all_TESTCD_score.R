rm(list = ls())
devtools::load_all(".")

#
# library(tidyr)
# library(dplyr)
# library(data.table)
# library(readxl)
# library(sendigR)

studyid <- c('2170016')
path_db <- 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db'
path <- path_db

# Helper function to fetch data from SQLite database
fetch_domain_data <- function(db_connection, domain_name, studyid) {
  domain_name <- toupper(domain_name)
  query_statement <- paste0('SELECT * FROM ', domain_name, " WHERE STUDYID = :x")
  query_result <- DBI::dbGetQuery(db_connection, statement = query_statement, params = list(x = studyid))
  query_result
}

# Establish a connection to the SQLite database
db_connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)

# Fetch data for required domains
lb <- fetch_domain_data(db_connection, 'lb', studyid)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~LB_DATA-processing~~~~~~~~~~~~~~~~~~~~~~~~~~~

LB_selected_columns <-  lb[ , c("STUDYID", "DOMAIN", "USUBJID", "LBTESTCD",
"LBTEST", "LBCAT", "LBORRES", "LBORRESU", "LBSTRESC", "LBSTRESN", "LBSTRESU", "LBSPEC", "VISITDY", "LBDY")]

#cleaning the "LBSTRESN" column
# convert the "LBSTRESN" column to numeric
LB_selected_columns$LBSTRESN <- as.numeric(LB_selected_columns$LBSTRESN)

# Remove rows with NA  values in the LBSTRESN column
LB_selected_columns <- LB_selected_columns[!is.na(LB_selected_columns$LBSTRESN),]

# Remove rows with  "o" values in the LBSTRESN column
LB_selected_columns <- LB_selected_columns[!(LB_selected_columns$LBSTRESN) == 0,]


#save(LB_selected_columns, file = "LB_selected_columns.RData")

#Make LB Data Data Frame to Hold Information
LBData <- data.frame("STUDYID" = NA,"USUBJID" = NA,"LBSPEC" = NA,"LBTESTCD" = NA,
                     "LBSTRESN" = NA, "VISITDY" = NA)

# for (Name in unique(filtered_combined_lb$STUDYID)) {
# Filter the data for the current STUDYID
study_data_LB <- LB_selected_columns

# Check if LBDY column exists and process accordingly
if ("LBDY" %in% names(study_data_LB)) {
  LBD <- study_data_LB %>%
    dplyr::filter(LBDY >= 1) %>%
    dplyr::select(STUDYID,USUBJID, LBSPEC, LBTESTCD, LBSTRESN, LBDY)

  colnames(LBD) <- c("STUDYID", "USUBJID", "LBSPEC", "LBTESTCD", "LBSTRESN", "VISITDY")

  # Convert LBCAT to LBSPEC if LBSPEC is NA
  if (all(is.na(LBD$LBSPEC))) {
    LBD$LBSPEC <- study_data_LB$LBCAT[study_data_LB$LBDY >= 1]

    if (any(c("HEMATOLOGY", "Hematology","hematology") %in% levels(LBD$LBSPEC))){
      levels(LBD$LBSPEC)[match(c("HEMATOLOGY", "Hematology","hematology"),
                               levels(LBD$LBSPEC))] <- "WHOLE BLOOD"
    }
    if (any(c("CLINICAL CHEMISTRY","Clinical Chemistry") %in% levels(LBD$LBSPEC))){
      levels(LBD$LBSPEC)[match(c("CLINICAL CHEMISTRY","Clinical Chemistry"),
                               levels(LBD$LBSPEC))] <- "SERUM"
    }
    if (any(c("URINALYSIS","Urinalysis") %in% levels(LBD$LBSPEC))){
      levels(LBD$LBSPEC)[match(c("URINALYSIS","Urinalysis"),
                               levels(LBD$LBSPEC))] <- "URINE"
    }
  }
} else {
  # If LBDY column does not exist, handle accordingly
  LBD <- study_data_LB[which(study_data_LB$VISITDY >= 1),
                       c("STUDYID","USUBJID","LBSPEC","LBTESTCD","LBSTRESN", "VISITDY")]
}

# Add to LBData
LBData <- rbind(LBData, LBD)

# Remove rows with all NAs
LBData <- stats::na.omit(LBData)

# Concatenate LBSPEC and LBTESTCD
#LBData$LBTESTCD <- paste(LBData$LBSPEC, LBData$LBTESTCD, sep = ' | ')
# This step remove not rows matching test from ogransystem
#test_cleaned_LBData <- LBData[LBData$LBTESTCD %in% organTESTCDlist[['LIVER']],]

test_cleaned_LBData <- LBData

# Create a new data frame with the row having the max VISITDY for each USUBJID and LBTESTCD combination
max_visitdy_df <- test_cleaned_LBData %>%
  dplyr::group_by(USUBJID, LBTESTCD) %>%
  dplyr::filter(VISITDY == max(VISITDY, na.rm = TRUE)) %>%
  dplyr::ungroup()

#<><><><><><><><><><><><><><><><>... Remove TK animals and Recovery animals......<><><><><><>.............
#<><><><><><><><> master_compiledata is free of TK animals and Recovery animals<><><><><><><><><><><><><><>
master_compiledata <- get_compile_data(studyid = studyid, path_db = path_db,
                                       fake_study = FALSE,
                                       use_xpt_file = FALSE )

# Remove the TK animals and Recovery animals
LB_tk_recovery_filtered <- max_visitdy_df %>%
  dplyr::filter(USUBJID %in% master_compiledata$USUBJID)

# Perform a left join to match USUBJID and get ARMCD ## 020924
#-inner_join() used instead of left_join()#199
LB_tk_recovery_filtered_ARMCD <- LB_tk_recovery_filtered %>%
  dplyr::inner_join(master_compiledata %>%
                      dplyr::select(USUBJID, ARMCD), by = "USUBJID")

###---zscore calculate for each of the 'lbtestcd'---------------------------
#lbtestcd_list <- c("BILI", "ALB", "AST")

lbtestcd_list <- unique(max_visitdy_df$LBTESTCD)

# Initialize an empty list to store the results
#results_list <- list()

# Initialize an empty data frame for the final results
final_results <- NULL

for (lbtestcd in lbtestcd_list) {
  # Filter for the current LBTESTCD
print(lbtestcd)
  filtered_df <- LB_tk_recovery_filtered_ARMCD %>%
    dplyr::filter(LBTESTCD == lbtestcd)

  # Calculate the z-score for the current LBTESTCD
  zscore_df <- filtered_df %>%
    dplyr::group_by(STUDYID) %>%
    dplyr::mutate(
      mean_vehicle = mean(LBSTRESN[ARMCD == "vehicle"], na.rm = TRUE),
      sd_vehicle = sd(LBSTRESN[ARMCD == "vehicle"], na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      zscore = (LBSTRESN - mean_vehicle) / sd_vehicle,
      zscore = abs(zscore)
    )

  # Average z-score per STUDYID for all subjects with the current LBTESTCD
  final_zscore <- zscore_df %>%
    dplyr::filter(ARMCD == "HD") %>%  # Filter for HD
    dplyr::group_by(STUDYID) %>%  # Group by STUDYID
    dplyr::summarise(
      avg_zscore = mean(zscore, na.rm = TRUE),  # Average z-score
      LBTESTCD = dplyr::first(LBTESTCD)  # Include LBTESTCD in the summarized data
    ) %>%
    dplyr::mutate(avg_zscore = ifelse(avg_zscore >= 3, 3,
                                      ifelse(avg_zscore >= 2, 2,
                                             ifelse(avg_zscore >= 1, 1, 0))))




  # Dynamically rename the avg_zscore column to aveg_lbtestcd
  final_zscore <- final_zscore %>%
    dplyr::rename(!!glue::glue("avg_{lbtestcd}") := avg_zscore) %>%
    dplyr::select(STUDYID, !!glue::glue("avg_{lbtestcd}"))

  # If final_results is NULL (first iteration), set it to final_zscore
  if (is.null(final_results)) {
    final_results <- final_zscore
  } else {
    # Otherwise, join with the existing final_results
    final_results <- dplyr::full_join(final_results, final_zscore, by = "STUDYID")
  }
}

# Combine the results into a single data frame
#final_results <- do.call(rbind, results_list)










