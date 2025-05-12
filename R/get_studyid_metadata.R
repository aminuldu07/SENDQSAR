





get_studyid_metadata <- function(input_df) {


  # Check if input is a data frame
  if (!is.data.frame(input_df)) {
    stop("Input must be a data frame.")
  }

  # Check for STUDYID column
  if (!"STUDYID" %in% colnames(input_df)) {
    stop("Input data frame must contain a 'STUDYID' column.")
  }

  # Create studyid_metadata with STUDYID and Target_Organ columns
  studyid_metadata <- data.frame(STUDYID = input_df$STUDYID)
  n_rows <- nrow(studyid_metadata)
  half_n <- ceiling(n_rows / 2)

  studyid_metadata$Target_Organ <- c(rep("Liver", half_n),
                                     rep("not_Liver", n_rows - half_n))

  return(studyid_metadata)
}
