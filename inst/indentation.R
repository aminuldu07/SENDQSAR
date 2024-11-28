if (ncol(mi_CompileData) > 6) {

  # Calculate Incidence per group for MI Data
  MIIncidencePRIME <- merge(MIIncidencePRIME, unique(mi_CompileData[, c("STUDYID", "USUBJID", "ARMCD")]), by = c("USUBJID"))

  # Get the name of the columns of "MIIncidencePRIME"
  column_MIIncidencePRIME <- data.frame(names(MIIncidencePRIME)) # column names of MIIncidencePRIME

  # Directly subset the data frame
  MIIncidence <- MIIncidencePRIME[, c("STUDYID", "USUBJID", "MISTRESC", "ARMCD")]

  test_MIIncidence <- MIIncidence

  GroupIncid <- data.frame(Treatment = NA,
                           Sex = NA,
                           Finding = NA,
                           Count = NA)

  # Iterate over sex categories
  for (sex in c('M', 'F')) {

    # Filter data for the current study
    StudyMI <- MIIncidence

    # "StudyGroupIncid" data frame creation
    StudyGroupIncid <- data.frame(Treatment = NA,
                                  Sex = NA,
                                  Finding = NA,
                                  Count = NA)

    # Filter data for the current sex
    sexSubjects <- mi_CompileData$USUBJID[which(mi_CompileData$SEX == sex)]
    sexIndex <- which(StudyMI$USUBJID %in% sexSubjects)
    StudyMI <- StudyMI[sexIndex, ]

    # Iterate over unique treatment arms (ARMCD)
    for (dose in unique(StudyMI$ARMCD)) {

      doseMI <- StudyMI[which(StudyMI$ARMCD == dose), ]

      # Calculate the incidence for each finding
      Incid <- data.frame(table(toupper(doseMI$MISTRESC)) / length(unique(doseMI$USUBJID)))

      names(Incid)[2] <- "Count"
      names(Incid)[1] <- "Finding"
      Incid$Treatment <- paste0(unique(unique(StudyMI$STUDYID)), " ", dose)
      Incid$Sex <- sex

      StudyGroupIncid <- rbind(StudyGroupIncid, Incid)
    }

    # Removing of Vehicle Baseline
    for (finding in unique(StudyGroupIncid$Finding)) {
      findingIndex <- which(StudyGroupIncid$Finding == finding)
      vehicleIndex <- grep('Vehicle', StudyGroupIncid$Treatment[findingIndex]) # VEHICLE FOR THE CURRENT FINDINGS
      if (length(vehicleIndex) > 0) {
        baseline <- StudyGroupIncid$Count[findingIndex][vehicleIndex]
        StudyGroupIncid$Count[findingIndex] <- StudyGroupIncid$Count[findingIndex] - baseline
      }
    }

    negativeIndex <- which(StudyGroupIncid$Count < 0) # when findings in HD group are less than the vehicle group
    if (length(negativeIndex) > 0) {
      StudyGroupIncid$Count[negativeIndex] <- 0
    }

    # Combine results
    GroupIncid <- rbind(GroupIncid, StudyGroupIncid)
  }

  removeIndex <- which(is.na(GroupIncid$Treatment))
  if (length(removeIndex) > 0) {
    GroupIncid <- GroupIncid[-removeIndex, ]
  }

  MIIncidence <- GroupIncid

  # Create a copy of mi_CompileData named mi_CompileData2
  mi_CompileData2 <- mi_CompileData

  # Severity calculation and recalculation
  # Adjustment of the severity score based on the Incidence score

  # Initialize ScoredData with the first 6 columns of "mi_CompileData2"
  ScoredData <- mi_CompileData2[, 1:6]

  # Initialize a counter for incidence overrides
  IncidenceOverideCount <- 0

  # Define column range for MI Data (from the 6th to the last column of mi_CompileData2)
  colIndex <- seq(7, ncol(mi_CompileData2))

  # Iterate over each column for scoring and adjustments
  for (i in colIndex) {
    colName <- colnames(mi_CompileData2)[i]
    ScoredData[[colName]] <- NA

    # Score Severity # changing the current severity value in MISEV column
    # Score Severity based on mi_CompileData2

    x <- ifelse(mi_CompileData2[, i] == 5, 5,
                ifelse(mi_CompileData2[, i] > 3, 3,
                       ifelse(mi_CompileData2[, i] == 3, 2,
                              ifelse(mi_CompileData2[, i] > 0, 1, 0))))

    ScoredData[, colName] <- x

    # Update mi_CompileData2 with the values from ScoredData for the current column
    mi_CompileData2[, colName] <- x

    # Check the Incidence percentage for each group
    for (sex in c('M', 'F')) {
      studyDataStudyIndex <- which(mi_CompileData2$STUDYID == unique(ScoredData$STUDYID))
      studyDataSexIndex <- which(mi_CompileData2$SEX == sex)
      studyDataIndex <- intersect(studyDataStudyIndex, studyDataSexIndex)
      StudyData <- mi_CompileData2[studyDataIndex, ]

      MIIncidStudyIndex <- grep(unique(ScoredData$STUDYID), MIIncidence$Treatment)
      MIIncidSexIndex <- which(MIIncidence$Sex == sex)
      MIIncidIndex <- intersect(MIIncidStudyIndex, MIIncidSexIndex)
      MIIncidStudy <- MIIncidence[MIIncidIndex, ]

      for (Dose2 in unique(StudyData$ARMCD)) {
        DoseSevIndex <- which(StudyData$ARMCD == Dose2)
        DoseSev <- StudyData[DoseSevIndex, ]
        DoseIncid <- MIIncidStudy[which(stringr::word(MIIncidStudy$Treatment, -1) == Dose2), ]
        if (colName %in% DoseIncid$Finding) {
          findingIndex <- which(DoseIncid$Finding == colName)

          Incid <- DoseIncid$Count[findingIndex]
          Incid <- ifelse(Incid >= 0.75, 5,
                          ifelse(Incid >= 0.5, 3,
                                 ifelse(Incid >= 0.25, 2,
                                        ifelse(Incid >= 0.1, 1, 0))))

          swapIndex <- which(DoseSev[[colName]] < Incid & DoseSev[[colName]] > 0)
          if (length(swapIndex) > 0) {
            DoseSev[swapIndex, colName] <- Incid
            ScoredData[studyDataIndex[DoseSevIndex], colName] <- DoseSev[, colName]
            IncidenceOverideCount <- IncidenceOverideCount + 1
          }
        }
      }
    }
  }

  # Subset the ScoredData
  ScoredData_subset_HD <- ScoredData %>% dplyr::filter(ARMCD == "HD")

  # Convert columns from 7th to the last to numeric
  ScoredData_subset_HD[, 7:ncol(ScoredData_subset_HD)] <- lapply(
    ScoredData_subset_HD[, 7:ncol(ScoredData_subset_HD)],
    function(x) as.numeric(as.character(x))
  )

  # Check the number of columns
  num_cols_ScoredData_subset_HD <- ncol(ScoredData_subset_HD)

  # If number of columns is 7, assign highest_score as the value of the 7th column
  if (num_cols_ScoredData_subset_HD == 7) {
    ScoredData_subset_HD$highest_score <- ScoredData_subset_HD[, 7]
  } else {
    # If number of columns is more than 7, get the max value from column 7 to the end
    ScoredData_subset_HD$highest_score <- matrixStats::rowMaxs(as.matrix(ScoredData_subset_HD[, 7:ncol(ScoredData_subset_HD)]), na.rm = TRUE)
  }

  # Move the highest_score column to be the third column
  ScoredData_subset_HD <- ScoredData_subset_HD[, c(1:2, ncol(ScoredData_subset_HD), 3:(ncol(ScoredData_subset_HD)-1))]

  # Enforce mutual exclusivity: If both are TRUE, throw an error or handle it
  if (return_individual_scores && return_zscore_by_USUBJID) {
    stop("Error: Both 'return_individual_scores' and 'return_zscore_by_USUBJID' cannot be TRUE at the same time.")
  }

  if (return_individual_scores) {
    #~~~~~~~~~~ GET all the severity as individual in a list ~~~~~~~~~~~~~~

    # Create a variable for "ScoredData_subset_HD" data frame
    mi_scoredata_hd <- ScoredData_subset_HD

    # Average calculation for each of
