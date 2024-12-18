
make_histogram <- function(Data =NULL,
                           generateBarPlot= FALSE){




  # Generate data if not provided
  if (is.null(Data)) {
    #---------------------------------------------------------------------
    Data <- get_col_harmonized_scores_df(liver_score_data_frame,
                                                     Round = TRUE)
    #---------------------------------------------------------------------
  }

  #---------------------------------------------------------------------------
  #Check if data is a valid data frame
  if (!is.data.frame(Data)) {
    stop("The input data must be a data frame.")
  }
  #---------------------------------------------------------------------------

  ##---------------
  if (generateBarPlot == T) {

    Finding <- NULL
    LIVER <- NULL
    Value <- NULL
    for (finding in colnames(Data)[3:ncol(Data)]) {

      Finding <- c(Finding, finding)
      LIVER <- c(LIVER, 'Y')
      Value <- c(Value, mean(Data[which(Data$indst_TO == "Liver"), finding], na.rm = T))

      Finding <- c(Finding, finding)
      LIVER <- c(LIVER, 'N')
      Value <- c(Value, mean(Data[which(Data$indst_TO != "Liver"), finding], na.rm = T))

    }

    plotData <- as.data.frame(cbind(Finding, LIVER, Value))
    plotData$LIVER <- factor(plotData$LIVER)
    plotData$Finding <- factor(plotData$Finding)
    plotData$Value = as.numeric(plotData$Value)
    # plotData$Value = log(as.numeric(plotData$Value), base = 10)

    ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~check and double check
    p <- ggplot2::ggplot(plotData, ggplot2::aes(x = Finding, y = Value, fill = LIVER)) +
      ggplot2::geom_bar(stat="identity", position = 'dodge') +
      ggplot2::theme(text = ggplot2::element_text(size = 20),
                     axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggplot2::ylab('Average Score')
    print(p)
  }


return()


}
