% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_reprtree_from_rf_model.R
\name{get_reprtree_from_rf_model}
\alias{get_reprtree_from_rf_model}
\title{Get Representation Tree from Random Forest Model}
\usage{
get_reprtree_from_rf_model(
  ml_formatted_scores_df = NULL,
  path_db,
  rat_studies = FALSE,
  studyid_metadata = NULL,
  fake_study = FALSE,
  use_xpt_file = FALSE,
  Round = FALSE,
  Impute = FALSE,
  reps,
  holdback,
  Undersample = FALSE,
  hyperparameter_tuning = FALSE,
  error_correction_method,
  best.m = NULL
)
}
\arguments{
\item{path_db}{A character string representing the path to the database used for fetching or processing the data.}

\item{rat_studies}{A logical flag indicating whether rat studies are used (default: \code{FALSE}).}

\item{studyid_metadata}{A data frame containing metadata related to study IDs (default: \code{NULL}).}

\item{fake_study}{A logical flag indicating whether to use fake study data (default: \code{FALSE}).}

\item{use_xpt_file}{A logical flag indicating whether to use the XPT file format for data input (default: \code{FALSE}).}

\item{Round}{A logical flag indicating whether to round the data before processing (default: \code{FALSE}).}

\item{Impute}{A logical flag indicating whether to impute missing values in the data (default: \code{FALSE}).}

\item{reps}{An integer specifying the number of repetitions to perform for cross-validation or resampling.}

\item{holdback}{A numeric value representing the fraction of data to hold back for testing.}

\item{Undersample}{A logical flag indicating whether undersampling should be applied to balance the dataset (default: \code{FALSE}).}

\item{hyperparameter_tuning}{A logical flag indicating whether hyperparameter tuning should be performed (default: \code{FALSE}).}

\item{error_correction_method}{A character string specifying the method for error correction. Must be one of \code{'Flip'}, \code{'Prune'}, or \code{'None'}.}

\item{Data}{A data frame containing the dataset to train the Random Forest model. If \code{NULL}, data is fetched using the \code{get_Data_formatted_for_ml_and_best.m} function.}
}
\value{
A plot of the first tree from the Random Forest model is displayed. The function does not return the ReprTree object explicitly, but it is generated and used for plotting.
}
\description{
This function trains a Random Forest model on a provided dataset and generates a representation tree (ReprTree) from the trained model. It supports various preprocessing configurations, model hyperparameters, and sampling strategies, including random undersampling. The function also allows for error correction and hyperparameter tuning.
}
\details{
The function performs the following steps:
\enumerate{
\item \strong{Data Preparation}: If \code{Data} is \code{NULL}, it is fetched using the \code{get_Data_formatted_for_ml_and_best.m} function. Data is then split into training (70\%) and testing (30\%) sets. If \code{Undersample} is \code{TRUE}, the training data is balanced using undersampling.
\item \strong{Model Training}: A Random Forest model is trained using the \code{randomForest::randomForest} function. The target variable is \code{Target_Organ}, and the model uses the best hyperparameter (\code{best.m}). The number of trees is set to 500.
\item \strong{ReprTree Generation}: The \code{reprtree::ReprTree} function is used to generate the representation tree from the trained Random Forest model.
\item \strong{Visualization}: The first tree from the Random Forest model is plotted using the \code{reprtree::plot.getTree} function.
}
}
\examples{
\dontrun{
#get_reprtree_from_rf_model(
  Data = my_data,
  path_db = "path/to/database",
  rat_studies = TRUE,
  studyid_metadata = my_metadata,
  fake_study = FALSE,
  use_xpt_file = TRUE,
  Round = TRUE,
  Impute = TRUE,
  reps = 5,
  holdback = 0.3,
  Undersample = TRUE,
  hyperparameter_tuning = FALSE,
  error_correction_method = "Flip"
)
}
}
