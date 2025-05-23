% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_lb_score.R
\name{get_lb_score}
\alias{get_lb_score}
\title{Get Laboratory Test Results (LB) domain Z-score for a given STUDYID from SQLite Database or \code{.xpt} Files}
\usage{
get_lb_score(
  studyid = NULL,
  path_db,
  fake_study = FALSE,
  use_xpt_file = FALSE,
  master_compiledata = NULL,
  return_individual_scores = FALSE,
  return_zscore_by_USUBJID = FALSE
)
}
\arguments{
\item{studyid}{Character. STUDYID number. Defaults to \code{NULL}.
Required for SQLite databases (\code{use_xpt_file = FALSE}).
Must be \code{NULL} for \code{.xpt} files (\code{use_xpt_file = TRUE}).}

\item{path_db}{Character. Path to the SQLite database file or a folder containing \code{.xpt} files. Mandatory.}

\item{fake_study}{Logical. Whether the study data is generated by the \code{SENDsanitizer} package. Defaults to \code{FALSE}.}

\item{use_xpt_file}{Logical. Whether to retrieve study data from \code{.xpt} files instead of the SQLite database. Defaults to \code{FALSE}.}

\item{master_compiledata}{Optional, character \cr
If \code{master_compiledata} is not supplied (i.e., \code{NULL}), the function will automatically call the \code{get_compile_data} function to calculate it.}

\item{return_individual_scores}{Optional, logical \cr
If TRUE, the function returns individual scores for each domain by averaging the scores of all subjects/animals (\code{USUBJID}) in the study. Default is \code{FALSE}.}

\item{return_zscore_by_USUBJID}{Optional, logical \cr
If \code{TRUE}, the function returns Z-scores for each animal/subject by \code{USUBJID}. Default is \code{FALSE}.}
}
\value{
A data frame containing the LB Z-scores:
\itemize{
\item If \code{return_individual_scores = TRUE}: Returns averaged Z-scores for each  domain per \code{studyid}.
\item If \code{return_zscore_by_USUBJID = TRUE}: Returns Z-score for each animal/subject by \code{USUBJID} for each domain per \code{studyid}.
\item Otherwise, a summarized BW score for the specified \code{studyid}.
}
}
\description{
This function computes the LB Z-score for a given STUDYID using data stored in a specified database.
It offers various optional parameters to customize the output, such as whether to return individual scores or Z-scores by \code{USUBJID}.
}
\examples{
\dontrun{
# Example usage of the function
get_lb_score(studyid='1234123', path_db='path/to/database.db')
}

}
