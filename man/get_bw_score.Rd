% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_bw_score.R
\name{get_bw_score}
\alias{get_bw_score}
\title{get BW score for a given studyid}
\usage{
get_bw_score(
  studyid = NULL,
  path_db,
  fake_study = FALSE,
  use_xpt_file = FALSE,
  master_compiledata = NULL,
  return_individual_scores = FALSE
)
}
\arguments{
\item{studyid}{Mandatory, character \cr
Studyid number}

\item{path_db}{Mandatory, character \cr
path of database}

\item{fake_study}{optional, Boolean \cr
whether study generated by SENDsanitizer package}

\item{use_xpt_file}{Mandatory, character \cr
Studyid number}

\item{master_compiledata}{Mandatory, character \cr
path of database}

\item{return_individual_scores}{optional, Boolean \cr
whether study generated by SENDsanitizer package}
}
\value{
dataframe
}
\description{
get BW score for a given studyid
}
\examples{
\dontrun{
get_bw_score(studyid='1234123', path_db='path/to/database.db')
}
}
