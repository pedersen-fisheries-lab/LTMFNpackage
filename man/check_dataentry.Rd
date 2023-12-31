% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality_check.R
\name{check_dataentry}
\alias{check_dataentry}
\title{Performs comprehensive verification of all excel data entry files within a given folder}
\usage{
check_dataentry(folder_path)
}
\arguments{
\item{folder_path}{the path of the folder in which the data entry xlsx sheets are stored. Folder must only contain the correct format of data entry sheets}
}
\value{
creates a folder in which the flagged xlsx files are placed for the tester to look through them and correct data entry mistakes.
}
\description{
Performs comprehensive verification of all excel data entry files within a given folder
}
