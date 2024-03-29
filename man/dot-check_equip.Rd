% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality_check_internal_fncs.R
\name{.check_equip}
\alias{.check_equip}
\title{Checks equipment type to make sure it is within the list of allowed values}
\usage{
.check_equip(equip, mandatory = TRUE)
}
\arguments{
\item{equip}{a single equipment entry (as a character string)}

\item{mandatory}{TRUE/FALSE designates if the data to be checked is mandatory}
}
\value{
a report of the check status for each entry
}
\description{
Checks equipment type to make sure it is within the list of allowed values
}
