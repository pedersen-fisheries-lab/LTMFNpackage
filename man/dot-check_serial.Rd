% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality_check_internal_fncs.R
\name{.check_serial}
\alias{.check_serial}
\title{Checks the serial ID to make sure it is listed in our equipment list}
\usage{
.check_serial(serial, equip_type = "all", mandatory = TRUE)
}
\arguments{
\item{serial}{a single serial id entry (as a character or integer)}

\item{equip_type}{what category of equipment is to be checked (ex: tag, VR2Tx, etc.)}

\item{mandatory}{TRUE/FALSE designates if the data to be checked is mandatory}
}
\value{
a report of the check status for each entry
}
\description{
Checks the serial ID to make sure it is listed in our equipment list
}
