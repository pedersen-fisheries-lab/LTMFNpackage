% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality_check_internal_fncs.R
\name{.check_action}
\alias{.check_action}
\title{Checks to make sure the action is one of the permitted entries}
\usage{
.check_action(action, mandatory = TRUE)
}
\arguments{
\item{action}{a single station ID entry entry of the equipment type (as a character)}

\item{mandatory}{TRUE/FALSE designates if the data to be checked is mandatory}
}
\value{
a report of the check status for the entry
}
\description{
Checks to make sure the action is one of the permitted entries
}
