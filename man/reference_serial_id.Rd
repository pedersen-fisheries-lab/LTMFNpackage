% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{reference_serial_id}
\alias{reference_serial_id}
\title{Reference document of Innovasea-Provided equipment}
\format{
A data frame with 157 rows and 4 columns:

\describe{
\item{type}{Type of equipment (based on our defined codeing system)}
\item{product_id}{Official Innovasea Product ID}
\item{serial_id}{serial number of each piece of equipment}
\item{transmitter_id}{transmitter id number of each piece of equipment. If the equipment has multiple transmitter codes, only one is given here}
}
}
\source{
{Downloaded from https://liveconcordia-my.sharepoint.com/personal/eric_pedersen_concordia_ca/Documents/Research/projects\%20-\%20spatial\%20community\%20ecology/Quebec\%20fish\%20telemetry\%20network/data/EquipmentSummary.xlsx:

reference_serial_id <- read_excel("data-raw/EquipmentSummary.xlsx", sheet = "reference_serial_id")
usethis::use_data(reference_serial_id, overwrite = TRUE)}
}
\usage{
reference_serial_id
}
\description{
List of all innovasea-provided and GPS equipment, equipment type, product ID and serial number. \strong{\strong{MUST BE UPDATED WHEN NEW EQUIPMENT IS PURCHASED}}
}
\keyword{datasets}
