#' Raw VUE detection data
#'
#' An example of a raw VUE download of detection data. This is used to illustrate data cleaning steps necessary for analysis
#'
#' @format A data frame with 1000 rows and 13 columns:
#'
#' \describe{
#'   \item{Date.UTC., Time.UTC.}{Date and time of dection in UTC}
#'   \item{Receiver}{which receiver recorded the detection}
#'   \item{Transmitter}{which transmitter code was detected}
#'   \item{TransmitterName, TransmitterSerial}{Unpopulated columns (not applicable for our project)}
#'   \item{SensorValue}{Untransformed interger sensor values acoustically transmitted by an AquaMeasure sensor}
#'   \item{SensorUnit}{}
#'   \item{StationName}{Station Name assigned tot he receiver at the time of recording}
#'   \item{Latitude, Longitude}{Unpopulated columns (not applicable for our project)}
#'   \item{TransmitterType, SensorPrecision}{Unpopulated Columns for sensor tags (not applicable for our project)}
#' }
#' @source {subset of VUE download}
"vue_det"

#' GPS waypoint data frame
#'
#' An example of a GPS waypoint data imported from a GPX file using readGPX(). This is used to illustrate GPS data cleaning steps necessary for analysis
#'
#' @format A data frame with 82 rows and 8 columns:
#'
#' \describe{
#'   \item{lon, lat}{longitude and latitude in WGS84}
#'   \item{ele}{elevation from sea level  in meters}
#'   \item{time}{time of data download in YYYY-MM-DDTHH:MM:SSZ}
#'   \item{name}{the ID name assigned to each waypoint}
#'   \item{sym}{symbol type used by GARMIN}
#'   \item{type, extensions}{GARMIN extra data}
#' }
#' @source {From an eTREX device, data imported into GARMIN basecamp, downloaded as a GPX and loaded into R using readGPX()}
"wpts_gpx"

#' GPS track data frame
#'
#' An example of  GPS track data imported from a GPX file using readGPX(). This is used to illustrate GPS data cleaning steps necessary for analysis
#'
#' @format A data frame with 8323 rows and 4 columns:
#'
#' \describe{
#'   \item{lon, lat}{longitude and latitude in WGS84}
#'   \item{ele}{elevation from sea level  in meters}
#'   \item{time}{time of data download in YYYY-MM-DDTHH:MM:SSZ}
#' }
#' @source {From an eTREX device, data imported into GARMIN basecamp, downloaded as a GPX and loaded into R using readGPX()}
"track_gpx"

#' Reference document of Innovasea-Provided equipment
#'
#' List of all innovasea-provided and GPS equipment, equipment type, product ID and serial number. ****MUST BE UPDATED WHEN NEW EQUIPMENT IS PURCHASED****
#'
#' @format A data frame with 157 rows and 4 columns:
#'
#' \describe{
#'   \item{type}{Type of equipment (based on our defined codeing system)}
#'   \item{product_id}{Official Innovasea Product ID}
#'   \item{serial_id}{serial number of each piece of equipment}
#'   \item{transmitter_id}{transmitter id number of each piece of equipment. If the equipment has multiple transmitter codes, only one is given here}
#' }
#' @source {Downloaded from https://liveconcordia-my.sharepoint.com/personal/eric_pedersen_concordia_ca/Documents/Research/projects%20-%20spatial%20community%20ecology/Quebec%20fish%20telemetry%20network/data/EquipmentSummary.xlsx:
#'
#' reference_serial_id <- read_excel("data-raw/EquipmentSummary.xlsx", sheet = "reference_serial_id")}
"reference_serial_id"

#' Approved work sites code
#'
#' @format A vector of the entries "JEA", "JGR", "JMA", "JRU", "JOY", "LEA", "LWE", "base", "lab", and "other"
#'
#' @source {manually created from the excel data validation}
"sites"

#' Approved equipment type codes
#'
#' @format A vector of the entries "VR2Tx", "VR2W",  "tag",   "RTtag", "VR100", "AMchl", "AMdo",  "AMsal", "GPS"
#'
#' @source {manually created from the excel data validation}
"equip_types"

#' Approved deployment type codes
#'
#' @format A vector of the entries "GA", "GR", "RT", "other"
#'
#' @source {manually created from the excel data validation}
"deploy_types"

#' Approved fish capture method codes
#'
#' @format A vector of the entries "fyke", "angling", "castNet", "other"
#'
#' @source {manually created from the excel data validation}
"capture_methods"

#' Approved fish species codes
#'
#' @format A vector of the entries 'c("COAR",  "COCL", "ESLU",  "MOHU",  "MOSA" , "OSMO", "PEFL", "SAFO", "SANA", "SAVI",  "TITI", "bycatch")
#
#' @source {manually created from the excel data validation}
"species_codes"

#' Approved fish sex and maturity codes
#'
#' @format A vector of the entries "MM", "IM", "MF", "IF", "unk"
#'
#' @source {manually created from the excel data validation}
"sex_options"

#' Approved equipment action codes
#'
#' @format A vector of the entries "On", "Off", "deployed", "retrieved", "moved" , "dataDownload", "checked", "configure", "recharge", "other"
#'
#' @source {manually created from the excel data validation}
"equipment_actions"

#' Approved fyke action codes
#'
#' @format A vector of the entries "set", "retrieved", "checked", "moved"
#'
#' @source {manually created from the excel data validation}
"fyke_actions"

#' Approved fish condition codes
#'
#' @format a vector with entries "vigorous", "normal", "weak"
#'
#' @source {condition_codes <- c("vigorous", "normal", "weak")     usethis::use_data(condition_codes)}
"condition_codes"

#' Approved tag model codes
#'
#' @format a vector with entries "V6", "V7", "V8", "V9", "V13", "V16"
#'
#' @source {tag_models <- c("V6", "V7", "V8", "V9", "V13", "V16")       usethis::use_data(tag_models)}
"tag_models"
