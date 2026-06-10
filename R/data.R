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
#' reference_serial_id <- read_excel("data-raw/EquipmentSummary.xlsx", sheet = "reference_serial_id")
#' usethis::use_data(reference_serial_id, overwrite = TRUE)}
"reference_serial_id"

#' @title Approved work sites code
#'
#' @format A vector of the entries "JEA", "JWE", "JWA", JWH", "JGR", "JMA", "JRU", "JOY", "LEA", "LWE", "LSW", "base", "lab", and "other"
#'
#' @source {Running the "make-validation-datafiles.R" script in `data-raw`}
"sites"

#' Approved equipment type codes
#'
#' @format A vector of the entries "VR2Tx", "VR2W",  "tag", "RTtag", "VR100", "AMchl", "AMdo",  "AMsal", "GPS", "other"
#'
#' @source {Running the "make-validation-datafiles.R" script in `data-raw`}
"equip_types"

#' Approved deployment type codes
#'
#' @format A vector of the entries "GA", "GR", "RT","SE", and  "other"
#'
#' @source {Running the "make-validation-datafiles.R" script in `data-raw`}
"deploy_types"


#' Approved fish capture method codes
#'
#' @format A vector of the entries "fyke", "angling", "castNet", "other"
#'
#' @source {Running the "make-validation-datafiles.R" script in `data-raw`}
"capture_methods"


#' Approved fish capture outcome codes
#'
#' @format A vector of the entries "released", "tagged",
#' "euthenizedCapture", "euthenizedSurgery","euthenizedRecovery",
#' "mortalityCapture" "mortalitySurgery", "mortalityRecovery"
#'
#' @source {Running the "make-validation-datafiles.R" script in `data-raw`}
"capture_outcomes"


#' Approved fish species codes '
#'
#' @format A vector of the entries 'c("COAR",  "COCL", "ESLU",
#"MOHU",  "MOSA" , "OSMO", "PEFL", "SAFO", "SANA", "SAVI",  "TITI", "other")
#
#' @source {Running the "make-validation-datafiles.R" script in `data-raw`}
"species_codes"

#' Approved fish sex and maturity codes
#'
#' @format A vector of the entries "MM", "IM", "MF", "IF", "unk"
#'
#' @source {Running the "make-validation-datafiles.R" script in `data-raw`}
"sex_options"

#' Approved equipment action codes
#'
#' @format A vector of the entries "On", "Off", "deployed", "retrieved", "moved","notLocated", "notRetrieved", "located",
#'  "dataDownload", "checked", "configure", "recharge",  "anchorMid", "anchorEnd",
#'  "repairSent", "repairReturn", and
#'   "other"
#'
#' @source {Running the "make-validation-datafiles.R" script in `data-raw`}
"equipment_actions"

#' Approved fyke action codes
#'
#' @format A vector of the entries "set", "retrieved", "checked", "moved"
#'
#' @source {Running the "make-validation-datafiles.R" script in `data-raw`}
"fyke_actions"

#' Approved fish condition codes
#'
#' @format a vector with entries "vigorous", "normal", "weak"
#'
#' @source {Running the "make-validation-datafiles.R" script in `data-raw`}
"condition_codes"

#' Approved tag model codes
#'
#' @format a vector with entries "V6", "V7", "V8", "V9", "V13", "V16"
#'
#' @source {Running the "make-validation-datafiles.R" script in `data-raw`}
"tag_models"

#' Approved rt type codes
#'
#' @format a vector with entries "boat", "fixed_st", "fixed_lt"
#'
#' @source {Running the "make-validation-datafiles.R" script in `data-raw`}
"rt_types"

#' OTN metadata tagging sheets headers
#'
#' @format a vector with character data for all headers in the OTN tagging metadata sheet
#'
#' @source {Running the "make-validation-datafiles.R" script in `data-raw`. Based off Tag Metadata data sheet provided by OTN: https://members.oceantrack.org/data/data-collection.}
"otn_tagging_metadata_headers"

#' OTN metadata deployment sheets headers
#'
#' @format a vector with character data for all headers in the OTN deployment metadata sheet
#'
#' @source {Running the "make-validation-datafiles.R" script in `data-raw`. Based off Instrument Deployment Metadata data sheet provided by OTN: https://members.oceantrack.org/data/data-collection.}
"otn_instrument_deployment_headers"
