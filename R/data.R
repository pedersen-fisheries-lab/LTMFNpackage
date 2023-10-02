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
#' @example
#' data(vue_det)
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
#' @example
#' data(wpts_gpx)
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
#' @example
#' data(track_gpx)
"track_gpx"
