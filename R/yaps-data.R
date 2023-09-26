######################## Receiver Data ##########################

#' Reformats the receiver log into a format that interfaces well with yaps. The receiver log is downloaded from the VUE software database
#'
#' @param receiverLog the VUE receiver detections database
#' @returns returns a reformated detections database.
#'
#' @export format_receiverlog_yaps
format_receiverlog_yaps <- function(receiverLog, starttime="1970-01-01 00:00:00", endtime="3000:01:01 00:00:00"){

  #checks to make sure the original log is in a useable format
  cols_match <- sum(colnames(receiverLog)!=c("Date.UTC.","Time.UTC.","Receiver","Transmitter", "TransmitterName", "TransmitterSerial","SensorValue","SensorUnit","StationName" ,"Latitude", "Longitude","TransmitterType","SensorPrecision"  ))

  if(!is.data.frame(receiverLog)){
    stop("The receiver log must be an object class data.frame or data.table. Please ensure this is the case.")
  }
  if(!(is.character(starttime)|lubridate::is.POSIXct(starttime)|lubridate::is.POSIXt(starttime))|
     !(is.character(endtime)|lubridate::is.POSIXct(endtime)|lubridate::is.POSIXt(endtime))){
    #INCLUDE DATE CHECKING CODE
    stop("Please enter the start and endtime either as a character string in the format \"YYYY-MM-DD HH:MM:SS\", or as a POSIXct or POSIXt date object. These date classes can be obtained using the lubridate package")
  }
  if (cols_match>0|
      length(colnames(receiverLog))!=13){
    warning("The column names of the provided dataframe do not match the expected column names:\n
            \"Date.UTC.\",\"Time.UTC.\",\"Receiver\",\"Transmitter\", \"TransmitterName\", \"TransmitterSerial\",\"SensorValue\",\"SensorUnit\",\"StationName\" ,\"Latitude\",
            \"Longitude\",\"TransmitterType\",\"SensorPrecision\".\n
            This may affect the performance of this function. For best results, import and use a raw csv file downloaded from the VUE software's detections page.")
  }

  # renaming the headers
  colnames(receiverLog)[colnames(receiverLog)=="Date.UTC."] <- "Date_UTC"
  colnames(receiverLog)[colnames(receiverLog)=="Time.UTC."] <- "Time_UTC"
  colnames(receiverLog) <- tolower(colnames(receiverLog))

  # adding times stamp, UNIX time stamp, sub-second offset, and converting date and timestamp to POSIct
  receiverLog$ts <- lubridate::ymd_hms(paste0(receiverLog$date_utc, " ", substring(receiverLog$time_utc,1 , 8)), tz="UTC")
  receiverLog$date_utc <- lubridate::ymd(receiverLog$date_utc, tz="UTC")
  receiverLog$frac <- as.numeric(substring(receiverLog$time_utc,9, 12))
  receiverLog$epo <- as.numeric(receiverLog$ts)

  #reordering and removing unnecessary columns
  receiverLog <- dplyr::select(receiverLog,date_utc, time_utc, ts, epo, frac, receiver, transmitter, stationname, sensorvalue, sensorunit)
  receiverLog <- dplyr::relocate(receiverLog,date_utc, time_utc, ts, epo, frac, receiver, transmitter, stationname, sensorvalue, sensorunit)

  #filtering the time range desired

  #converting character time stamps into POSIX
  if(is.character(starttime)){
    starttime <- lubridate::ymd_hms(starttime, tz="UTC")
  }
  if(is.character(endtime)){
    endtime <- lubridate::ymd_hms(endtime, tz="UTC")
  }

  receiverLog <- dplyr::filter(receiverLog, ts>=starttime, ts<=endtime)

  return(receiverLog)
}
