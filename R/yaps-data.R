######################## Receiver Data ##########################

#' Reformats the receiver log into a format that interfaces well with yaps. The receiver log is downloaded from the VUE software database
#'
#' @param receiverLog the VUE receiver detections database
#' @returns returns a reformated detections database.
#'
#' @export format_receiverlog_yaps
format_receiverlog_yaps <- function(receiverLog){

  colnames(receiverLog)[c(1, 2)] <- c("Date_UTC", "Time_UTC")
  colnames(receiverLog) <- tolower(colnames(receiverLog))
  receiverLog$ts <- lubridate::ymd_hms(paste0(receiverLog$date_utc, " ", substring(receiverLog$time_utc,1 , 8)), tz="UTC")
  receiverLog$date_utc <- lubridate::ymd(receiverLog$date_utc, tz="UTC")
  receiverLog$frac <- as.numeric(substring(receiverLog$time_utc,9, 12))
  receiverLog$epo <- as.numeric(receiverLog$ts)
  receiverLog <- dplyr::select(receiverLog,date_utc, time_utc, ts, epo, frac, receiver, transmitter, stationname, sensorvalue, sensorunit)
  receiverLog <- dplyr::relocate(receiverLog,date_utc, time_utc, ts, epo, frac, receiver, transmitter, stationname, sensorvalue, sensorunit)

  return(receiverLog)
}
