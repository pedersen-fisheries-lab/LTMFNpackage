######################## Receiver Data ##########################

#' Reformats the receiver log into a format that interfaces well with yaps.
#'
#' @param receiverLog the VUE receiver detections database. The receiver log is downloaded from the VUE software database
#' @param full logical TRUE/FALSE statement for stating if you want a full converstion to yaps formatting, or a partial conversion to maintain some extra information (ex: sensor data, etc.). Default set to TRUE
#' @param starttime/endtime Character string or POSIXt date and time stamp to filter only a given date range of data
#' @returns returns a reformated detections database.
#'
#' @export format_receiverlog_yaps
format_receiverlog_yaps <- function(receiverLog, full=TRUE, starttime="1970-01-01 00:00:00", endtime="3000:01:01 00:00:00"){

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
  if(!is.logical(full)){
    stop("Please enter TRUE or FALSE for parameter full. This parameter indicates if data should be fully converted into yaps-compatible format or only partially to maintain more information.")
  }

  # renaming the headers
  colnames(receiverLog)[colnames(receiverLog)=="Date.UTC."] <- "Date_UTC"
  colnames(receiverLog)[colnames(receiverLog)=="Time.UTC."] <- "Time_UTC"
  colnames(receiverLog) <- tolower(colnames(receiverLog))

  # adding times stamp, UNIX time stamp, sub-second offset, and converting date and timestamp to POSIct
  receiverLog$ts <- lubridate::ymd_hms(paste0(receiverLog$date_utc, " ", substring(receiverLog$time_utc,1 , 8)), tz="UTC")
  receiverLog$date_utc <- lubridate::ymd(receiverLog$date_utc, tz="UTC")
  receiverLog$frac <- as.numeric(substring(receiverLog$time_utc,9, 12))
  receiverLog$epo <- as.integer(receiverLog$ts)

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

  if (full){
    colnames (receiverLog) [colnames (receiverLog) == 'transmitter'] <- 'tag'
    colnames (receiverLog) [colnames (receiverLog) == 'receiver'] <- 'serial'

    receiverLog$serial <- substr(receiverLog$serial, start =  7, stop = 12)

    receiverLog <- dplyr::select(receiverLog, ts, tag, epo, frac, serial)
  }

  return(receiverLog)
}



######################## GPS Data ##########################
#' convert a dataframe of gps waypoints into UTM
#'
#' @param gps_df one dataframe file with all gps waypoints to format. Best practice recommendation is to import GPX data using plotKML's readGPX function, and extract the waypoints dataframe fromt he resulting list
#' @param crs 4-digit EPSG code for the coordinate system gps points are currently in. Default is 4326 for EPSG:WGS84.
#' @param utm_zone UTM zone in which the field site is located. "18N" for Lac-Saint-Pierre, and "17N" for James Bay sites
#' @returns returns a dataframe with the UTM coordinates added
#'
#' @export format_wpts_utm_yaps
  format_wpts_utm_yaps <- function(gps_df, utm_zone, crs=4326){
    #data checks
    if(!is.data.frame(gps_df)){
      stop("The gps wpts must be in an object class data.frame or data.table. Please ensure this is the case.")
    }

    if (!is.numeric(crs)){
      stop("The crs code should be a 4-digit interger ID code in the format XXXX. Please ensure this is the case")
    }
    else if (crs !=4326){
      warning("Unless manually changed, all GPS devices have been set to WGS 84 (EPSG: 4326). Please make sure the you are correct in changing the EPSG code.")
    }
    if(!(utm_zone=="17N"|utm_zone=="18N")){
      stop("The UTM zone for James Bay is 17N, and the UTM zone for Lac-Saint-Pierre is 18N. PLease chose one of these two options")
    }

    #setting crs
    gps_df_crs <- sf::st_as_sf(gps_df, coords=c("lon", "lat"))
    sf::st_crs(gps_df_crs) <- crs
    print(sf::st_crs(gps_df_crs))

    #converting to UTM
    if(utm_zone=="17N"){
      gps_utm <- sf::st_transform(gps_df_crs, 32617)
    }
    if(utm_zone=="18N"){
      gps_utm <- sf::st_transform(gps_df_crs, 32618)
    }

    #extracting just UTM coordinates
    gps_utm_df <- sf::st_coordinates(gps_utm)
    colnames(gps_utm_df) <- c("lon_utm", "lat_utm")

    #adding UTM coordinates to  original dataframe
    gps_df <- cbind(gps_df, gps_utm_df)

    return(gps_df)
  }


#' convert a dataframe of gps track points into UTM
#'
#' @param track one dataframe file with all track points from a SINGLE track. Best practice recommendation is to import GPX data using plotKML's readGPX function, and extract the desired trackfrom the resulting list
#' @param starttime/endtime character string or POSIXt date and time stamp to filter only a given date range of data
#' @param utm_zone UTM zone in which the field site is located. "18N" for Lac-Saint-Pierre, and "17N" for James Bay sites
#' @param crs 4-digit EPSG code for the coordinate system the tracks were recorded in. Default is 4326 for EPSG:WGS84.
#' @param formatts boolean term for whether the
#' @returns returns a dataframe with the UTM coordinates added
#'
#' @export format_tracks_utm_yaps
format_tracks_utm_yaps <- function(track, starttime, endtime, utm_zone, crs=4326, formatts=TRUE){
  if(!is.data.frame(track)){
    stop("The gps track data must be in an object class data.frame or data.table. Please ensure this is the case.")
  }

  if (!is.numeric(crs)){
    stop("The crs code should be a 4-digit interger ID code in the format XXXX. Please ensure this is the case")
  }
  else if (crs !=4326){
    warning("Unless manually changed, all GPS devices have been set to WGS 84 (EPSG: 4326). Please make sure the you are correct in changing the EPSG code.")
  }
  if(!(utm_zone=="17N"|utm_zone=="18N")){
    stop("The UTM zone for James Bay is 17N, and the UTM zone for Lac-Saint-Pierre is 18N. PLease chose one of these two options")
  }
  if(!(is.character(starttime)|lubridate::is.POSIXct(starttime)|lubridate::is.POSIXt(starttime))|
     !(is.character(endtime)|lubridate::is.POSIXct(endtime)|lubridate::is.POSIXt(endtime))){
    #INCLUDE DATE CHECKING CODE
    stop("Please enter the start and endtime either as a character string in the format \"YYYY-MM-DD HH:MM:SS\", or as a POSIXct or POSIXt date object. These date classes can be obtained using the lubridate package")
  }
  if(!is.logical(formatts)){
    stop("formatts variable should be TRUE or FALSE and controls if the time column need to be formatted to remove the letters and convert to POSIXt or not. Default is TRUE")
  }

  #reformatting time column if needed
  if (formatts){
    track$time <- gsub("T", " ", track$time)
    track$time <- gsub("Z", " ", track$time)
    track$ts <- lubridate::ymd_hms(track$time, tz="UTC" )
  }

  #filtering time range
  #converting character time stamps into POSIX if necessary
  if(is.character(starttime)){
    starttime <- lubridate::ymd_hms(starttime, tz="UTC")
  }
  if(is.character(endtime)){
    endtime <- lubridate::ymd_hms(endtime, tz="UTC")
  }

  track <- dplyr::filter(track,
                         ts>=starttime,
                         ts<=endtime)

  #setting track to WGS84
  track_lat_lon <-dplyr::select(track, lon, lat)

  #setting gps points into WGS84
  track_crs <- sf::st_as_sf(track_lat_lon, coords=c("lon", "lat"))
  sf::st_crs(track_crs) <- crs
  print(sf::st_crs(track_crs))

  if(utm_zone=="17N"){
    track_utm <- sf::st_transform(track_crs, 32617)
  }
  if(utm_zone=="18N"){
    track_utm <- sf::st_transform(track_crs, 32618)
  }

  track_utm_df <- sf::st_coordinates(track_utm)
  colnames(track_utm_df) <- c("lon_utm", "lat_utm")

  #Adding utm coordinates back into original dataframe
  track <- cbind(track, track_utm_df)

  return(track)
}



