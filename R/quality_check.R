############## internal functions ####
#' Checks date formats to make sure they are in YYYY-MM-DD format at the correct time
#'
#' @param date a single date entry (as a character string)
#' @returns a report of the check status for each entry
.check_date <-  function(date){
  date_not_entered <- FALSE
  date_invalid <- FALSE
  date_out_of_range <- FALSE

  #check if entered
  if (is.na(date)| !is.character(date) | date=="") {
    date_not_entered <- TRUE
  } else {
    #check length
    if (nchar(date)!=10)
      date_invalid <- TRUE
    #check dash locations
    if (substr(date, 5,5)!="-" |substr(date, 8,8)!="-")
      date_invalid <- TRUE

    #check values
    if(!date_invalid) {
      year <- as.numeric(substring(date, 1, 4))
      month <- as.numeric(substring(date, 6, 7))
      day <- as.numeric(substring(date, 9, 10))

      #checking that year, month and date are written numerically
      if(is.na(year)|is.na(month)|is.na(day)){
        date_invalid <- TRUE
      #checking month
      } else if (month>12 |month<1) {
        date_invalid <- TRUE
      #Check day for each month option (long, short and february)
      } else if (month %in% c(1,3,5, 7, 8, 10, 12)){
        if(day>31 |day<1)
          date_invalid <- TRUE
      } else if (month%in% c( 4, 6, 9)){
        if(day>30 |day<1)
          date_invalid <- TRUE
      } else if (month==2){
        if(day>29 |day<1)
          date_invalid <- TRUE
      }
    }

    #check date range
    if (!date_invalid){
      if(as.numeric(substring(date, 1, 4))<2023 | as.numeric(substring(date, 1, 4))>as.numeric(substring(Sys.Date(), 1, 4)))
      date_out_of_range <- TRUE
    }
  }

  #return standardized error code
  report <- ""
  if(date_not_entered)
    report <- paste0(report ,"date_not_entered")
  if (date_invalid){
    report <- paste0(report ,"date_invalid")
  }
  if(date_out_of_range){
    report <- paste0(report, "date_out_of_range")
  }

  return(report)
}

#' Checks manually-input time entries
#'
#' @param time a single time entry (as a character string)
#' @returns a report of the check status
.check_time <- function(time){

  time_not_entered <- FALSE
  time_invalid <- FALSE

  #checking it exists
  if (is.na(time) | !is.character(time) | time=="") {
    time_not_entered <- TRUE
  } else {
    if(nchar(time)==8) {
      hours <- 0
    } else if (nchar(time)==7) {
      hours <- -1
    }

    if (!(nchar(time)==8 |nchar(time)==7)) {
      time_invalid <- TRUE
    } else if (substr(time, 3+hours,3+hours)!=":" |substr(time, 6+hours,6+hours)!=":") {
      time_invalid <- TRUE
    } else {
      hour <- as.numeric(substring(time, 1, 2+hours))
      minute <- as.numeric(substring(time, 4+hours, 5+hours))
      second <- as.numeric(substr(time, 7+hours, 8+hours))

      #checking that all entries are numeric
      if(is.na(hour)|is.na(minute)|is.na(second)) {
        time_invalid <- TRUE
      } else if(hour<0| hour>24){
        time_invalid <- TRUE
      } else if (minute<0 |minute>59){
        time_invalid <- TRUE
      } else if(second<0 |second>59){
        time_invalid <- TRUE
      }
    }
  }

  #generating the report
  report <- ""
  if(time_not_entered){
    report <- paste0(report, "time_not_entered")
  }
  if (time_invalid)
    report <- paste0(report, "time_invalid")
  return (report)
}

#' Checks site ID to make sure it is within the list of allowed values
#'
#' @param site a single site entry (as a character string)
#' @returns a report of the check status for each entry
.check_site <-  function(site) {
  site_not_entered <- FALSE
  site_invalid <- FALSE

  if (is.na(site) | !is.character(site) | site=="") {
    site_not_entered <- TRUE
  } else if(!(site %in% sites)){
    site_invalid <- TRUE
  }

  #return report
  report <- ""
  if (site_not_entered)
    report <- paste0(report, "site_not_entered")
  if(site_invalid)
    report <- paste0(report, "site_invalid")

  return(report)
}

#' Checks equipment type to make sure it is within the list of allowed values
#'
#' @param equip a single equipment entry (as a character string)
#' @returns a report of the check status for each entry
.check_equip <-  function(equip){
  equip_not_entered <- FALSE
  equip_invalid <- FALSE

  if (is.na(equip) | !is.character(equip) | equip=="") {
    equip_not_entered <- TRUE
  } else if(!(equip %in% equip_types)){
    equip_invalid <- TRUE
  }

  #return report
  report <- ""
  if (equip_not_entered)
    report <- paste0(report, "equip_not_entered")
  if(equip_invalid)
    report <- paste0(report, "equip_invalid")

  return(report)
}

#' Checks the serial ID to make sure it is listed in our equipment list
#'
#' @param serial a single serial id entry (as a character or integer)
#' @returns a report of the check status for each entry
.check_serial <- function(serial){
  serial_not_entered <- FALSE
  serial_invalid <- FALSE

  if (is.na(serial)|serial=="") {
    serial_not_entered <- TRUE
  } else if(!(serial %in% reference_serial_id$SerialNo)){
    serial_invalid <- TRUE
  }

  #return report
  report <- ""
  if (serial_not_entered)
    report <- paste0(report, "serial_not_entered")
  if(serial_invalid)
    report <- paste0(report, "serial_invalid")

  return(report)
}

#' Checks to make sure the serial ID number matches the equipment type
#'
#' @param equip a single equipment type entry of the equipment type (as a character)
#' @param serial a single serial ID entry of the serial id number (as a character or integer)
#' @returns a report of the check status for each entry
.check_equip_serial_match <- function(equip, serial){
  equip_serial_nomatch <- FALSE

  if(equip != reference_serial_id$Type[reference_serial_id$SerialNo==serial]) {
    equip_serial_nomatch <- TRUE
  }

  report <- ""
  if (equip_serial_nomatch) {
    report <- paste0(report, "equip_serial_nomatch")
  }

  return(report)
}

#' Checks to make sure the station ID follows a valid format
#'
#' @param stnid a single station ID entry entry of the equipment type (as a character)
#' @returns a report of the check status for the entry
.check_stnid <- function(stnid){
  stnid_invalid <- FALSE

  if(! (is.na(stnid) | stnid=="" )){

    deploy <- substring(stnid, 1, 2)
    site <- substring(stnid, 4, 6)
    id_num <- substring(stnid, 8, 9)
    id_let <- substring(stnid, 11, 11)

    #checking length
    if(nchar(stnid)!=11) {
      stnid_invalid <- TRUE

    #checking - at positions 3, 7, 10
    } else if(substr(stnid, 3, 3)!="-" | substr(stnid, 7, 7)!="-" |substr(stnid, 10, 10)!="-" ) {
      stnid_invalid <- TRUE

    #checking deplyment
    } else if( !(deploy %in% c("RT", "GR", "GA"))){
      stnid_invalid <- TRUE

    #checking site (list of site ids, excluding general "base", "lab", and "other")
    } else if (!(site %in% sites)){
      stnid_invalid <- TRUE

    } else if (site %in% c("lab", "base","other")) {
      stnid_invalid <- TRUE

    #checking number
    } else if (is.na(as.numeric(id_num))){
      stnid_invalid <- TRUE

    } else if (as.numeric(id_num)<1 | as.numeric(id_num) >99){
      stnid_invalid <- TRUE

    #checking id_let based on unicode order
    } else if ( !(id_let %in% LETTERS)) {
      stnid_invalid <- TRUE
    }
  }


  report <- ""
  if (stnid_invalid) {
    report <- paste0(report, "stnid_invalid")
  }

  return(report)
}

#' Checks to make sure the action is one of the permitted entries
#'
#' @param action a single station ID entry entry of the equipment type (as a character)
#' @returns a report of the check status for the entry
.check_action <- function(action){
  action_not_entered <- FALSE
  action_invalid <- FALSE

  if (is.na(action)| action==""){
    action_not_entered <- TRUE
  } else if (!(action %in% equipment_actions)){
    action_invalid <- TRUE
  }

  report <- ""
  if(action_not_entered){
    report <- paste0(report, "action_not_entered")
  }
  if(action_invalid) {
    report <- paste0(report, "action_invalid")
  }

  return(report)
}

.check_deploy <- function(deploy){

}

.check_stnid_deploy_match <- function(stnid, deploy){

}

.check_wpt <- function(wpt){

}

.check_lat <- function(lat){

}

.check_lon <- function(lon){

}

#' Checks to make sure that the crew is entered and comma separated
#'
#' @param crew a single station ID entry entry of the equipment type (as a character)
#' @returns a report of the check status for the entry
.check_crew <- function(crew){
  crew_not_entered <- FALSE
  crew_invalid <- FALSE

  #checking that crew was entered
  if(is.na(crew) | crew==""){
    crew_not_entered <- TRUE

  #checking that all crew codes are comma separated and are 2-3 letter codes
  #****DOES NOT CURRENTLY CHECK THE LAST ENTRY
  } else {
    commas <- unlist(gregexpr(',', crew))
    commas_dist <- diff(sort(commas))
    num_invalid <- sum(commas_dist>5 | commas_dist<3)

    if(num_invalid>0){
      crew_invalid <- TRUE
    }
  }

  report <- ""
  if (crew_not_entered)
    report <- paste0(report, "crew_not_entered")
  if(crew_invalid)
    report <- paste0(report, "crew_invalid")

  return(report)
}

######################### Error code summary ###################

#' Provides a summary of the possible error codes and their meaning
#'
#' @export quality_control_codes
quality_control_codes <- function () {
  cat(
    "date_not_entered: Date is left blank. This entry is mandatory, please fill it.
date_invalid: Date is invalid. The date should be reported in YYYY-MM-DD format. Check that year, month and date values are correct, that they are - delimited, and that no extra characters are present

time_not_entered: time is left blank. This entry is mandatory, please fill it.
time_invalid (col#): Time is invalid in the column indicated in parentheses. Time should be reported in HH:MM:SS format in 24h time. Check that hour, minute and second values are correct, that they are : delimited, that there are no extra characters and that the time is in 24h format. Seconds must be included. You may write 00 if a value is not reported to the seconds

site_not_entered: site is left blank. This entry is mandatory, please fill it.
site_invalid: Site ID is invalid. It must be one of the following values _____________. Make sure the value matches and that no extra characters are present

equip_invalid: equipment type is invalid. It must be one of the following values _____________. Make sure the value matches and that no extra characters are present

serial_invalid (col#): serial ID is invalid in the column indicated in parentheses. Make sure the serial ID is present in the list ___________.

equip_id_nomatch: serial ID value does not match the equipment type. Make sure the correct serial ID or equipment type is enterred

stnid_invalid: the station ID is invalid. This should be written as DD-SSS-XX-Z, where DD is the deployment type (RT, GA, or GR), SSS is the site id (_________), XX is a number from 01-99, and Z is a letter from A-Z.

action_invalid: the action is invalid. It must be one of the following values _____________.

deploy_invalid: deployment type is invalid. It must be one of the following values _____________.

stn_deploy_nomatch: the deployment type and the station ID do not match.

wpt_invalid: the waypoint entry is invalid************

lat_invalid: latitude value invalid. Must be in decimal degrees with at least 5 (xx.xxxxxx) and within the range of the data for a given site

lon_invalid: longitude value invalid. Must be in decimal degrees with at least 5 decimals (-xx.xxxxxx)

lat_out_of_range: latitude not within the range of the data for a given site

lon_out_of_range: longitude not within the range of the data for a given site

depth_invalid: depth invalid. Depth is reported in meters in positive values. Make sure it is reported in meters

depth_out_of_range: valid for a given site

crew_invalid: the crew is not entered correctly. Make sure it is written as 2-3 letter initials unique to each person, separated by \', \'.")
}




