############## internal functions ###############################
#' Checks date formats to make sure they are in YYYY-MM-DD format at the correct time
#'
#' @param date a single date entry (as a character string)
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for each entry
.check_date <-  function(date, mandatory = TRUE){
  date_not_entered <- FALSE
  date_invalid <- FALSE
  date_out_of_range <- FALSE

  #check if entered
  if (is.null(date)) {
    date_not_entered <- TRUE
  } else if(is.na(date)){
    date_not_entered <- TRUE
  }
  else if ( as.character(date)==""){
    date_not_entered <- TRUE
  }
  else {
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
  if (mandatory){
    if(date_not_entered)
      report <- paste0(report ,"date_not_entered/")
  }
  if (date_invalid){
    report <- paste0(report ,"date_invalid/")
  }
  if(date_out_of_range){
    report <- paste0(report, "date_out_of_range/")
  }

  return(report)
}

#' Checks manually-input time entries
#'
#' @param time a single time entry (as a character string)
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status
.check_time <- function(time, mandatory = TRUE){

  time_not_entered <- FALSE
  time_invalid <- FALSE

  #checking it exists
  if (is.null(time)){
    time_not_entered <- TRUE
  }else if (is.na(time) | !is.character(time) | time=="") {
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
  if (mandatory){
    if(time_not_entered){
      report <- paste0(report, "time_not_entered/")
    }
  }
  if (time_invalid)
    report <- paste0(report, "time_invalid/")
  return (report)
}

#' Checks site ID to make sure it is within the list of allowed values
#'
#' @param site a single site entry (as a character string)
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for each entry
.check_site <-  function(site, mandatory = TRUE) {
  site_not_entered <- FALSE
  site_invalid <- FALSE

  if (is.na(site) | !is.character(site) | site=="") {
    site_not_entered <- TRUE
  } else if(!(site %in% sites)){
    site_invalid <- TRUE
  }

  #return report
  report <- ""
  if (mandatory){
    if (site_not_entered)
      report <- paste0(report, "site_not_entered/")
  }
  if(site_invalid)
    report <- paste0(report, "site_invalid/")

  return(report)
}

#' Checks equipment type to make sure it is within the list of allowed values
#'
#' @param equip a single equipment entry (as a character string)
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for each entry
.check_equip <-  function(equip, mandatory = TRUE){
  equip_not_entered <- FALSE
  equip_invalid <- FALSE

  if (is.na(equip) | equip=="") {
    equip_not_entered <- TRUE
  } else if(!(equip %in% equip_types)){
    equip_invalid <- TRUE
  }

  #return report
  report <- ""
  if (mandatory){
    if (equip_not_entered)
      report <- paste0(report, "equip_not_entered/")
  }
  if(equip_invalid)
    report <- paste0(report, "equip_invalid/")

  return(report)
}

#' Checks the serial ID to make sure it is listed in our equipment list
#'
#' @param serial a single serial id entry (as a character or integer)
#' @param equip_type what category of equipment is to be checked (ex: tag, VR2Tx, etc.)
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for each entry
.check_serial <- function(serial, equip_type = "all", mandatory = TRUE){
  serial_not_entered <- FALSE
  serial_invalid <- FALSE
  serial_equip_nomatch <- FALSE

  if(length(equip_type) == 0){
    equip_type <- "all"
  } else if(missing(equip_type)){
    equip_type <- "all"
  } else if (is.null(equip_type)){
    equip_type <- "all"
  } else if (is.na(equip_type)){
    equip_type <- "all"
  }

  if(equip_type == "all"){
    serial_ref <- c(reference_serial_id$serial_id, "all")
  } else {
    serial_ref <- c(reference_serial_id$serial_id[reference_serial_id$type == equip_type], "all")
  }

  if (equip_type %in% equip_types |equip_type == "all" ){
    if (is.na(serial)|serial=="") {
      serial_not_entered <- TRUE
    } else if(!(serial %in% c(reference_serial_id$serial_id, "all"))){
      serial_invalid <- TRUE
    } else if (!(serial %in% serial_ref)){
      serial_equip_nomatch <- TRUE
    }
  }

  #return report
  report <- ""
  if (mandatory){
    if (serial_not_entered)
      report <- paste0(report, "serial_not_entered/")
  }
  if(serial_invalid)
    report <- paste0(report, "serial_invalid/")
  if(serial_equip_nomatch)
    report <- paste0(report, "serial_equip_nomatch/")

  return(report)
}

#' Checks to make sure the station ID follows a valid format
#'
#' @param stnid a single station ID entry entry of the equipment type (as a character)
#' @param deploy a single station ID entry entry of the equipment type (as a character)
#'
#' @returns a report of the check status for the entry
.check_stnid <- function(stnid, deploy = NA){
  stnid_invalid <- FALSE
  stnid_deploy_nomatch <- FALSE
  stnid_site_nomatch <- FALSE

  if(! (is.na(stnid) | stnid=="" )){
    st_deploy <- substring(stnid, 1, 2)
    st_site <- substring(stnid, 4, 6)
    st_id_num <- substring(stnid, 8, 9)
    st_id_let <- substring(stnid, 11, 11)
    st_dash <- c(substring(stnid, 3, 3), substring(stnid, 7, 7), substring(stnid, 10, 10))

    if(length(stnid)>11){
      stnid_invalid <- TRUE
    }else if( !(st_deploy %in% deploy_types[-4])){
      stnid_invalid <- TRUE
      #checking site (list of site ids, excluding general "base", "lab", and "other")
    } else if (!(st_site %in% sites)){
      stnid_invalid <- TRUE
    } else if (st_site %in% c("lab", "base","other")) {
      stnid_invalid <- TRUE
      #checking number
    } else if (is.na(as.numeric(st_id_num))){
      stnid_invalid <- TRUE
    } else if (as.numeric(st_id_num) <1 | as.numeric(st_id_num) >99){
      stnid_invalid <- TRUE
      #checking id_let based on unicode order
    } else if ( !(st_id_let %in% LETTERS)) {
      stnid_invalid <- TRUE
    } else if(any(st_dash != "-")){
      stnid_invalid <- TRUE
    }

    #checking that station matches site and deplyment type
    if(deploy %in% deploy_types){
      if(st_deploy != deploy){
        stnid_deploy_nomatch <- TRUE
      }
    }
  }

  report <- ""
  if (stnid_invalid) {
    report <- paste0(report, "stnid_invalid/")
  } else if(stnid_deploy_nomatch){
    report <- paste0(report, "stnid_deploy_nomatch/")
  }

  return(report)
}

#' Checks to make sure the action is one of the permitted entries
#'
#' @param action a single station ID entry entry of the equipment type (as a character)
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_action <- function(action, mandatory = TRUE){
  action_not_entered <- FALSE
  action_invalid <- FALSE

  if (is.na(action)| action==""){
    action_not_entered <- TRUE
  } else if (!(action %in% equipment_actions)){
    action_invalid <- TRUE
  }

  report <- ""
  if (mandatory){
    if(action_not_entered){
      report <- paste0(report, "action_not_entered/")
    }
  }
  if(action_invalid) {
    report <- paste0(report, "action_invalid/")
  }

  return(report)
}

#' Checks to make sure the deployment type is entered and is one of the permitted entries
#'
#' @param deploy a single station ID entry entry of the equipment type (as a character)
#' @param site site ID
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_deploy <- function(deploy, site, mandatory = FALSE){
  deploy_not_entered <- FALSE
  deploy_invalid <- FALSE

  if (site %in% sites[(1:I(length(sites)-3))])
    if (is.na(deploy) | deploy=="") {
      deploy_not_entered <- TRUE
    }

  if(! (is.na(deploy) | deploy=="")) {
    if(! (deploy %in% deploy_types)){
      deploy_invalid <- TRUE
    }
  }

  #return report
  report <- ""
  if (mandatory){
    if (deploy_not_entered)
      report <- paste0(report, "deploy_not_entered/")
  }
  if(deploy_invalid)
    report <- paste0(report, "deploy_invalid/")

  return(report)
}

#' Checks to make sure the waypoints are entered in an acceptable format
#'
#' CURRENTLY NOT CHECKING ANYTHING> POSSIBLE OPTIONS: checking for the presence of special cahracters, checking from proper comma delimitations
#' @param wpt a single entry of waypoint names as a comma-separated string
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_wpt <- function(wpt, mandatory = FALSE){
  invalid_wpt <- FALSE

  # possible gps special characters ! " # $ % & ' ( ) * + , - . / : ; < > = ? @ [ ] \ ^ _ ` { } | ~
  report <- ""
  if(invalid_wpt){
    report <- paste0(report, "invalid_wpt/")
  }

  return(report)
}

#' Checks to make sure the gps latitude is within sensible bounds
#'
#' @param lat a single numeric latitude value
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_lat <- function(lat, mandatory = FALSE){
  lat_not_entered <- FALSE
  lat_invalid <- FALSE
  lat_out_of_range <- FALSE

  if (is.na(lat) | lat=="") {
    lat_not_entered <- TRUE
  }

  if(! is.na(lat)) {
    if(is.na(as.numeric(lat))){
      lat_invalid <- TRUE
    } else if (as.numeric(lat) < 45.3 | as.numeric(lat) > 55.5 ){
      lat_out_of_range <- TRUE
    }
  }

  report <- ""
  if (mandatory){
    if(lat_not_entered){
      report <- paste0(report, "lat_not_entered/")
    }
  }
  if(lat_invalid){
    report <- paste0(report, "lat_invalid/")
  }
  if (lat_out_of_range){
    report <- paste0(report, "lat_out_of_range/")
  }

  return(report)
}

#' Checks to make sure the gps longitude is within sensible bounds
#'
#' @param lon a single numeric longitude value
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_lon <- function(lon, mandatory = FALSE){
  lon_not_entered <- FALSE
  lon_invalid <- FALSE
  lon_out_of_range <- FALSE

  if(!(is.na(lon) | lon == "")){
    if(is.na(as.numeric(lon))){
      lon_invalid <- TRUE
    } else if (as.numeric(lon) < -79.5 | as.numeric(lon) > -72.5 ){
      lon_out_of_range <- TRUE
    }
  } else {
    lon_not_entered <- TRUE
  }

  report <- ""
  if (mandatory){
    if(lon_not_entered){
      report <- paste0(report, "lon_not_entered/")
    }
  }
  if(lon_invalid){
    report <- paste0(report, "lon_invalid/")
  }
  if (lon_out_of_range){
    report <- paste0(report, "lon_out_of_range/")
  }

  return(report)
}

#' Checks to make sure the depth is within sensible bounds
#'
#' @param depth a single numeric depth value
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_depth <- function(depth, mandatory = FALSE){
  depth_invalid <- FALSE
  depth_out_of_range <- FALSE

  if(! is.na(depth)){
    if(is.na(as.numeric(depth))){
      depth_invalid <- TRUE
    } else if (as.numeric(depth) < 0 | as.numeric(depth) > 30){
      depth_out_of_range <- TRUE
    }
  }

  report <- ""

  if(depth_invalid){
    report <- paste0(report, "depth_invalid/")
  }
  if (depth_out_of_range){
    report <- paste0(report, "depth_out_of_range/")
  }

  return(report)
}

#' Checks to make sure that the crew is entered and comma separated
#'
#' @param crew a single station ID entry entry of the equipment type (as a character)
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_crew <- function(crew, mandatory = TRUE){
  crew_not_entered <- FALSE
  crew_invalid <- FALSE

  #checking that crew was entered
  if(is.na(crew) | crew==""){
    crew_not_entered <- TRUE

    #checking that all crew codes are comma separated and are 2-3 letter codes
  } else {
    commas <- c(unlist(gregexpr(',', crew)), nchar(crew) + 1)
    commas_dist <- diff(sort(commas))
    num_invalid <- sum(commas_dist>5 | commas_dist<3)

    if(num_invalid>0){
      crew_invalid <- TRUE
    }
  }

  report <- ""
  if (mandatory){
    if (crew_not_entered)
      report <- paste0(report, "crew_not_entered/")
  }
  if(crew_invalid)
    report <- paste0(report, "crew_invalid/")

  return(report)
}

#' Checks to make sure that the capture method one of the accepted values and is filled in
#'
#' @param capture_method a single station ID entry entry of the equipment type (as a character)
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_capture_method <- function(capture_method, mandatory = TRUE){
  capture_not_entered <- FALSE
  capture_invalid <- FALSE

  if (is.na(capture_method) | capture_method=="") {
    capture_not_entered <- TRUE
  } else if(!(capture_method %in% capture_methods)){
    capture_invalid <- TRUE
  }

  #return report
  report <- ""
  if (mandatory){
    if (capture_not_entered)
      report <- paste0(report, "capture_not_entered/")
  }
  if(capture_invalid)
    report <- paste0(report, "capture_invalid/")

  return(report)
}

#' Checks to make sure that the capture method one of the accepted values and is filled in
#'
#' @param fykeid a single fyke id number entry
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_fykeid <- function(fykeid, mandatory = FALSE){
  fykeid_not_entered <- FALSE
  fykeid_invalid <- FALSE

  fyke_let <- substring(fykeid, 1, 2)
  fyke_num <- substring(fykeid, 3, 100)

  if (is.na(fykeid) | fykeid=="") {
    fykeid_not_entered <- TRUE
  } else if(!(stringr::str_detect(fyke_let, "^[:upper:]+$")) | is.na(as.numeric(fyke_num))){
    fykeid_invalid <- TRUE
  } else if( as.numeric(fyke_num)>99){
    fykeid_invalid <- TRUE
  }

  #return report
  report <- ""
  if (mandatory){
    if (fykeid_not_entered)
      report <- paste0(report, "fykeid_not_entered/")
  }
  if(fykeid_invalid)
    report <- paste0(report, "fykeid_invalid/")

  return(report)
}

#' Checks to make sure that the fish species is of the accepted values and is filled in
#'
#' @param species a single station ID entry entry of the equipment type (as a character)
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_species <- function(species, mandatory = TRUE){
  species_not_entered <- FALSE
  species_invalid <- FALSE

  if (is.na(species) | species=="") {
    species_not_entered <- TRUE
  } else if(!(species %in% species_codes)){
    species_invalid <- TRUE
  }

  #return report
  report <- ""
  if (mandatory){
    if (species_not_entered)
      report <- paste0(report, "species_not_entered/")
  }
  if(species_invalid)
    report <- paste0(report, "species_invalid/")

  return(report)
}

#' Checks to make sure the temp is within sensible bounds
#'
#' @param temp a single numeric temp value
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_temp <- function(temp, mandatory = FALSE){
  temp_not_entered <-  FALSE
  temp_invalid <- FALSE
  temp_out_of_range <- FALSE

  if(is.na(temp) | temp==""){
    temp_not_entered <- TRUE
  } else if(is.na(as.numeric(temp))){
    temp_invalid <- TRUE
  } else if (as.numeric(temp) < I(-4) | as.numeric(temp) > 25){
    temp_out_of_range <- TRUE
  }


  report <- ""
  if(mandatory){
    if (temp_not_entered){
      report <- paste0(report, "temp_not_entered/")
    }
  }
  if(temp_invalid){
    report <- paste0(report, "temp_invalid/")
  }
  if (temp_out_of_range){
    report <- paste0(report, "temp_out_of_range/")
  }

  return(report)
}

#' Checks whether the conditions code has been entered
#'
#' @param condition a single condition value to be checked
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_condition <- function(condition, mandatory = TRUE){
  condition_not_entered <- FALSE
  condition_invalid <- FALSE

    if (is.na(condition) | condition=="") {
      condition_not_entered <- TRUE
    } else {
    if(! (condition %in% condition_codes)){
      condition_invalid <- TRUE
    }
  }

  #return report
  report <- ""
  if (mandatory){
    if (condition_not_entered)
      report <- paste0(report, "condition_not_entered/")
  }
  if(condition_invalid)
    report <- paste0(report, "condition_invalid/")

  return(report)
}

#' Checks to make sure the length is within sensible bounds
#'
#' @param length a single numeric length value
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_length <- function(length, mandatory = TRUE){
  length_not_entered <-  FALSE
  length_invalid <- FALSE
  length_out_of_range <- FALSE

  if(is.na(length) | length==""){
    length_not_entered <- TRUE
  } else if(is.na(as.numeric(length))){
    length_invalid <- TRUE
  } else if (as.numeric(length) < I(150) | as.numeric(length) > 1500){
    length_out_of_range <- TRUE
  }


  report <- ""
  if(mandatory){
    if (length_not_entered){
      report <- paste0(report, "length_not_entered/")
    }
  }
  if(length_invalid){
    report <- paste0(report, "length_invalid/")
  }
  if (length_out_of_range){
    report <- paste0(report, "length_out_of_range/")
  }

  return(report)
}

#' Checks to make sure the weight is within sensible bounds
#'
#' @param weight a single numeric weight value
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_weight <- function(weight, mandatory = TRUE){
  weight_not_entered <-  FALSE
  weight_invalid <- FALSE
  weight_out_of_range <- FALSE

  if(is.na(weight) | weight==""){
    weight_not_entered <- TRUE
  } else if(is.na(as.numeric(weight))){
    weight_invalid <- TRUE
  } else if (as.numeric(weight) < I(85) | as.numeric(weight) > 30000){
    weight_out_of_range <- TRUE
  }

  report <- ""
  if(mandatory){
    if (weight_not_entered){
      report <- paste0(report, "weight_not_entered/")
    }
  }
  if(weight_invalid){
    report <- paste0(report, "weight_invalid/")
  }
  if (weight_out_of_range){
    report <- paste0(report, "weight_out_of_range/")
  }

  return(report)
}

.check_spp_size_match <-  function(species, length, weight){

}

#' Checks to make sure the weight is within sensible bounds
#'
#' @param dna_id a single dna sample ID number
#' @param scale_id a single scale sample ID number
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_dna_scale_id <- function(dna_id, scale_id, mandatory = FALSE){
  dna_id_not_entered <- FALSE
  dna_id_invalid <- FALSE
  scale_id_not_entered <- FALSE
  scale_id_invalid <- FALSE
  dna_scale_nomatch <- FALSE

  if (is.na(dna_id) | dna_id == ""){
    dna_id_not_entered <- TRUE
  } else if(nchar(dna_id)>7){
    dna_id_invalid <- TRUE
  }

  if (is.na(scale_id) | scale_id == ""){
    scale_id_not_entered <- TRUE
  } else if(nchar(scale_id)>6){
    scale_id_invalid <- TRUE
  }

  if(!dna_id_not_entered & !scale_id_not_entered){
    if(dna_id != scale_id){
      dna_scale_nomatch <- TRUE
    }
  }

  report <- ""
  if(mandatory){
    if (dna_id_not_entered){
      report <- paste0(report, "dna_id_not_entered/")
    }
  }
  if (dna_id_invalid){
    report <- paste0(report, "dna_id_invalid/")
  }
  if (mandatory){
    if (scale_id_not_entered){
      report <- paste0(report, "scale_id_not_entered/")
    }
  }
  if (scale_id_invalid){
    report <- paste0(report, "scale_id_invalid/")
  }
  if(dna_scale_nomatch){
    report <- paste0(report, "dna_scale_nomatch/")
  }

  return(report)
}

#' Checks to make sure the sex is entered as on of the approved codes
#'
#' @param sex a single character string sex and maturity code
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_sex <- function(sex, mandatory = TRUE){
  sex_not_entered <- FALSE
  sex_invalid <- FALSE

  if (is.na(sex) | !is.character(sex) | sex=="") {
    sex_not_entered <- TRUE
  } else if(!(sex %in% sex_options)){
    sex_invalid <- TRUE
  }

  #return report
  report <- ""
  if(mandatory){
    if (sex_not_entered)
      report <- paste0(report, "sex_not_entered/")
  }
  if(sex_invalid)
    report <- paste0(report, "sex_invalid/")

  return(report)
}

.check_tag_model <- function(tag_model, mandatory = FALSE){
  tag_model_not_entered <- FALSE
  tag_model_invalid <- FALSE

  if (is.na(tag_model) | tag_model=="") {
    tag_model_not_entered <- TRUE
  } else if(!(tag_model %in% tag_models)){
    tag_model_invalid <- TRUE
  }

  #return report
  report <- ""
  if(mandatory){
    if (tag_model_not_entered)
      report <- paste0(report, "tag_model_not_entered/")
  }
  if(tag_model_invalid)
    report <- paste0(report, "tag_model_invalid/")

  return(report)
}

.check_clove_conc <- function(clove_conc, mandatory = FALSE){
  clove_conc_not_entered <-  FALSE
  clove_conc_invalid <- FALSE
  clove_conc_out_of_range <- FALSE

  if(is.na(clove_conc) | clove_conc==""){
    clove_conc_not_entered <- TRUE
  } else if(is.na(as.numeric(clove_conc))){
    clove_conc_invalid <- TRUE
  } else if (as.numeric(clove_conc) < I(0) | as.numeric(clove_conc) > 5){
    #range max set to 5ml/L, based on MFFP eugenol dose of 60mg/L, with a density of 1.0652 g/mL, *100%
    clove_conc_out_of_range <- TRUE
  }

  report <- ""
  if(mandatory){
    if (clove_conc_not_entered){
      report <- paste0(report, "clove_conc_not_entered/")
    }
  }
  if(clove_conc_invalid){
    report <- paste0(report, "clove_conc_invalid/")
  }
  if (clove_conc_out_of_range){
    report <- paste0(report, "clove_conc_out_of_range/")
  }

  return(report)
}

.check_mort <- function(mort){
  mort_invalid <- FALSE

  if (!(is.na(mort) | mort=="")) {
    if((mort != "yes")){
      mort_invalid <- TRUE
    }
  }

  #return report
  report <- ""
  if(mort_invalid)
    report <- paste0(report, "mort_invalid/")

  return(report)
}

.check_recap <- function(recap, mandatory = FALSE){
  recap_invalid <- FALSE

  if (!(is.na(recap) | recap=="")) {
    if((recap != "yes")){
      recap_invalid <- TRUE
    }
  }

  #return report
  report <- ""
  if(recap_invalid)
    report <- paste0(report, "recap_invalid/")

  return(report)
}

#' Checks to make sure that entries that require single initials are correctly filled out
#'
#' @param initials a single 2 or 3-letter initials code
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_single_initials <- function(initials, mandatory = TRUE){
  initials_not_entered <- FALSE
  intials_invalid <- FALSE

  if(initials=="" | is.na(initials)){
    initials_not_entered <- TRUE

    #checking if only a three-letter code
  } else if (nchar(initials)<2 |nchar(initials)>3) {
    intials_invalid <- TRUE

    #check that it is all only capital letters (no lowercase or other characters)
  } else if (! (stringr::str_detect(initials, "^[:upper:]+$"))) {
    intials_invalid <- TRUE
  }

  report <- ""
  if(mandatory){
    if(initials_not_entered){
      report <- paste0(report, "initials_not_entered/")
    }
  }
  if (intials_invalid) {
    report <- paste0(report, "intials_invalid/")
  }

  return(report)
}

#' Checks to make sure the sex is entered as on of the approvd codes
#'
#' @param net_action a single character string sex and maturity code
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_net_action <- function(net_action, mandatory = TRUE){
  net_action_not_entered <- FALSE
  net_action_invalid <- FALSE

  if (is.na(net_action) | net_action=="") {
    net_action_not_entered <- TRUE
  } else if(!(net_action %in% fyke_actions)){
    net_action_invalid <- TRUE
  }

  #return report
  report <- ""
  if(mandatory){
    if (net_action_not_entered)
      report <- paste0(report, "net_action_not_entered/")
  }
  if(net_action_invalid)
    report <- paste0(report, "net_action_invalid/")

  return(report)
}

#' Checks to make sure the sex is entered as on of the approved codes
#'
#' @param fish_caught a single character string of yes or no
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#' @returns a report of the check status for the entry
.check_fish_caught <- function(fish_caught, mandatory = TRUE){
  fish_caught_not_entered <- FALSE
  fish_caught_invalid <- FALSE

  if (is.na(fish_caught) | fish_caught=="") {
    fish_caught_not_entered <- TRUE
  } else if(!(fish_caught %in% c("yes", "no"))){
    fish_caught_invalid <- TRUE
  }

  #return report
  report <- ""
  if(mandatory){
    if (fish_caught_not_entered)
      report <- paste0(report, "fish_caught_not_entered/")
  }
  if(fish_caught_invalid)
    report <- paste0(report, "fish_caught_invalid/")

  return(report)
}

#' Checks to make sure the length is within sensible bounds
#'
#' @param nrods a single numeric length value
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#' @param fishtype if cast-netting or angling were used
#'
#' @returns a report of the check status for the entry
.check_nrods_or_nets <- function(nrods, mandatory = TRUE, fishtype = c("nrods", "nnets")){
  fishtype <-  match.arg(fishtype)

  not_entered <-  FALSE
  invalid <- FALSE
  out_of_range <- FALSE

  if(is.na(nrods) | nrods==""){
    not_entered <- TRUE
  } else if(is.na(as.numeric(nrods))){
    invalid <- TRUE
  } else if (as.numeric(nrods) < I(1) | as.numeric(nrods) > 20){
    out_of_range <- TRUE
  }

  report <- ""
  if(mandatory){
    if (not_entered){
      report <- paste0(report, fishtype, "_not_entered/")
    }
  }
  if(invalid){
    report <- paste0(report, fishtype, "_invalid/")
  }
  if (out_of_range){
    report <- paste0(report, fishtype, "_out_of_range/")
  }

  return(report)
}

#' Checks to make sure the deployment type is entered and is one of the permitted entries
#'
#' @param rt a single rt type code
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_rt <- function(rt, mandatory = TRUE){
  rt_not_entered <- FALSE
  rt_invalid <- FALSE

  if (is.na(rt) | rt=="") {
    rt_not_entered <- TRUE
  }

  if(! (is.na(rt) | rt=="")) {
    if(! (rt %in% rt_types)){
      rt_invalid <- TRUE
    }
  }

  #return report
  report <- ""
  if (mandatory){
    if (rt_not_entered)
      report <- paste0(report, "rt_not_entered/")
  }
  if(rt_invalid)
    report <- paste0(report, "rt_invalid/")

  return(report)
}

#' Checks to make sure the depth is within sensible bounds
#'
#' @param dist a single numeric distance value
#' @param mandatory TRUE/FALSE designates if the data to be checked is mandatory
#'
#' @returns a report of the check status for the entry
.check_distance <- function(dist, mandatory = FALSE){
  dist_invalid <- FALSE
  dist_out_of_range <- FALSE

  if(! is.na(dist)){
    if(is.na(as.numeric(dist))){
      dist_invalid <- TRUE
    } else if (as.numeric(dist) < 0 | as.numeric(dist) > 3000){
      dist_out_of_range <- TRUE
    }
  }

  report <- ""

  if(dist_invalid){
    report <- paste0(report, "dist_invalid/")
  }
  if (dist_out_of_range){
    report <- paste0(report, "dist_out_of_range/")
  }
  return(report)
}

