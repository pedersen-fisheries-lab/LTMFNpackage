#' Exports a csv with tagging data formatted for the OTN metadata sheet
#'
#' This is useful for uploading data to Fathom Central and sharing data with OTN
#'
#' @param fish fish database to extract data from
#' @param sink the filepath where to save the csv file
#' @param startdate/enddate Character string or POSIXt date and time stamp to filter only a given date range of data
#' @param sp (optional) the species code to filter for
#' @param tag_models (optional) the tag model to filter for
#' @param tagger (optional) the tagger/surgeon to filter for (as a single initial or as a vector of initials)
#' @returns returns a reformated detections database.
#'
#' @export otn_tagging_metadata
otn_tagging_metadata <- function(fish, sink, startdate="1970-01-01", enddate="3000-01-01", sp=c("COAR","COCL","ESLU","MOHU","MOSA","OSMO","PEFL","SAFO","SANA","SAVI","TITI"), tag_models=NULL, tagger=NULL){
  #checking that fish is an appropriate dataframe
  if(!is.data.frame(fish) |
     ! (all(c("date","site","capture_method", "fyke_id","capture_time","capture_lat",
          "capture_lon","species","temp_c","capture_cond","length_mm","weight_g",
          "dna_id","scale_id","sex","tag_serial","tag_model","clove_conc",
          "anesth_start","surg_start","surg_end","recov_end","release_time","release_lat",
          "release_lon","release_cond","mort","recap","surgeon","assistant",
          "recorder","crew","comments","data_flag")%in% colnames(fish)))){
    stop("ensure that the fish object is a dataframe of the cleaned fish database")
  }

  fish <-dplyr::filter(fish, is.na(mort),
                       recap !="yes"|is.na(recap))
  #checking and filtering date
  if(!(is.character(startdate)|lubridate::is.POSIXct(startdate)|lubridate::is.POSIXt(startdate))|
     !(is.character(enddate)|lubridate::is.POSIXct(enddate)|lubridate::is.POSIXt(enddate))){
    #INCLUDE DATE CHECKING CODE
    stop("Please enter the start and enddate either as a character string in the format \"YYYY-MM-DD HH:MM:SS\", or as a POSIXct or POSIXt date object. These date classes can be obtained using the lubridate package")
  } else {
    if(is.character(startdate)){
      startdate <- lubridate::ymd(startdate)
    }
    if(is.character(enddate)){
      enddate <- lubridate::ymd(enddate)
    }
    fish <- dplyr::filter(fish, lubridate::as_date(date)>=startdate,
                   lubridate::as_date(date)<=enddate)
  }

  #sp
  if(all(sp %in% LTMFNpackage::species_codes)){
    fish <- dplyr::filter(fish, species %in% sp)
  }else{
    stop("Please enter a valid species codes. Valid species codes are listed in LTMFNpackage::species_codes")
  }

  #tag_model
  if(!is.null(tag_models)){
    if(all(tag_models %in% LTMFNpackage::tag_models)){
      fish <- dplyr::filter(fish, tag_model %in% tag_models)
    }else{
      stop("Please enter valid tag_model codes. Valid codes are listed in LTMFNpackage::tag_models")
    }
  }

  #tagger
  if(!is.null(tagger)){
    #tagger present in the fish database?
    tagger_valid <- any(fish$surgeon %in% tagger)
    if(tagger_valid){
      fish <- dplyr::filter(fish, surgeon %in% tagger)
    } else {
      stop("None of the taggers matched the surgeons listed in the database. Please make sure they are formatted correctly and matched the pre-assigned initials (Ex: ND for Natalie Dupont")
    }
  }

  otn_fish <- matrix(nrow = nrow(fish), ncol = length(otn_tagging_metadata_headers))
  otn_fish <- data.frame(otn_fish)
  colnames(otn_fish) <- otn_tagging_metadata_headers
  otn_fish$ANIMAL_ID <- paste0(fish$species, "_", fish$site, "_", fish$date, "_", fish$tag_serial)
  otn_fish$TAG_TYPE <- "acoustic"
  otn_fish$TAG_MANUFACTURER <- "VEMCO"
  otn_fish$TAG_MODEL <- dplyr::case_match(.x = fish$tag_model, "V7" ~ "V7", "V8" ~ "V8-4x", "V9" ~ "V9-1x", "V13"~"V13-1x", .default = fish$tag_model)
  otn_fish$TAG_SERIAL_NUMBER <- as.character(fish$tag_serial)
  otn_fish$TAG_ID_CODE <- as.numeric(substring(reference_serial_id$transmitter_id[match(fish$tag_serial,reference_serial_id$serial_id )] ,10  , 17))
  otn_fish$TAG_CODE_SPACE <- substring(reference_serial_id$transmitter_id[match(fish$tag_serial,reference_serial_id$serial_id )] ,1 , 8)
  otn_fish$TAG_IMPLANT_TYPE <- "internal"
  otn_fish$TAG_IMPLANT_METHOD <- "inserted into abdominal cavity closed by 2-4 sutures of polydioxanone filament"
  otn_fish$TAG_ACTIVATION_DATE <- NA
  otn_fish$EST_TAG_LIFE <- NA
  otn_fish$TAGGER <- fish$surgeon
  otn_fish$TAG_OWNER_PI <- "Eric Pedersen"
  otn_fish$TAG_OWNER_ORGANIZATION <- "Concordia University"
  otn_fish$COMMON_NAME_E <- dplyr::case_match(fish$species, "COAR"~"cisco", "COCL"~"lake whitefish", "ESLU"~ "northern pike",
                                              "MOHU"~"copper redhorse", "MOSA"~"striped bass", "OSMO"~"rainbow smelt", "PEFL"~"yellow perch",
                                              "SAFO"~"brook trout", "SANA"~"lake trout", "SAVI"~"walleye", "TITI"~"tench", .default = fish$species)
  otn_fish$SCIENTIFIC_NAME <-  dplyr::case_match(fish$species, "COAR"~"Coregonus artedii", "COCL"~"Coregonus clupeaformis", "ESLU"~ "Esox lucius",
                                                 "MOHU"~"Moxostoma hubbsi", "MOSA"~"Morone saxatilis", "OSMO"~"Osmerus mordax", "PEFL"~"Perca flavescens",
                                                 "SAFO"~"Salvelinus fontinalis", "SANA"~"Salvelinus namaycush", "SAVI"~"Sander vitreus", "TITI"~"Tinca tinca", .default = fish$species)
  otn_fish$CAPTURE_LOCATION <- fish$site
  otn_fish$CAPTURE_LATITUDE <- round(as.numeric(fish$capture_lat),digits = 5)
  otn_fish$CAPTURE_LONGITUDE <- round(as.numeric(fish$capture_lon), digits = 5)
  otn_fish$WILD_OR_HATCHERY <- "W"
  otn_fish$STOCK <-dplyr::case_match(fish$site, "JEA"~"Eastmain river", "JGR"~"Great Whale river", "JMA" ~ "Maquatua river", "JRU"~"Rupert river", "JOY" ~ "Joyce Pond", "LEA"~ "Lac-Saint-Pierre - East", "LWE" ~ "Lac-Saint-Pierre - West", .default = fish$site)
  otn_fish$LENGTH <- as.numeric(fish$length_mm)/1000
  otn_fish$WEIGHT <- as.numeric(fish$weight_g)/1000
  otn_fish$LENGTH_TYPE <- "fork length"
  otn_fish$LENGTH2 <- NA
  otn_fish$LENGTH2_TYPE <- NA
  otn_fish$LIFE_STAGE <-  dplyr::case_match(fish$sex,"MM"~"sexually mature adult", "MF"~"sexually mature adult", "IM"~"sexually immature adult", "IF"~"sexually immature adult", "unk"~"adult", .default = "adult")
  otn_fish$AGE <- "unknown"
  otn_fish$AGE_UNITS <- NA
  otn_fish$SEX <- dplyr::case_match(fish$sex, "MM"~"M", "IM"~"M", "MF"~"F", "IF"~"F", "unk"~"U")
  otn_fish$DNA_SAMPLE_TAKEN <- "yes"
  otn_fish$TREATMENT_TYPE <- NA
  otn_fish$RELEASE_GROUP <- NA
  otn_fish$RELEASE_LOCATION <- fish$site
  otn_fish$RELEASE_LATITUDE <- round(as.numeric(fish$release_lat),digits = 5)
  otn_fish$RELEASE_LONGITUDE <- round(as.numeric(fish$release_lon),digits = 5)
  otn_fish$UTC_RELEASE_DATE_TIME <- format(lubridate::with_tz(lubridate::ymd_hms(paste0(fish$date, fish$release_time), tz = "America/Montreal"), tzone = "UTC"), "%Y-%m-%dT%H:%M:%S")
  otn_fish$HARVEST_DATE <- NA
  otn_fish$CAPTURE_DEPTH <- NA
  otn_fish$TEMPERATURE_CHANGE <- NA
  otn_fish$HOLDING_TEMPERATURE <- NA
  otn_fish$PREOP_HOLD_PERIOD <- as.numeric(lubridate::hms(fish$surg_start)-lubridate::hms(fish$capture_time))/3600
  otn_fish$POSTOP_HOLD_PERIOD <- as.numeric(lubridate::hms(fish$release_time)-lubridate::hms(fish$surg_end))/3600
  otn_fish$SURGERY_LOCATION <- fish$site
  otn_fish$SURGERY_LATITUDE <- NA
  otn_fish$SURGERY_LONGITUDE <- NA
  otn_fish$SEDATIVE <- NA
  otn_fish$SEDATIVE_CONCENTRATION <- NA
  otn_fish$ANAESTHETIC <- "clove oil"
  otn_fish$BUFFER <- NA
  otn_fish$ANAESTHETIC_CONCENTRATION <- as.numeric(fish$clove_conc)*1000
  otn_fish$BUFFER_CONCENTRATION_IN_ANAESTHETIC <- NA
  otn_fish$ANAESTHETIC_CONCENTRATION_IN_RECIRCULATION <- NA
  otn_fish$BUFFER_CONCENTRATION_IN_RECIRCULATION <- NA
  otn_fish$DISSOLVED_OXYGEN <- NA
  otn_fish$COMMENTS <- fish$comments

  write.csv(x = otn_fish, file = sink)
}

#' Exports a csv with tagging data formatted for the OTN metadata sheet
#'
#' This is useful for uploading data to Fathom Central and sharing data with OTN
#'
#' @param equipmentlog equipmentlog database to extract data from
#' @param sink the filepath where to save the csv file
#' @param startdate/enddate Character string or POSIXt date and time stamp to filter only a given date range of data
#' @param sites (optional) the site codes to filter for
#' @param equipment_types (optional) the equipment to filter for
#' @param lead_tech Last Name, First initial of lead technician responsible for deployment of these receivers
#' @returns returns a reformated detections database.
#'
#' @export otn_instrument_deployment
otn_instrument_deployment <- function(equipmentlog, sink, startdate="1970-01-01", enddate="3000-01-01", sites=c("JEA","JGR","JMA","JRU","JOY","LEA","LWE"), equipment_types=c("VR2Tx", "VR2W", "RTtag", "AMchl", "AMdo",  "AMsal"), lead_tech = NULL){
  #checking that equipmentlog is an appropriate dataframe
  if(!is.data.frame(equipmentlog) |
     ! (all(c("date","site","equip_type","serial_id","station_id","deploy_type","action",
          "time","wpt","lat","lon","depth_m","crew","data_flag") %in% colnames(equipmentlog) ))){
    stop("ensure that the equipmentlog object is a dataframe of the cleaned equipmentlog database")
  }


  equipmentlog <-dplyr::filter(equipmentlog, action %in% c("deployed", "retrieved", "dataDownload"))

  #checking and filtering date
  if(!(is.character(startdate) | lubridate::is.POSIXct(startdate) | lubridate::is.Date(startdate))|
     !(is.character(enddate)|lubridate::is.POSIXct(enddate)|lubridate::is.Date(enddate))){
    #INCLUDE DATE CHECKING CODE
    stop("Please enter the start and enddate either as a character string in the format \"YYYY-MM-DD HH:MM:SS\", or as a POSIXct or POSIXt date object. These date classes can be obtained using the lubridate package")
  } else {
    if(is.character(startdate)){
      startdate <- lubridate::ymd(startdate)
    }
    if(is.character(enddate)){
      enddate <- lubridate::ymd(enddate)
    }
    equipmentlog <- dplyr::filter(equipmentlog, lubridate::as_date(date)>=startdate,
                          lubridate::as_date(date)<=enddate)
  }

  #sites
  if(all(sites %in% LTMFNpackage::sites)){
    equipmentlog <- dplyr::filter(equipmentlog, site %in% sites)
  }else{
    stop("Please enter valid sites codes. Valid species codes are listed in LTMFNpackage::sites")
  }

  #equipment
  if(!is.null(equipment_types)){
    if(all(equipment_types %in% LTMFNpackage::equip_types)){
      equipmentlog <- dplyr::filter(equipmentlog, equip_type %in% equipment_types)
    }else{
      stop("Please enter valid equipment_types codes. Valid codes are listed in LTMFNpackage::equip_types")
    }
  }

  #lead_tech
  if(is.null(lead_tech)){
    equipmentlog$lead_tech <- substring(equipmentlog$crew, 1,3)
  } else(
    equipmentlog$lead_tech <- lead_tech
  )

  deployments <- dplyr::filter(equipmentlog, action=="deployed")
  retrievals <- dplyr::filter(equipmentlog, action == "retrieved")
  downloads <- dplyr::filter(equipmentlog, action == "dataDownload")
  colnames(downloads)[!colnames(downloads)%in% c( "serial_id","station_id")] <- paste0( colnames(downloads)[!colnames(downloads)%in% c( "serial_id","station_id")], "_dd")

  equipmentlog_short <- merge(deployments, retrievals, by = c("station_id", "serial_id"), all = TRUE, suffixes = c("_d", "_r"))
  equipmentlog_short <- merge(equipmentlog_short, downloads, by = c("station_id", "serial_id"), all = TRUE, suffixes = c("", "_dd"))

  otn_deployment <- matrix(nrow = nrow(equipmentlog_short), ncol = length(otn_instrument_deployment_headers))
  otn_deployment <- data.frame(otn_deployment)
  colnames(otn_deployment) <- otn_instrument_deployment_headers

  otn_deployment$OTN_ARRAY <- rep(NA, nrow(otn_deployment))
  otn_deployment$STATION_NO <- equipmentlog_short$station_id
  otn_deployment$DEPLOY_DATE_TIME <- format(lubridate::with_tz(lubridate::ymd_hms(paste0(equipmentlog_short$date_d, " ", equipmentlog_short$time_d), tz = "America/Montreal"), tzone = "UTC"), "%Y-%m-%dT%H:%M:%S")
  otn_deployment$DEPLOY_LAT <- round(as.numeric(equipmentlog_short$lat_d), digits = 5)
  otn_deployment$DEPLOY_LONG <- round(as.numeric(equipmentlog_short$lon_d), digits = 5)
  otn_deployment$BOTTOM_DEPTH <- equipmentlog_short$depth_m_d
  otn_deployment$RISER_LENGTH <- NA
  otn_deployment$INSTRUMENT_DEPTH <- NA
  otn_deployment$INS_MODEL_NO <- reference_serial_id$product_id[match(equipmentlog_short$serial_id,reference_serial_id$serial_id )]
  otn_deployment$INS_SERIAL_NO <- equipmentlog_short$serial_id
  otn_deployment$CODE_SET <- "Generation 2"
  otn_deployment$TRANSMITTER <- reference_serial_id$transmitter_id[match(equipmentlog_short$serial_id,reference_serial_id$serial_id )]
  otn_deployment$TRANSMIT_MODEL <- reference_serial_id$product_id[match(equipmentlog_short$serial_id,reference_serial_id$serial_id )]
  otn_deployment$AR_MODEL_NO <- NA
  otn_deployment$AR_SERIAL_NO <- NA
  otn_deployment$DEPLOYED_BY <- equipmentlog_short$lead_tech_d
  otn_deployment$RECOVERED <- ifelse(is.na(equipmentlog_short$date_r), "TO BE FILLED", "y")
  otn_deployment$RECOVER_DATE_TIME <- format(lubridate::with_tz(lubridate::ymd_hms(paste0(equipmentlog_short$date_r, " ", equipmentlog_short$time_r), tz = "America/Montreal"), tzone = "UTC"), "%Y-%m-%dT%H:%M:%S")
  otn_deployment$RECOVER_LAT <- round(as.numeric(equipmentlog_short$lat_r), digits = 5)
  otn_deployment$RECOVER_LONG <- round(as.numeric(equipmentlog_short$lon_r), digits = 5)
  otn_deployment$DATA_DOWNLOADED <- ifelse(is.na(equipmentlog_short$action_dd), "n", "y")
  otn_deployment$DOWNLOAD_DATE_TIME <- format(lubridate::with_tz(lubridate::ymd_hms(paste0(equipmentlog_short$date_dd, " ", equipmentlog_short$time_dd), tz = "America/Montreal"), tzone = "UTC"), "%Y-%m-%dT%H:%M:%S")
  otn_deployment$FILENAME <- NA
  otn_deployment$COMMENTS <- paste0("deployment: ", equipmentlog_short$comment_d, ". retrieval: ", equipmentlog_short$comment_r)

  write.csv(x = otn_deployment, file = sink)
}



