######################### Data checking flow functions ###################
#' Performs comprehensive verification of all excel data entry files within a given folder
#'
#' @param folder_path the path of the folder in which the data entry xlsx sheets are stored. Folder must only contain the correct format of data entry sheets
#' @param return_summary whether to output a summary of data checking to the console
#' @returns creates a folder in which the flagged xlsx files are placed for the tester to look through them and correct data entry mistakes.
#' @export check_dataentry
check_dataentry <- function(folder_path, return_summary = TRUE){

  #Checking that data entry is occurring in the right spot
    #checking device
  if (Sys.info()[6] != "PedersenLab" |
      Sys.info()[4] != "DESKTOP-H8SPKU6"){
    message("Data checking should be performed in the database directory of the Lab Surface Pro computer.
            This device does not seem to be the lab surface pro (NodeName: DESKTOP-H8SPKU6), or it is not signed into the PedersenLab account.
            If you wish to proceed, enter \"Y\" into the console. If not, enter \"N\" into the console")

    user_decision <- readline(prompt = "Would you like to proceed? (enter Y or N): ")

    if(user_decision == "N"){
      stop("Data checking stopped")
    } else if (user_decision == "Y") {
      message("Data checking is proceeding")
    } else {
      stop("User entry is not valid, Data checking halted")
    }
  }
    #checking file directory
  if (getwd() != "C:/Users/PedersenLab/Documents/ltmftn_project/ltmftn_database/internal_database"){
    message(paste0("Your working directory for data checking should be set to \"ltmftn_project/ltmftn_database/internal_database\" on the lab surface pro.
            You can do this with the following line of code: setwd(\"~/ltmftn_project/ltmftn_database/internal_database\").
            The current directory is", getwd(),  ". Please choose one of the following options by entering a number into the console (1, 2, 3):"))

    user_decision <- readline(prompt = "1: change the working directory to \"~/ltmftn_project/ltmftn_database/internal_database\" \n2: stop data checking\n3: proceed with the current working directory")

    if(user_decision == "1"){
      setwd("~/ltmftn_project/ltmftn_database/internal_database")
      message(paste0("Working directory set to: ", getwd()))
    } else if (user_decision == "2") {
      stop("Data checking stopped")
    } else if( user_decision == "3") {
      message("Data checking is proceeding")
    } else {
      stop("User entry is not valid, Data checking halted")
    }
  }

  if(substring(folder_path, nchar(folder_path), nchar(folder_path))!="/"){
    stop("Please include the final / of the folder path")
  }

  #getting all file paths within the folder
  file_names <- list.files(folder_path, pattern=".xlsx")

  flagged_folder_path <- "flagged/"

  #IMPORT AND MERGE ALL DATA ENTRY FILES
    database_list <- lapply(file_names, FUN = function(x) import_database_xl(paste0(folder_path, x)))
    names(database_list) <- file_names

    #check that data entry information is present
      missing_entry_data <- rep(FALSE, times = length(database_list))
      entry_date_invalid <- rep(FALSE, times = length(database_list))
      entry_initials_invalid <- rep(FALSE, times = length(database_list))

      for (i in 1:length(database_list)){
        if(is.null(database_list[[i]]$entry_metadata$date[1]) |
           is.null(database_list[[i]]$entry_metadata$enterer_initials[1])){
          missing_entry_data[i] <- TRUE
        }else{ if(.check_date(as.character(database_list[[i]]$entry_metadata$date[1])) != ""){
          entry_date_invalid[i] <- TRUE
        }

          if(.check_single_initials(database_list[[i]]$entry_metadata$date[1]) != ""){
            entry_initials_invalid[i] <- TRUE
          }
        }
      }

    if(any(missing_entry_data) ){
      stop(paste0("The following files have missing data entry data: ", file_names[missing_entry_data],
                  "both the date and initials of the data enterer are necessary. Please add this data in the entry_metadata sheet"))
    }
    if(entry_date_invalid != ""){
      stop(paste0("The following files have an invalid date for data entry metadata: ", file_names[entry_date_invalid],
                  "Please follow the YYYY-MM-DD format for the date in the entry_metadata sheet"))
    }
    if(entry_initials_invalid != ""){
      stop(paste0("The following files have invalid initials for data entry metadata: ", file_names[entry_initials_invalid],
                  "Please write the data enterer's initials as 2 or 3 letters in the entry_metadata sheet"))
    }

  #adding data entry metadata into each row of data
    database_list <- lapply(X = database_list, FUN = function(list_entry){
      #Extracting the data entry data from the sheet for each database being checked
      date <- list_entry$entry_metadata$date[1]
      enterer <- list_entry$entry_metadata$enterer_initials[1]

      #Adding the data entry data to each sheet for each database being checked
      list_entry <- lapply(list_entry, function(list_entry) {
        list_entry$entry_date <- date
        list_entry$entry_initials <- enterer
        return(list_entry)
      })
      return(list_entry)
    } )

  #merging the separate lists
  names_tables <- names(database_list[[1]])

  database <- purrr::map(names_tables, ~purrr::map_dfr(database_list, purrr::pluck, .x))
  names(database) <- names_tables

  #removing separate entry_matadata dataframe
  #database <- database[setdiff(names(database), "entry_metadata")]

  #checking the data
  database_flagged <- database
  database_flagged$equipment_log <- check_equipment_log(database$equipment_log)
  database_flagged$fish <- check_fish(database$fish)
  database_flagged$fykes <- check_fykes(database$fykes)
  database_flagged$angling <- check_angling(database$angling)
  database_flagged$cast_netting <- check_cast_netting(database$cast_netting)
  database_flagged$range_test <- check_angling(database$range_test)
  database_flagged$gps_records <- check_angling(database$gps_records)

  openxlsx::write.xlsx(x = database_flagged, file = paste0(Sys.Date(), "database_flagged.xlsx"))

  output_message <- paste0("Data entry successfully complete. Flagged data have been output in the ", getwd(), flagged_folder_path, " directory. Make all data entry corrections in this flagged file")

  #creating summary report
  if (return_summary){
    eq_flagged_rows <- which(database_flagged$equipment_log$data_flag != "")
    fish_flagged_rows <- which(database_flagged$fish$data_flag != "")
    fyke_flagged_rows <- which(database_flagged$fykes$data_flag != "")
    angling_flagged_rows <- which(database_flagged$angling$data_flag != "")
    cast_flagged_rows <- which(database_flagged$cast_netting$data_flag != "")
    rt_flagged_rows <- which(database_flagged$range_test$data_flag != "")
    gps_flagged_rows <- which(database_flagged$gps_records$data_flag != "")

    output_message <- paste0(output_message, "Here is a list of the flagged rows in each sheet:",
                             "\nequipment_log rows: ", paste0(eq_flagged_rows, collapse = ", "),
                             "\nfish rows: ", paste0(fish_flagged_rows, collapse = ", "),
                             "\nfyke rows: ",paste0(fyke_flagged_rows, collapse = ", "),
                             "\nangling rows: ", paste0(angling_flagged_rows, collapse = ", "),
                             "\ncast_netting rows: ",paste0(cast_flagged_rows, collapse = ", "),
                             "\nrange_test rows: ", paste0(rt_flagged_rows, collapse = ", "),
                             "\ngps_records rows: ", paste0(gps_flagged_rows, collapse = ", "))

    #data entry summary
    data_enterers <- unique(database_flagged$entry_metadata$enterer_initials)
    all_crew <- unique(c(data_enterers,
                         unlist(stringr::str_split(database_flagged$equipment_log$crew, ", ")),
                         unlist(stringr::str_split(database_flagged$fish$crew, ", ")),
                         unlist(stringr::str_split(database_flagged$fykes$crew, ", ")),
                         unlist(stringr::str_split(database_flagged$angling$crew, ", ")),
                         unlist(stringr::str_split(database_flagged$range_test$crew, ", ")),
                         unlist(stringr::str_split(database_flagged$gps_records$crew, ", "))))
    output_message <- paste0(output_message, "\n\nThe following people participated in the data pipeline: \n",
                             "data entry: ",  paste0(data_enterers, collapse = ", "),
                             "\nall crew: ", paste0(all_crew, collapse = ", "))

  }

  output_message <- paste0("\nFor further data checking, please use the visualize_data_check function")

  return(cat(output_message))
}

######################### Importing and Exporting data  ################################
#' Imports the excel database into R, as a list of multiple tibbles
#'
#' @param path a string designating the directory path of the database excel sheet
#' @returns a report of the check status for the entry
#' @export import_database_xl
import_database_xl <- function(path){

  sheet_names <- readxl::excel_sheets(path)
  database <- lapply(sheet_names[!sheet_names== "README! - Instructions"], function(x) readxl::read_excel(path = path, sheet = x))
  names(database) <- sheet_names[!sheet_names== "README! - Instructions"]

  return(database)
}

#' Appends the checked data entry files to the relevant csv databases
#'
#' @param checked_database the new data entry database that has been checked and is ready to add to the main database
#' @returns a report of the check status for the entry
#' @export append_to_database
append_to_database <- function(checked_database){
  #verify that there are no duplicates

  #import csvs into R
  #merge the checked ones and the orignal main csv
  #check for duplicates (duplicated(merged_db))
  #if duplicates exists, stop and return a message
  # if no duplicates exists, export each to csv with the merging timestamp added
  #return a message summarizing the merging (how many data comments remain)

}

visualize_data_check <- function(database){
  #check dates
  #check initials
  #check
}

######################### Checking each sheet  ################################

#' Performs comprehensive verification on equipment_log
#'
#' @param equipment_log the equipment log tibble
#' @returns an equipment_log tibble with the added data flag column
#' @export check_equipment_log
check_equipment_log <- function(equipment_log){
  eq <- equipment_log
  eq$data_flag <- ""

  eq$data_flag <- eq$data_flag <- apply(eq, 1, function(row) {
    data_flag <- ""
    data_flag <- paste0(data_flag, .check_date(row["date"]))
    data_flag <- paste0(data_flag, .check_site(row["site"]))
    data_flag <- paste0(data_flag, .check_equip(row["equip_type"]))
    data_flag <- paste0(data_flag, .check_serial(row["serial_id"], equip_type = row["equip_type"]))
    data_flag <- paste0(data_flag, .check_stnid(row["station_id"], row["deploy_type"]))
    data_flag <- paste0(data_flag, .check_action(row["action"]))
    data_flag <- paste0(data_flag, .check_deploy(deploy = row["deploy_type"], site = row["site"]))
    data_flag <- paste0(data_flag, .check_time(row["time"]))
    data_flag <- paste0(data_flag, .check_wpt(row["wpt"]))
    data_flag <- paste0(data_flag, .check_lat(row["lat"]))
    data_flag <- paste0(data_flag, .check_lon(row["lon"]))
    data_flag <- paste0(data_flag, .check_depth(row["depth_m"]))
    data_flag <- paste0(data_flag, .check_crew(row["crew"]))

    return(data_flag)
  })

  return(eq)
}

#' Performs comprehensive verification on fish database
#'
#' @param fish the equipment log tibble
#' @returns an fish tibble with the added data flag column
#' @export check_fish
check_fish <- function(fish){
  fish$data_flag <- ""

  if(!(is.na(row["tag_serial"])| row["tag_serial"] == "") & row["recap"] !="yes"){
    tagging <- TRUE
  } else {
    tagging <- FALSE
  }

  fish$data_flag <- fish$data_flag <- apply(fish, 1, function(row) {
    data_flag <- ""
    data_flag <- paste0(data_flag, .check_date(row["date"]))
    data_flag <- paste0(data_flag, .check_site(row["site"]))
    data_flag <- paste0(data_flag, .check_capture_method(row["capture_method"]))
    data_flag <- paste0(data_flag, .check_fykeid(row["fyke_id"]))
    data_flag <- paste0(data_flag, "capt_", .check_time(row["capture_time"], mandatory = FALSE))
    data_flag <- paste0(data_flag, "capt", .check_lat(row["capture_lat"], mandatory = FALSE))
    data_flag <- paste0(data_flag, "capt", .check_lon(row["capture_lon"], mandatory = FALSE))
    data_flag <- paste0(data_flag, .check_species(row["species"]))
    data_flag <- paste0(data_flag, .check_temp(row["temp_c"]))
    data_flag <- paste0(data_flag, "capt_", .check_condition(row["capture_cond"]))
    data_flag <- paste0(data_flag, .check_length(row["length_mm"]))
    data_flag <- paste0(data_flag, .check_weight(row["weight_g"]))
    data_flag <- paste0(data_flag, .check_dna_scale_id(row["dna_id"], row["scale_id"])) #populate fnct
    data_flag <- paste0(data_flag, .check_sex(row["sex"]))
    data_flag <- paste0(data_flag, .check_serial(row["tag_serial"]), equip_type = "tag")
    data_flag <- paste0(data_flag, .check_tag_model(row["tag_model"])) #create fnct
    data_flag <- paste0(data_flag, .check_clove_conc(row["clove_conc"], mandatory = tagging))
    data_flag <- paste0(data_flag, "anesth_", .check_time(row["anesth_start"], mandatory = tagging))
    data_flag <- paste0(data_flag, "surgs_", .check_time(row["surg_start"], mandatory = tagging))
    data_flag <- paste0(data_flag, "surge_", .check_time(row["surg_end"], mandatory = tagging))
    data_flag <- paste0(data_flag, "recov_", .check_time(row["recov_end"], mandatory = tagging))
    data_flag <- paste0(data_flag, "rele_", .check_time(row["release_time"], mandatory = tagging))
    data_flag <- paste0(data_flag, "rele_", .check_lat(row["release_lat"], mandatory = tagging))
    data_flag <- paste0(data_flag, "rele_", .check_lon(row["release_lon"], mandatory = tagging))
    data_flag <- paste0(data_flag, "rele_", .check_condition(row["release_cond"], mandatory = tagging))
    data_flag <- paste0(data_flag, .check_mort(row["mort"], mandatory = tagging))
    data_flag <- paste0(data_flag, .check_recap(row["recap"], mandatory = tagging))
    data_flag <- paste0(data_flag, "surg_",.check_single_initials(row["surgeon"], mandatory = tagging))
    data_flag <- paste0(data_flag, "assi_",.check_single_initials(row["assistant"], mandatory = tagging))
    data_flag <- paste0(data_flag, "reco_",.check_single_initials(row["recorder"], mandatory = tagging))

    data_flag <- paste0(data_flag, .check_crew(row["crew"]))

    return(data_flag)
  })

  return(fish)
}

#' Performs comprehensive verification on fyke database
#'
#' @param fyke the equipment log tibble
#' @returns an angling tibble with the added data flag column
#' @export check_fyke
check_fyke <- function(fyke){
  fyke$data_flag <- ""

  if(row["net_action"] %in% c("set", "retrieved")){
    need_gps <- TRUE
  } else {
    need_gps <- FALSE
  }

  fyke$data_flag <- fyke$data_flag <- apply(fyke, 1, function(row) {
    data_flag <- ""
    data_flag <- paste0(data_flag, .check_date(row["date"]))
    data_flag <- paste0(data_flag, .check_site(row["site"]))
    data_flag <- paste0(data_flag, .check_fykeid(row["fyke_id"], mandatory = TRUE))
    data_flag <- paste0(data_flag, .check_net_action(row["net_action"]))
    data_flag <- paste0(data_flag, .check_lat(row["lat"], mandatory = need_gps))
    data_flag <- paste0(data_flag, .check_lon(row["lon"], mandatory = need_gps))
    data_flag <- paste0(data_flag, "out_", .check_time(row["out_time"]))
    data_flag <- paste0(data_flag, "in_", .check_time(row["in_time"]))
    data_flag <- paste0(data_flag, .check_fish_caught(row["fish_caught"]))
    data_flag <- paste0(data_flag, .check_crew(row["crew"]))

    return(data_flag)
  })
  return(fyke)
}

#' Performs comprehensive verification on angling database
#'
#' @param angling the equipment log tibble
#' @returns an angling tibble with the added data flag column
#' @export check_angling
check_angling <- function(angling){
  angling$data_flag <- ""

  angling$data_flag <- angling$data_flag <- apply(angling, 1, function(row) {
    data_flag <- ""
    data_flag <- paste0(data_flag, .check_date(row["date"]))
    data_flag <- paste0(data_flag, .check_site(row["site"]))
    data_flag <- paste0(data_flag, "start_", .check_time(row["start_time"], mandatory = TRUE))
    data_flag <- paste0(data_flag, "start_", .check_lat(row["start_lat"], mandatory = TRUE))
    data_flag <- paste0(data_flag, "start_", .check_lon(row["start_lon"], mandatory = TRUE))
    data_flag <- paste0(data_flag, "end_", .check_time(row["end_time"], mandatory = TRUE))
    data_flag <- paste0(data_flag, "end_", .check_lat(row["end_lat"], mandatory = FALSE))
    data_flag <- paste0(data_flag, "end_", .check_lon(row["end_lon"], mandatory = FALSE))
    data_flag <- paste0(data_flag, "end_", .check_nrods_or_nets(row["n_rods"], mandatory = TRUE, fishtype = "nrods"))
    #check trackid create fnct NOT NECESSARY
    data_flag <- paste0(data_flag, .check_crew(row["crew"]))

    return(data_flag)
  })
  return(angling)
}

#' Performs comprehensive verification on cast_netting database
#'
#' @param cast_netting the equipment log tibble
#' @returns an cast_netting tibble with the added data flag column
#' @export check_cast_netting
check_cast_netting <- function(cast_netting){
  cast_netting$data_flag <- ""

  cast_netting$data_flag <- cast_netting$data_flag <- apply(cast_netting, 1, function(row) {
    data_flag <- ""
    data_flag <- paste0(data_flag, .check_date(row["date"]))
    data_flag <- paste0(data_flag, .check_site(row["site"]))
    data_flag <- paste0(data_flag, "start", .check_time(row["start_time"]))
    data_flag <- paste0(data_flag, "start", .check_lat(row["start_lat"]))
    data_flag <- paste0(data_flag, "start", .check_lon(row["start_lon"]))
    data_flag <- paste0(data_flag, "end", .check_time(row["end_time"]))
    data_flag <- paste0(data_flag, "end", .check_lat(row["end_lat"]))
    data_flag <- paste0(data_flag, "end", .check_lon(row["end_lon"]))
    #check trackid
    data_flag <- paste0(data_flag, "end_", .check_nrods_or_nets(row["n_rods"], mandatory = TRUE, fishtype = "nnets"))
    data_flag <- paste0(data_flag, .check_crew(row["crew"]))

    return(data_flag)
  })
  return(cast_netting)
}

#' Performs comprehensive verification on range_test database
#'
#' @param range_test the equipment log tibble
#' @returns an range_test tibble with the added data flag column
#' @export check_range_test
check_range_test <- function(range_test){
  range_test$data_flag <- ""

  range_test$data_flag <- range_test$data_flag <- apply(range_test, 1, function(row) {
    data_flag <- ""
    data_flag <- paste0(data_flag, .check_date(row["date"]))
    data_flag <- paste0(data_flag, .check_site(row["site"]))
    #check rt type
    #check distance
    #chekc object_id???
    #check wpt
    data_flag <- paste0(data_flag, .check_lat(row["lat"]))
    data_flag <- paste0(data_flag, .check_lon(row["lon"]))
    data_flag <- paste0(data_flag, "start", .check_time(row["start_time"]))
    data_flag <- paste0(data_flag, "end", .check_time(row["end_time"]))
    data_flag <- paste0(data_flag, .check_depth(row["depth_m"]))
    #check susbtrate
    #check rttag_ids?
    data_flag <- paste0(data_flag, .check_crew(row["crew"]))

    return(data_flag)
  })
  return(range_test)
}

#' Performs comprehensive verification on gps_records database
#'
#' @param gps_records the equipment log tibble
#' @returns an gps_records tibble with the added data flag column
#' @export check_gps_records
check_gps_records <- function(gps_records){
  gps_records$data_flag <- ""

  gps_records$data_flag <- gps_records$data_flag <- apply(gps_records, 1, function(row) {
    data_flag <- ""
    data_flag <- paste0(data_flag, .check_date(row["date"]))
    data_flag <- paste0(data_flag, .check_site(row["site"]))
    #check obj_name
    #check_file_name
    #check record type
    #check device
    data_flag <- paste0(data_flag, .check_crew(row["crew"]))

    return(data_flag)
  })
  return(gps_records)
}
######################### Error code summary ###################

#' Provides a summary of the possible error codes and their meaning
#'
#' @export quality_control_codes
quality_control_codes <- function () {
  message(
    "date_not_entered: Date is left blank. This entry is mandatory, please fill it.
date_invalid: Date is invalid. The date should be reported in YYYY-MM-DD format. Check that year, month and date values are correct, that they are - delimited, and that no extra characters are present
date_out_of_range: Date is out of valid range. The date should be between January 1 2023 and the computer's system date at the time of data checking

time_not_entered: time is left blank. This entry is mandatory, please fill it.
time_invalid (col#): Time is invalid in the column indicated in parentheses. Time should be reported in HH:MM:SS format in 24h time. Check that hour, minute and second values are correct, that they are : delimited, that there are no extra characters and that the time is in 24h format. Seconds must be included. You may write 00 if a value is not reported to the seconds

site_not_entered: site is left blank. This entry is mandatory, please fill it.
site_invalid: Site ID is invalid. It must be one of the values listed in object sites. Make sure the value matches and that no extra characters are present

equip_not_entered: equipment type is left blank. This entry is mandatory, please fill it.
equip_invalid: equipment type is invalid. It must be one of the values listed in object equip_types. Make sure the value matches and that no extra characters are present

serial_not_entered: serail ID is left blank. This entry is mandatory, please fill it. Each individual piece of equipment must have its own entry, and thus an associated serial ID number. The only exception is a task done on all units of a given equipment type, at which point \"all\" may be written.
serial_invalid (col#): serial ID is invalid in the column indicated in parentheses. Make sure the serial ID is present in the list reference_serial_id$serial_id

equip_serial_nomatch: serial ID value does not match the equipment type. Make sure the correct serial ID or equipment type is enterred

stnid_invalid: the station ID is invalid. This should be written as DD-SSS-XX-Z, where DD is the deployment type (RT, GA, or GR), SSS is the site id (see sites), XX is a number from 01-99, and Z is a letter from A-Z.

action_not_entered: action is left blank. This entry is mandatory, please fill it.
action_invalid: the action is invalid. It must be one of the values listed in equipment_actions.  Make sure the value matches and that no extra characters are present

deploy_not_entered: deployment type is left blank. This entry is mandatory, please fill it. If the purpose of the action does not fall within a deployment type, enter \"other\"
deploy_invalid: deployment type is invalid. It must be one of the values listed in deploy_types  Make sure the value matches and that no extra characters are present

stn_deploy_nomatch: the deployment type and the station ID do not match.

wpt_invalid: the waypoint entry contains unexpected special characters, or is not comma-separated

lat_invalid: latitude value invalid. Must be in decimal degrees with at least 5 (xx.xxxxxx) and within the range of the data for a given site
lat_out_of_range: latitude not within the range of the data for a given site

lon_invalid: longitude value invalid. Must be in decimal degrees with at least 5 decimals (-xx.xxxxxx)
lon_out_of_range: longitude not within the range of the data for a given site

site_gps_nomatch: latitude and longitude are outside of the expected range for a given field site

depth_invalid: depth invalid. Depth is reported in meters in positive values. Make sure it is reported in meters
depth_out_of_range: valid for a given site

crew_invalid: the crew is not entered correctly. Make sure it is written as 2-3 letter initials unique to each person, separated by \', \'.")
}
#
#
# #
# #
# # database <- list(equipment_log=NULL
# #                  , fish=NULL)
# # for (i in names.tables) {
# #   for (j in 1:length(database_list)) {
# #     temp_data <- database_list[[j]][i]
# #
# #     record_list[[i]] <- rbind(record_list[[i]], data.frame(temp_data))
# #   }
# # }
# #
# # result_df <- do.call(rbind, lapply(names(database_list), function(name) {
# #   data <- database_list[[name]]$data
# #   data$df_name <- name
# #   return(data)
# # }))
#
#
#
#
#
#
# for (current_file in file_names){
#   #importing database
#   database <- import_database_xl(paste0(folder_path, current_file))
#
#   #cheking database
#   database_flagged <- list()
#
#   database_flagged$equipment_log <- check_equipment_log(database$equipment_log)
#
#   database_flagged$fish <- check_fish(database$fish)
#
#   #the same for all the other sheets "fykes"         "angling"       "cast_netting"  "range_test"    "gps_records"
#
#   #exporting flagged database to excel
#   openxlsx::write.xlsx(x = database_flagged, file = paste0(flagged_folder_path, "/", substring(current_file, 1, I(nchar(current_file)-5)), "_flagged.xlsx"))
# }
#
# #if no comments flagged, immediately append to main database
# #if( original data_flag column in each sheets is all NA )
# #append immediately to final database and announce that this was done
#
