######################### Data checking flow functions ###################
#' Performs comprehensive verification of all excel data entry files within a given folder
#'
#' @param folder_path the path of the folder in which the data entry xlsx sheets are stored. Folder must only contain the correct format of data entry sheets
#' @returns creates a folder in which the flagged xlsx files are placed for the tester to look through them and correct data entry mistakes.
#' @export check_dataentry
check_dataentry <- function(folder_path){

  #Checking that data entry is occurring in the right spot
    #checking device
  if (Sys.info()[6] != "PedersenLab" |
      Sys.info()[4] != "DESKTOP-H8SPKU6"){
    message("Data checking should be performed in the database directory of the Lab Surface Pro computer.
            This device does not seem to be the lab surface pro (NodeName: DESKTOP-H8SPKU6), or it is not signed into PedersenLab.
            If you wish to proceed, enter \"Y\" into the console. If not, enter \"N\" into the console")

    user_decision <- readline(prompt = "Would you like to proceed? (enter N or Y): ")

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
    message("Your working directory for data checking should be set to \"ltmftn_project/ltmftn_database/internal_database\" on the lab surface pro.
            You can do this with the following line of code: setwd(\"~/ltmftn_project/ltmftn_database/internal_database\".
            The current directory is []. If you wish to proceed with this working directory, enter \"Y\" into the console. If not, enter \"N\".")

    user_decision <- readline(prompt = "Would you like to proceed? (enter N or Y): ")

    if(user_decision == "N"){
      stop("Data checking stopped")
    } else if (user_decision == "Y") {
      message("Data checking is proceeding")
    } else {
      stop("User entry is not valid, Data checking halted")
    }
  }

  if(substring(folder_path, nchar(folder_path), nchar(folder_path))!="/"){
    stop("Please include the final / of the folder path")
  }

  #getting all file paths within the folder
  file_names<- list.files(folder_path, pattern=".xlsx")

  flagged_folder_path <- "flagged/"

  #IMPORT AND MERGE ALL DATA ENTRY FILES
  database_list <- lapply(file_names, FUN = function(x) import_database_xl(paste0(folder_path, x)))

  #check that data entry information is present
  missing_entry_data <- ""
  entry_date_invalid <- ""
  entry_initials_invalid <- ""
  for (i in 1:length(database_list)){
    if(is.null(database_list[[i]]$entry_metadata$date[1]) |
       is.null(database_list[[i]]$entry_metadata$enterer_initials[1])){
      missing_entry_data <- paste0(missing_entry_data, ", ", i)
    }
    if(.check_date(as.character(database_list[[i]]$entry_metadata$date[1])) != ""){
      entry_date_invalid <- paste0(entry_date_invalid, ", ", i)
    }

    if(.check_crew(database_list[[i]]$entry_metadata$date[1]) != ""){
      entry_initials_invalid <- paste0(entry_initials_invalid, ", ", i)
    }
  }
  if(missing_entry_data != ""){
    stop(paste0("The following files have missing data entry data: ", missing_entry_data,
                "both the date and initials of the data enterer are necessary. Please add this data in the entry_metadata sheet"))
  }
  if(entry_date_invalid != ""){
    stop(paste0("The following files have an invalid date for data entry metadata: ", entry_date_invalid,
                "Please follow the YYYY-MM-DD format for the date in the entry_metadata sheet"))
  }
  if(entry_initials_invalid != ""){
    stop(paste0("The following files have invalid initials for data entry metadata: ", entry_initials_invalid,
                "Please write the data enterer's initials as 2 or 3 letters in the entry_metadata sheet"))
  }

  #adding data entry metadata into each row of data
  database_list <- lapply(X = database_list, FUN = function(list_entry){
    date <- list_entry$entry_metadata$date[1]
    enterer <- list_entry$entry_metadata$enterer_initials[1]

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
  database <- database[setdiff(names(database), "entry_metadata")]

  #checking the data
  database_flagged <- database
  database_flagged$equipment_log <- check_equipment_log(database$equipment_log)
  database_flagged$fish <- check_fish(database$fish)

  openxlsx::write.xlsx(x = database_flagged, file = paste0(Sys.Date(), "database_flagged.xlsx"))
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

######################### Checking each sheet  ################################

#' Performs comprehensive verification on equipment_log
#'
#' @param equipment_log the equipment log tibble
#' @returns an equipment_log tibble with the added data flag column
#' @export check_equipment_log
check_equipment_log <- function(equipment_log){
  eq <- equipment_log
  eq$data_flag <- ""

  for (i in 1: nrow(eq)){
    print(i)
    eq$data_flag[i] <- paste0( eq$data_flag[i], .check_date(eq$date[i]))
    eq$data_flag[i] <- paste0( eq$data_flag[i], .check_site(eq$site[i]))
    eq$data_flag[i] <- paste0( eq$data_flag[i], .check_equip(eq$equip_type[i]))
    eq$data_flag[i] <- paste0( eq$data_flag[i], .check_serial(eq$serial_id[i]))
    eq$data_flag[i] <- paste0( eq$data_flag[i], .check_stnid(eq$station_id[i]))
    eq$data_flag[i] <- paste0( eq$data_flag[i], .check_action(eq$action[i]))
    eq$data_flag[i] <- paste0( eq$data_flag[i], .check_deploy(deploy = eq$deploy_type[i], site = eq$site[i]))
    eq$data_flag[i] <- paste0( eq$data_flag[i], .check_time(eq$time[i]))
    eq$data_flag[i] <- paste0( eq$data_flag[i], .check_wpt(eq$wpt[i]))
    eq$data_flag[i] <- paste0( eq$data_flag[i], .check_lat(eq$lat[i]))
    eq$data_flag[i] <- paste0( eq$data_flag[i], .check_lon(eq$lon[i]))
    eq$data_flag[i] <- paste0( eq$data_flag[i], .check_depth(eq$depth_m[i]))
    eq$data_flag[i] <- paste0( eq$data_flag[i], .check_crew(eq$crew[i]))

    #checking matching
    #NEED CHECKS ON WHETHER ENTRIES ARE FILLED AND IF SO WHETHER THEY CAN BE CHECKED (IE, NO OTHER FLAGS BROUGHT UP.)
    if(! (grepl("serial_invalid", eq$data_flag[i]) | grepl("equip_invalid", eq$data_flag[i]) | grepl("serial_not_entered", eq$data_flag[i]) | grepl("equip_not_entered", eq$data_flag[i])) ){
      if (eq$serial_id[i] != "all") {
        eq$data_flag[i] <- paste0( eq$data_flag[i], .check_equip_serial_match(equip = eq$equip_type[i], serial = eq$serial_id[i]))
      }
    }
    # if(!is.na( eq$station_id[i])){
    #   if(! (grepl("stnid_invalid", eq$data_flag[i]) | grepl("deploy_invalid", eq$data_flag[i]) )){
    #     eq$data_flag[i] <- paste0( eq$data_flag[i], .check_stnid_deploy_match(stnid = eq$station_id[i], deploy = eq$deploy_type[i]))
    #   }
    # }
  }
  return(eq)
}

#' Performs comprehensive verification on fish database
#'
#' @param fish the equipment log tibble
#' @returns an equipment_log tibble with the added data flag column
#' @export check_fish
check_fish <- function(fish){
  return(fish)
}
######################### Error code summary ###################

#' Provides a summary of the possible error codes and their meaning
#'
#' @export quality_control_codes
quality_control_codes <- function () {
  cat(
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
