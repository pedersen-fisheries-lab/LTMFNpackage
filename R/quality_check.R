######################### Data checking flow functions ###################
#' Performs comprehensive verification of all excel data entry files within a given folder
#'
#' @param folder_path the path of the folder in which the data entry xlsx sheets are stored. Folder must only contain the correct format of data entry sheets
#' @returns creates a folder in which the flagged xlsx files are placed for the tester to look through them and correct data entry mistakes.
#' @export check_dataentry
check_dataentry <- function(folder_path){

  #getting all file paths within the folder
  file_names<- list.files(folder_path, pattern=".xlsx")

  flagged_folder_path <- paste0(folder_path, "flagged")

  #setting a flagged folder within working data folder
  if (! dir.exists(flagged_folder_path)){
    dir.create(flagged_folder_path)
  }

  for (current_file in file_names){
    #importing database
    database <- import_database_xl(paste0(folder_path, current_file))

    #cheking database
    database_flagged <- list()

    database_flagged$equipment_log <- check_equipment_log(database$equipment_log)

    database_flagged$fish <- check_fish(database$fish)

    #the same for all the other sheets "fykes"         "angling"       "cast_netting"  "range_test"    "gps_records"

    #exporting flagged database to excel
    openxlsx::write.xlsx(x = database_flagged, file = paste0(flagged_folder_path, "/", substring(current_file, 1, I(nchar(current_file)-5)), "_flagged.xlsx"))
  }


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

#' Performs comprehensive verification on equipment_log
#'
#' @param equipment_log the equipment log tibble
#' @returns an equipment_log tibble with the added data flag column
#' @export check_equipment_log
check_equipment_log <- function(equipment_log){
  eq <- equipment_log

  report_file <- data.frame(row_num = c(1:nrow(eq)),
                            data_flag = "")

  for (i in 1: nrow(eq)){
    print(i)

    report_file$data_flag[i] <- paste0( report_file$data_flag[i], ",", .check_date(eq$date[i]))
    report_file$data_flag[i] <- paste0( report_file$data_flag[i], ",", .check_site(eq$site[i]))
    report_file$data_flag[i] <- paste0( report_file$data_flag[i], ",", .check_equip(eq$equip_type[i]))
    report_file$data_flag[i] <- paste0( report_file$data_flag[i], ",", .check_serial(eq$serial_id[i]))
    report_file$data_flag[i] <- paste0( report_file$data_flag[i], ",", .check_stnid(eq$station_id[i]))
    report_file$data_flag[i] <- paste0( report_file$data_flag[i], ",", .check_action(eq$action[i]))
    report_file$data_flag[i] <- paste0( report_file$data_flag[i], ",", .check_deploy(deploy = eq$deploy_type[i], site = eq$site[i]))
    report_file$data_flag[i] <- paste0( report_file$data_flag[i], ",", .check_time(eq$time[i]))
    report_file$data_flag[i] <- paste0( report_file$data_flag[i], ",", .check_wpt(eq$wpt[i]))
    report_file$data_flag[i] <- paste0( report_file$data_flag[i], ",", .check_lat(eq$lat[i]))
    report_file$data_flag[i] <- paste0( report_file$data_flag[i], ",", .check_lon(eq$lon[i]))
    report_file$data_flag[i] <- paste0( report_file$data_flag[i], ",", .check_depth(eq$depth_m[i]))
    report_file$data_flag[i] <- paste0( report_file$data_flag[i], ",", .check_crew(eq$crew[i]))

    #checking matching
    #NEED CHECKS ON WHETHER ENTRIES ARE FILLED AND IF SO WHETHER THEY CAN BE CHECKED (IE, NO OTHER FLAGS BROUGHT UP.)
    if(! (grepl("serial_invalid", eq$data_flag[i]) | grepl("equip_invalid", eq$data_flag[i]) | grepl("serial_not_entered", eq$data_flag[i]) | grepl("equip_not_entered", eq$data_flag[i])) ){
      if (eq$serial_id[i] != "all") {
        eq$data_flag[i] <- paste0( eq$data_flag[i], ",", .check_equip_serial_match(equip = eq$equip_type[i], serial = eq$serial_id[i]))
      }
    }
    # if(!is.na( eq$station_id[i])){
    #   if(! (grepl("stnid_invalid", eq$data_flag[i]) | grepl("deploy_invalid", eq$data_flag[i]) )){
    #     eq$data_flag[i] <- paste0( eq$data_flag[i], ",", .check_stnid_deploy_match(stnid = eq$station_id[i], deploy = eq$deploy_type[i]))
    #   }
    # }
  }

  #identifying which rows have which errors
  # which(grepl(report_file$data_flag))
  # report_file$row_num

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




