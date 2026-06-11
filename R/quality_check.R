######################### Data checking flow functions ###################
#' Performs comprehensive verification of all excel data entry files within a given folder
#'
#' this function imports excel documents stored in the "raw" folder that need quality control. It performs a comprehensive quality control, and saves csv files output to the "flagged" folder for the QC person to go through and correct the data flags.
#'
#' @param folder_path the path of the folder in which the data entry xlsx sheets are stored. Folder must only contain the correct format of data entry sheets
#' @param return_summary whether to output a summary of data checking to the console
#' @param recheck TRUE/FALSE whether the data is to be rechecked (TRUE) or checked for the first time (FALSE)
#' @param visualize whether you want to return visual items for further data checking
#' @param testing_mode When in testing mode, all checks to ensure that data entry is only occurring on the lab computer are removed.
#' @returns if visualize = TRUE, this function returns a list of visuals for the data checker to go through
#' @export check_dataentry
check_dataentry <- function(
    folder_path,
    return_summary = TRUE,
    recheck = FALSE,
    visualize = TRUE,
    testing_mode = FALSE
    ){

  #checking that data checking is done in the right place
  check_folderlocation(testing_mode)

  if(substring(folder_path, nchar(folder_path), nchar(folder_path))!="/"){
    stop("Please include the final / of the folder path")
  }

  flagged_folder_path <- paste0(folder_path, "flagged")
  raw_folder_path <- paste0(folder_path,  "raw")
  clean_folder_path <- paste0(folder_path,  "clean")

  #getting all file paths within the folder
  if (recheck){
    #creating database frame - importing existing database
    orig_db_filenames <- list.files(path = clean_folder_path, pattern = "parquet$", full.names = TRUE)
    orig_db_filenames_short <- list.files(path = clean_folder_path, pattern = "parquet$", full.names = FALSE)
    orig_db_names <- gsub(pattern = ".parquet", replacement = "", x = orig_db_filenames_short)

    orig_database <- purrr::map(.x = orig_db_filenames, .f =~ arrow::read_parquet(file = .x))
    names(orig_database) <- orig_db_names

    #making empty database frame
    database_frame <- purrr::map(.x = orig_database, .f = function(.x) {.x[0,]})

    # reading in flagged sheets
    file_names <- list.files(path = flagged_folder_path, pattern = ".csv$", full.names = TRUE)
    file_names_short <-basename(file_names)

    sheet_names <- gsub(".csv", "", gsub(substring(file_names_short, 1, 16)[[1]], "", file_names_short))

    database_flag_sheets <- list()
    database_flag_sheets <- purrr::map(.x = file_names, .f = ~tibble::as_tibble(read.csv(.x, colClasses = "character")))
    names(database_flag_sheets) <- sheet_names

    # putting the new flagged data into the empty database frame
    database <- database_frame
    for(x in names(database_flag_sheets)){
      database[[x]] <- rbind(database[[x]], database_flag_sheets[[x]])
    }

  } else{
    file_names <- list.files(raw_folder_path, pattern=".xlsx$", full.names = TRUE)
    filenames_short <- basename(file_names)

    #IMPORT AND MERGE ALL DATA ENTRY FILES
    database_list <- lapply(file_names, FUN = function(x) import_database_xl(x))
    names(database_list) <- filenames_short

    #check that data entry information is present
    missing_entry_data <- rep(FALSE, times = length(database_list))
    entry_date_invalid <- rep(FALSE, times = length(database_list))
    entry_initials_invalid <- rep(FALSE, times = length(database_list))

    for (i in 1:length(database_list)){
      if(is.null(database_list[[i]]$entry_metadata$date[1]) |
         is.null(database_list[[i]]$entry_metadata$enterer_initials[1])){
        missing_entry_data[i] <- TRUE
      }else if(.check_date(as.character(database_list[[i]]$entry_metadata$date[1])) != ""){
        entry_date_invalid[i] <- TRUE
      } else if(.check_single_initials(database_list[[i]]$entry_metadata$enterer_initials[1]) != ""){
        entry_initials_invalid[i] <- TRUE
      }
    }

    if(any(missing_entry_data) ){
      stop(
        paste0(
          "The following files have missing data entry data: ",
          paste0(file_names[missing_entry_data], collapse = ", "),
          ". both the date and initials of the data enterer are necessary. Please add this data in the entry_metadata sheet"
          )
        )
    }
    if(any(entry_date_invalid)){
      stop(
        paste0(
          "The following files have an invalid date for data entry metadata: ",
          paste0(file_names[entry_date_invalid], collapse = ", "),
          ". Please follow the YYYY-MM-DD format for the date in the entry_metadata sheet"
          )
        )
    }
    if(any(entry_initials_invalid)){
      stop(
        paste0(
          "The following files have invalid initials for data entry metadata: ",
          paste0(file_names[entry_initials_invalid], collapse = ", "),
                  ". Please write the data enterer's initials as 2 or 3 letters in the entry_metadata sheet"
          )
        )
    }

    database_list <- purrr::map2(file_names, database_list, function(name, df){
      df[[1]]$file_name <- name
      df[[2]]$file_name <- name
      df[[3]]$file_name <- name
      df[[4]]$file_name <- name
      df[[5]]$file_name <- name
      df[[6]]$file_name <- name
      df[[7]]$file_name <- name
      df[[8]]$file_name <- name

      return(df)
    })
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
  }
  #removing separate entry_matadata dataframe
  #database <- database[setdiff(names(database), "entry_metadata")]

  #checking the data
  database_flagged <- database
  database_flagged$equipment_log <- check_equipment_log(database$equipment_log)
  database_flagged$fish <- check_fish(database$fish)
  database_flagged$fykes <- check_fyke(database$fykes)
  database_flagged$angling <- check_angling(database$angling)
  database_flagged$cast_netting <- check_cast_netting(database$cast_netting)
  database_flagged$range_test <- check_range_test(database$range_test)
  database_flagged$gps_records <- check_gps_records(database$gps_records)

  if(!recheck){
    #removing metadata and valid_levels sheet.
    database_flagged$entry_metadata <- NULL
    database_flagged$valid_levels <- NULL
  }

  # moving the raw files to an archive folder
  for(file in file_names){
    file.rename(from = file, to = file.path(raw_folder_path, "archive", basename(file)))
  }

  # filtering database
  database_dat_only <- Filter(function(x) nrow(x) > 0, database_flagged )
  names_dat_only <- names(database_dat_only) #getting names of files with data

  # export database_flagged
  if(recheck) {
    purrr::map2(
      .x = database_dat_only,
      .y = file_names,
      .f = ~write.csv(.x, file.path(.y), row.names = FALSE)
      )
  }else {
    purrr::map2(
      .x = database_dat_only,
      .y = names(database_dat_only),
      .f = ~write.csv(
        .x,
        file.path(
          flagged_folder_path,
          paste0(format(Sys.time(), "%Y-%m-%d-%H%M"),"_", .y, ".csv")
        ),
        row.names = FALSE
        )
      )
    #openxlsx::write.xlsx(x = database_flagged, file = paste0(flagged_folder_path, Sys.Date(), "_database_flagged.xlsx"))
  }

  #visualize data
  if(visualize){
    visuals <- visualize_data_check(database = database_flagged)
  }
  output_message <- paste0("Data entry successfully complete. Flagged data have been output in the directory ", getwd(), flagged_folder_path, ". Make all data entry corrections in these flagged files\n\n")

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
    data_enterers <- unique(database_flagged$equipment_log$entry_initials)
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

  output_message <- paste0("\nDATA CHECKING COMPLETE. For further data checking, please use the visualize_data_check function\n\n", output_message)

  print(cat(output_message))

  if (visualize){
    return(visuals)
  }
}

######################### Importing and merging GPS waypoints ###################
#' Merges GPS waypoints from a GPX file with waypoints input in the Equipment Log
#'
#' this function imports a GPX file stored in the "raw" folder and imports latitude and longitude from matching waypoints into the flagged version of the equipment_log, while adding flags to the flagged equipment_log.csv for potential issues with GPS points.
#' This function should be run after check_dataentry once all non-GPS flags that can be addressed have been addressed.
#'
#' @param folder_path the path of the raw folder in which the data entry xlsx sheets and .GPX file for merging are stored.
#' @param remerge TRUE/FALSE whether the waypoint data is to be re-merged (TRUE) after addressing waypoint flags or merged for the first time (FALSE)
#' @export merge_waypoints
merge_waypoints <- function(folder_path, remerge = F){

  raw_folder_path <- paste0(folder_path, "raw")
  flagged_folder_path <- paste0(folder_path, "flagged")

  gps_file_name <- list.files(path = raw_folder_path, pattern = ".gpx$", full.names = TRUE)
  if (length(gps_file_name) > 1){
    stop("Multiple GPX files in raw folder. Merge only one GPX file at a time.")
  }
  if (length(gps_file_name) == 0){
    stop("No GPX file found. Copy GPX file with waypoints into raw folder.")
  }

  #TODO: replace this with a step where waypoints are entered into the database and checked,
  #then this data is loaded to work with GPS data.

  # importing GPS points
  gps_waypoints <- load_gps_file(gps_file_name, type = "waypoints")


  gps_waypoints <- sf::st_read(
    gps_file_name,
    layer = "waypoints"
    ) |>
    dplyr::mutate(
      lat = sf::st_coordinates(geometry)[,2],
      lon = sf::st_coordinates(geometry)[,1],
      wpt = name
    ) |>
    dplyr::select(wpt, lat, lon) |>
    sf::st_drop_geometry()


  # reading in flagged equipment log
  equip_log_files <- list.files(
    path = flagged_folder_path,
    pattern = "equipment_log.csv$",
    full.names = TRUE
    )

  if (length(equip_log_files) == 0){
    stop("No equipment_log.csv file in flagged folder. To merge GPS points with newly-input equipment log, first run check_dataentry() on a raw .xlsx data template file with equipment log data.")
  }

  missing_wpts_list <- list()
  far_wpts_list <- list()
  for(equip_file in equip_log_files){

    equip_log <- read.csv(equip_file, colClasses = "character")  |>
      dplyr::mutate(
        row = 1:dplyr::n(),
        wpt = ifelse(wpt == "", NA, wpt)
        )

    if (remerge) {
      equip_log <- equip_log |>
        dplyr::mutate(
          data_flag = stringr::str_remove_all(data_flag, "max_waypoint_dist_\\S+/"),
          data_flag = stringr::str_remove_all(data_flag, "waypoint_not_found_in_GPX/"),
          lat = ifelse(stringr::str_detect(comment, "lat/lon_from_"), NA, lat),
          lon = ifelse(stringr::str_detect(comment, "lat/lon_from_"), NA, lon),
          comment = stringr::str_remove_all(comment, ",?\\s?lat/lon_from_\\S+"),
          comment = stringr::str_remove_all(comment, ",?\\s?lat/lon_from_\\S+"),
          comment = stringr::str_remove_all(comment, ",?\\lat/lon_entered_manually")
          )
    }

    equip_log_wpts <- equip_log |>
      dplyr::mutate(
        wpt_original = wpt,
        wpt = stringr::str_remove_all(wpt, pattern = " "),
        wpt = stringr::str_split(wpt, pattern = ",")
        )|>
      tidyr::unnest(wpt) |>
      dplyr::left_join(gps_waypoints, by = "wpt")
    # checking that the way points that will be averaged are very close to each other
    # avoids averaging way points that were mis-written (ex 627 and 268) and were not actually taken
    # at the same time

    equip_log_mult_wpts <- equip_log_wpts |>
      dplyr::filter(!is.na(wpt) & lon.y != "") |>
      sf::st_as_sf(coords = c("lon.y","lat.y"), crs= 4326, remove = FALSE) |>
      dplyr::filter(dplyr::n_distinct(wpt) > 1) |>
      dplyr::summarise(
        max_dist_m = suppressWarnings(max(sf::st_distance(geometry))),
        .by = row
        ) |>
      units::drop_units() |>
      sf::st_drop_geometry()

    equip_log_wpts_mean <- equip_log_wpts |>
      dplyr::left_join(
        equip_log_mult_wpts,
        by = "row"
        ) |>
      dplyr::summarize(
        lat = mean(lat.y),
        lon = mean(lon.y),
        max_dist_m = round(mean(max_dist_m),1),
        n_wpts = dplyr::n(),
        n_wpts_missing = sum(is.na(lat.y)),
        .by = row
        )

    equip_log_processed <- equip_log |>
      dplyr::left_join(
        equip_log_wpts_mean,
        by = "row") |>
      # putting in the lat/lon from the waypoint only when none entered
      dplyr::mutate(
        n_wpts = ifelse(is.na(n_wpts),0,n_wpts),
        n_wpts_missing = ifelse(is.na(n_wpts_missing)|is.na(wpt), 0, n_wpts_missing),
        lat.x = as.double(lat.x),
        lon.x = as.double(lon.x),
        lat = ifelse(is.na(lat.x) == T, lat.y, lat.x),
        lon = ifelse(is.na(lon.x) == T, lon.y, lon.x)
        )|>
      dplyr::mutate(
        wpt_comment = dplyr::case_when(
          !is.na(lat.x)               ~ "lat/lon_entered_manually",
          !is.na(lat.y) & n_wpts > 1  ~ paste0("lat/lon_from_mean_of", n_wpts,"_wpts"),
          !is.na(lat.y) & n_wpts == 1 ~ "lat/lon_from_waypoint",
          n_wpts > 0                  ~ paste0("lat/long_not_available"),
          TRUE                        ~ ""),
        comment = ifelse(is.na(comment), "", comment),
        comment = dplyr::case_when(
          comment == "" ~ wpt_comment,
          wpt_comment == "" ~ comment,
          TRUE  ~ paste0(comment, ", ", wpt_comment)
        ),
        wpt_flag = dplyr::case_when(
          n_wpts == 0        ~ "",
          n_wpts_missing > 0 ~ "waypoint_not_found_in_GPX/",
          max_dist_m >= 4     ~  paste0("max_waypoint_dist_", round(max_dist_m,2), "m_apart/"),
          TRUE               ~ ""),
        data_flag = paste0(data_flag, wpt_flag)
      )|>
      dplyr::relocate(lat, lon, .after = wpt) |>
      dplyr::select(-c(lat.x, lon.x, lat.y, lon.y, wpt_comment, wpt_flag))

    far_wpts_list[[equip_file]] <- equip_log_processed |>
      dplyr::filter(max_dist_m > 0 & n_wpts >0) |>
      dplyr::select(row, wpt, max_dist_m) |>
      dplyr::mutate(file = equip_file,.before = row)

    missing_wpts_list[[equip_file]] <- equip_log_processed |>
      dplyr::filter(!is.na(wpt) & is.na(lat)) |>
      dplyr::select(row, wpt) |>
      dplyr::mutate(file = equip_file,.before = row)

    equip_log_final <- equip_log_processed |>
      dplyr::select(-c(max_dist_m, n_wpts, row, n_wpts_missing))

    write.csv(equip_log_final, file = equip_file, row.names = F)


  }


  output_message <- paste0(
    "\n\nWaypoints merged successfully for all equipment files.\n",
    "Flagged data with merged GPS points have been output in the directory ",
    getwd(), "/",
    flagged_folder_path,
    ".\n Make all data entry corrections in these flagged files. \n\n"
    )

  print(cat(output_message))

  far_wpts <- dplyr::bind_rows(far_wpts_list)
  missing_wpts <- dplyr::bind_rows(missing_wpts_list)

  if(nrow(far_wpts) > 0) {
    cat("\nWARNING: The following entries have a maximum distance of >4m between waypoints.\nSee flagged .csv and check waypoints are correct.\n\n")
    print(far_wpts)
  }

  if(nrow(missing_wpts) > 0) {
    paste0("\nWARNING: The following entries include waypoints missing from the GPX file.\n See flagged .csv.\n\n") |>
      cat()
    print(missing_wpts)
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
  database <- lapply(sheet_names[!sheet_names== "README! - Instructions"], function(x) readxl::read_excel(path = path, sheet = x, col_types = "text"))
  names(database) <- sheet_names[!sheet_names== "README! - Instructions"]

  return(database)
}

#' Appends the checked data entry files to the relevant csv databases
#'
#' @param folder_path the new data entry database that has been checked and is ready to add to the main database
#' @returns a report of the check status for the entry
#' @export append_to_database
append_to_database <- function(folder_path, testing_mode = FALSE){
  #checking that data entry is done in the right place
  check_folderlocation(testing_mode)

  if(substring(folder_path, nchar(folder_path), nchar(folder_path))!="/"){
    stop("Please include the final / of the folder path")
  }

  flagged_folder_path <- paste0(folder_path, "flagged")
  clean_folder_path <- paste0(folder_path,  "clean")
  raw_folder_path <- paste0(folder_path,  "raw")

  # import original database
  orig_db_filenames <- list.files(path = clean_folder_path, pattern = "parquet$", full.names = TRUE)
  orig_names <- orig_db_filenames |>
    gsub(pattern = ".parquet", replacement = "") |>
    gsub(pattern = paste0(clean_folder_path, "/"), replacement = "")

  orig_database <- purrr::map(.x = orig_db_filenames, .f =~ arrow::read_parquet(file = .x))
  names(orig_database) <- orig_names

  # create database frame
  database_frame <- purrr::map(.x = orig_database, .f = function(.x) {.x[0,]})

  #import QC'd csv files
  file_names <- list.files(path = flagged_folder_path, pattern = ".csv$", full.names = TRUE)
  file_names_short <- basename(file_names)

  sheet_names <- gsub(".csv", "", gsub(substring(file_names_short, 1, 16), "", file_names_short))

  database_flag_sheets <- list()
  database_flag_sheets <- purrr::map(.x = file_names, .f = ~tibble::as_tibble(read.csv(.x, colClasses = "character")))
  names(database_flag_sheets) <- sheet_names

  # putting the new flagged data into the empty database frame
  database <- database_frame
  for(x in names(database_flag_sheets)){
    database[[x]] <- rbind(database[[x]], database_flag_sheets[[x]])
  }

  #final check
  database_flagged <- database
  database_flagged$equipment_log <- check_equipment_log(database$equipment_log)
  database_flagged$fish <- check_fish(database$fish)
  database_flagged$fykes <- check_fyke(database$fykes)
  database_flagged$angling <- check_angling(database$angling)
  database_flagged$cast_netting <- check_cast_netting(database$cast_netting)
  database_flagged$range_test <- check_range_test(database$range_test)
  database_flagged$gps_records <- check_gps_records(database$gps_records)

  database_flagged <- purrr::map(database_flagged, function(.x){
    .x$date_added <- Sys.Date()
    .x
  })

  eq_log_dup <- which(base::duplicated(database_flagged$equipment_log[,c("date", "serial_id", "action", "time")]))
  fish_dup <- which(base::duplicated(database_flagged$fish[,c("date", "site", "capture_method", "capture_time", "species", "length_mm", "dna_id", "tag_serial", "recap" )]))
  fish_dup <- fish_dup[database_flagged$fish[fish_dup,]$species != "bycatch" & database_flagged$fish[fish_dup,]$species != "other"] #filtering out bycatch
  fykes_dup <- which(base::duplicated(database_flagged$fykes[,c("date", "site", "fyke_id", "out_time", "in_time")]))
  angling_dup <- which(base::duplicated(database_flagged$angling[,c("date", "site", "start_time", "start_lat", "start_lon", "end_time")]))
  cast_dup <- which(base::duplicated(database_flagged$cast_netting[,c("date", "site", "start_time", "start_lat", "start_lon", "end_time")]))
  rt_dup <- which(base::duplicated(database_flagged$range_test[,c("start_date", "site", "rt_type", "object_id", "start_time")]))
  gps_dup <- which(base::duplicated(database_flagged$gps_records[,c("obj_name")]))

  if(length( c(eq_log_dup, fish_dup, fykes_dup, angling_dup, cast_dup, rt_dup, gps_dup))>0){
    dup_report <- paste0("duplicates were detected in the flagged dataset to append to the final database. Please either remove or fix the duplicates.\n
    The following rows are duplicates: \n",
                         "\nequipment_log row id: ", paste0(eq_log_dup, collapse = ", "),
                         "\n\nfish row id: ", paste0(fish_dup, collapse = ", "),
                         "\n\nfykes row id: ", paste0(fykes_dup, collapse = ", "),
                         "\n\nangling row id: ", paste0(angling_dup, collapse = ", "),
                         "\n\ncast row id: ", paste0(cast_dup, collapse = ", "),
                         "\n\nrange_test row id: ", paste0(rt_dup, collapse = ", "),
                         "\n\ngps row id: ", paste0(gps_dup, collapse = ", "), "\n")

    stop(cat(dup_report))
  }

  #merging the two
  appended_database <- purrr::map2(.x = orig_database, .y = database_flagged, .f = function(.x, .y){
    appended <- rbind(.x, .y)
    appended
  })

  #checking for dups in appended db
  eq_log_dup <- which(base::duplicated(appended_database$equipment_log[,c("date", "serial_id", "action", "time")]))
  fish_dup <- which(base::duplicated(appended_database$fish[,c("date", "site", "capture_method", "capture_time", "species", "length_mm", "dna_id", "tag_serial", "recap" )]))
  fish_dup <- fish_dup[appended_database$fish[fish_dup,]$species != "bycatch" & appended_database$fish[fish_dup,]$species != "other"] #filtering out bycatch
  fykes_dup <- which(base::duplicated(appended_database$fykes[,c("date", "site", "fyke_id", "out_time", "in_time")]))
  angling_dup <- which(base::duplicated(appended_database$angling[,c("date", "site", "start_time", "start_lat", "start_lon", "end_time")]))
  cast_dup <- which(base::duplicated(appended_database$cast_netting[,c("date", "site", "start_time", "start_lat", "start_lon", "end_time")]))
  rt_dup <- which(base::duplicated(appended_database$range_test[,c("start_date", "site", "rt_type", "object_id", "start_time")]))
  gps_dup <- which(base::duplicated(appended_database$gps_records[,c("obj_name")]))

  if(length( c(eq_log_dup, fish_dup, fykes_dup, angling_dup, cast_dup, rt_dup, gps_dup))>0){
    dup_report <- paste0("duplicates were detected between the newly appended flagged dataset and the final database. Please either remove or fix the duplicates.\n
    The following rows in the flagged dataset are duplicates of rows in the final dataset: \n",
                         "\nequipment_log row id: ", paste0(c(eq_log_dup-nrow(orig_database$equipment_log)), collapse = ", "),
                         "\n\nfish row id: ", paste0(c(fish_dup-nrow(orig_database$fish)), collapse = ", "),
                         "\n\nfykes row id: ", paste0(c(fykes_dup-nrow(orig_database$fykes)), collapse = ", "),
                         "\n\nangling row id: ", paste0(c(angling_dup-nrow(orig_database$angling)), collapse = ", "),
                         "\n\ncast row id: ", paste0(c(cast_dup-nrow(orig_database$cast_netting)), collapse = ", "),
                         "\n\nrange_test row id: ", paste0(c(rt_dup-nrow(orig_database$range_test)), collapse = ", "),
                         "\n\ngps row id: ", paste0(c(gps_dup-nrow(orig_database$gps_records)), collapse = ", "), "\n\n")

    stop(cat(dup_report))
  }

  #moving curring db to archive folder
  for(file in orig_db_filenames){
    file.rename(from = file, to = file.path(paste0(clean_folder_path, "/archive"),
                                            paste0(Sys.Date(), "_",basename(file))))
  }

  #moving flagged files to archive folder
  for(file in file_names){
    file.rename(from = file, to = file.path(paste0(flagged_folder_path, "/archive"),basename(file)))
  }

  # moving the gpx file to archive (if it exists)
  gps_file_name <- list.files(path = raw_folder_path, pattern = ".gpx$", full.names = TRUE)
  for(file in gps_file_name){
    file.rename(from = file, to = file.path(paste0(raw_folder_path, "/archive"),
                                          paste0(Sys.Date(), "_",basename(file))))
  }

  #exporting updated version
  purrr::map2(.x = appended_database, .y = orig_db_filenames, .f = function(.x, .y){
    arrow::write_parquet(x = .x, sink = file.path(.y))})
}

#' Appends the checked data entry files to the relevant csv databases
#'
#' @param flagged_folder_path the filepath to the flagged folder where files to be examined are kept
#' @param database the database object to be checked. Alternate to folder_path, to be used only within check_dataentry() function
#' @returns visual inspection aids to help identify error that are not captured by quality control
#' @export visualize_data_check
visualize_data_check <- function(flagged_folder_path = NULL, database = NULL){

  if(is.null(flagged_folder_path) & is.null(database)){
    stop("Visualize data check has no input")
  }
  if(!is.null(flagged_folder_path) & !is.null(database)){
    stop("Visualize data check has two inputs. Please choose either flagged_folder_path or database but not both")
  }

  if(!is.null(flagged_folder_path)){
    #import data
    file_names <- list.files(path = flagged_folder_path, pattern = ".csv$", full.names = TRUE)
    file_names_short <- basename(file_names)
    sheet_names <- gsub(".csv", "", substring(file_names_short, 17, 100))

    database <- list()
    database <- purrr::map(.x = file_names, .f = ~tibble::as_tibble(read.csv(.x, colClasses = "character")))
    names(database) <- sheet_names
  }

  #check dates
  database$range_test$date <- database$range_test$start_date

  dates <- dplyr::bind_rows(lapply(seq_along(database), function(i) {

    tibble::tibble(
      date =  database[[i]]$date,
      data_type =  names(database)[i]
    )
  }))
  dates_plot <- ggplot2::ggplot(data=dates, ggplot2::aes(x=lubridate::ymd(date), y=data_type))+
    ggplot2::geom_point(ggplot2::aes(colour=data_type))+
    ggplot2::labs(title = "All action dates", subtitle = "check that all dates are within the correct range, \nand that no NAs were present or introduced when converting to date", x="date", y="data type")

  #check initials
  initials <- tibble::tibble(initials = c(unique(database$equipment_log$entry_initials),
                                          unique(unlist(stringr::str_split(database$equipment_log$crew, ", "))),
                                          unique(unlist(stringr::str_split(database$fish$crew, ", "))),
                                          unique(unlist(stringr::str_split(database$fykes$crew, ", "))),
                                          unique(unlist(stringr::str_split(database$angling$crew, ", "))),
                                          unique(unlist(stringr::str_split(database$range_test$crew, ", "))),
                                          unique(unlist(stringr::str_split(database$gps_records$crew, ", "))),
                                          unique(database$fish$surgeon),
                                          unique(database$fish$assistant),
                                          unique(database$fish$recorder)),
                             initial_type = c(rep("data_entry", times = length(unique(database$equipment_log$entry_initials))),
                                              rep("equipment_log_crew", times = length(unique(unlist(stringr::str_split(database$equipment_log$crew, ", "))))),
                                              rep("fish_crew", times = length(unique(unlist(stringr::str_split(database$fish$crew, ", "))))),
                                              rep("fykes_crew", times = length(unique(unlist(stringr::str_split(database$fykes$crew, ", "))))),
                                              rep("angling_crew", times = length(unique(unlist(stringr::str_split(database$angling$crew, ", "))))),
                                              rep("range_test_crew", times = length(unique(unlist(stringr::str_split(database$range_test$crew, ", "))))),
                                              rep("gps_records_crew", times = length(unique(unlist(stringr::str_split(database$gps_records$crew, ", "))))),
                                              rep("surgeon" ,  times = length(unique(database$fish$surgeon))),
                                              rep("surgery_assistant", times = length(unique(database$fish$assistant))),
                                              rep("surgery_recorder", times = length(unique(database$fish$recorder)))))

  initials_table <- gt::gt(data = initials ,caption = "Ensure that all initials are indivitual initials of 2-3 capital letters only. If this is not the case, please fix it in the data",
                           row_group_as_column = 1,
                           groupname_col =  "initial_type")

  #check maps

  spatial_data <- dplyr::bind_rows(tibble::tibble(site = database$equipment_log$site,
                                                  lat = database$equipment_log$lat,
                                                  lon = database$equipment_log$lon,
                                                  point_type = "equipment"),
                                   tibble::tibble(site=database$fish$site,
                                                     lat=database$fish$capture_lat,
                                                     lon=database$fish$capture_lon,
                                                     point_type="fish_capture"),
                                   tibble::tibble(site=database$fish$site,
                                                  lat=database$fish$release_lat,
                                                  lon=database$fish$release_lon,
                                                  point_type="fish_release"),
                                   tibble::tibble(site=database$angling$site,
                                                  lat=database$angling$start_lat,
                                                  lon=database$angling$start_lon,
                                                  point_type="angling_start"),
                                   tibble::tibble(site=database$angling$site,
                                                  lat=database$angling$end_lat,
                                                  lon=database$angling$end_lon,
                                                  point_type="angling_end"),
                                   tibble::tibble(site=database$cast_netting$site,
                                                  lat=database$cast_netting$start_lat,
                                                  lon=database$cast_netting$start_lon,
                                                  point_type="cast_netting_start"),
                                   tibble::tibble(site=database$cast_netting$site,
                                                  lat=database$cast_netting$end_lat,
                                                  lon=database$cast_netting$end_lon,
                                                  point_type="cast_netting_end"),
                                   tibble::tibble(site=database$cast_netting$site,
                                                  lat=database$cast_netting$end_lat,
                                                  lon=database$cast_netting$end_lon,
                                                  point_type="cast_netting_end"),
                                   tibble::tibble(site=database$fykes$site,
                                                  lat=database$fykes$lat,
                                                  lon=database$fykes$lon,
                                                  point_type="fyke"),
                                   tibble::tibble(site=database$range_test$site,
                                                  lat=database$range_test$lat,
                                                  lon=database$range_test$lon,
                                                  point_type="range_test"))
  spatial_data <- dplyr::filter(spatial_data, !(is.na(lat)|is.na(lon)))
  spatial_data <- sf::st_as_sf(x = spatial_data, coords=c("lon", "lat"))
  sf::st_crs(spatial_data) <- 4326

  map_spatial <- mapview::mapview(spatial_data, zcol="point_type", layer.name="All spatial by data type")+
    mapview::mapview(spatial_data, zcol="site", layer.name = "All spatial by site")

  #print to viewer
  visual_outputs <- list(dates_plot =dates_plot,
                         initials_table = initials_table,
                         map_spatial = map_spatial)
  print(dates_plot)
  print(initials_table)
  print(map_spatial)

  return(visual_outputs)
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

  fish$data_flag <- apply(fish, 1, function(row) {

    if(is.na(row["capture_outcome"])){
      stop(paste(
      "cannot proceed with data checking; at least one fish is missing 'capture_outcome' values"
      ))
    }

    #checks to see if the fish was tagged (
    tagging <- row["capture_outcome"] == "tagged"

    data_flag <- ""
    data_flag <- paste0(data_flag, .check_date(row["date"]))
    data_flag <- paste0(data_flag, .check_site(row["site"]))
    data_flag <- paste0(data_flag, .check_capture_method(row["capture_method"]))
    data_flag <- paste0(data_flag, .check_fykeid(row["fyke_id"]))
    data_flag <- paste0(data_flag, .check_time(row["capture_time"], mandatory = FALSE))
    data_flag <- paste0(data_flag, .check_wpt(row["capture_wpt"], wpt_type = "capture", mandatory = FALSE))
    data_flag <- paste0(data_flag, .check_lat(row["capture_lat"], mandatory = FALSE))
    data_flag <- paste0(data_flag, .check_lon(row["capture_lon"], mandatory = FALSE))
    data_flag <- paste0(data_flag, .check_species(row["species"]))
    data_flag <- paste0(data_flag, .check_capture_outcome(row["capture_outcome"], mandatory = TRUE))
    data_flag <- paste0(data_flag, .check_temp(row["temp_c"]))
    data_flag <- paste0(data_flag, .check_condition(row["capture_cond"]))
    data_flag <- paste0(data_flag, .check_length(row["length_mm"]))
    data_flag <- paste0(data_flag, .check_weight(row["weight_g"]))
    data_flag <- paste0(data_flag, .check_dna_scale_id(row["dna_id"], row["scale_id"])) #populate fnct
    data_flag <- paste0(data_flag, .check_sex(row["sex"]))
    data_flag <- paste0(data_flag, .check_serial(row["tag_serial"], equip_type = "tag")) #bug here
    data_flag <- paste0(data_flag, .check_tag_model(row["tag_model"])) #create fnct
    data_flag <- paste0(data_flag, .check_clove_conc(row["clove_conc"], mandatory = tagging))
    data_flag <- paste0(data_flag, .check_time(row["anesth_start"], mandatory = tagging))
    data_flag <- paste0(data_flag, .check_time(row["surg_start"], mandatory = tagging))
    data_flag <- paste0(data_flag, .check_time(row["surg_end"], mandatory = tagging))
    data_flag <- paste0(data_flag, .check_time(row["recov_end"], mandatory = tagging))
    data_flag <- paste0(data_flag, .check_time(row["release_time"], mandatory = tagging))
    data_flag <- paste0(data_flag, .check_wpt(row["release_wpt"], wpt_type = "release", mandatory = FALSE))
    data_flag <- paste0(data_flag, .check_lat(row["release_lat"], mandatory = tagging))
    data_flag <- paste0(data_flag, .check_lon(row["release_lon"], mandatory = tagging))
    data_flag <- paste0(data_flag, .check_condition(row["release_cond"], mandatory = tagging))
    data_flag <- paste0(data_flag, .check_mort(row["mort"]))
    data_flag <- paste0(data_flag, .check_recap(row["recap"], mandatory = tagging))
    data_flag <- paste0(data_flag, .check_single_initials(row["surgeon"], mandatory = tagging))
    data_flag <- paste0(data_flag, .check_single_initials(row["assistant"], mandatory = tagging))
    data_flag <- paste0(data_flag, .check_single_initials(row["recorder"], mandatory = tagging))

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

  fyke$data_flag <- fyke$data_flag <- apply(fyke, 1, function(row) {
    if(row["net_action"] %in% c("set", "retrieved")){
      need_gps <- TRUE
    } else {
      need_gps <- FALSE
    }

    data_flag <- ""
    data_flag <- paste0(data_flag, .check_date(row["date"]))
    data_flag <- paste0(data_flag, .check_site(row["site"]))
    data_flag <- paste0(data_flag, .check_fykeid(row["fyke_id"], mandatory = TRUE))
    data_flag <- paste0(data_flag, .check_net_action(row["net_action"]))
    data_flag <- paste0(data_flag, .check_lat(row["lat"], mandatory = need_gps))
    data_flag <- paste0(data_flag, .check_lon(row["lon"], mandatory = need_gps))
    data_flag <- paste0(data_flag, .check_time(row["out_time"], mandatory = row["net_action"] %in% c("checked", "retrieved")))
    data_flag <- paste0(data_flag, .check_time(row["in_time"], mandatory = row["net_action"] %in% c("checked", "set")))
    data_flag <- paste0(data_flag, .check_fish_caught(row["fish_caught"], mandatory = row["net_action"] %in% c("checked", "retrieved")))
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
    data_flag <- paste0(data_flag, .check_time(row["start_time"], mandatory = TRUE))
    data_flag <- paste0(data_flag, .check_lat(row["start_lat"], mandatory = TRUE))
    data_flag <- paste0(data_flag, .check_lon(row["start_lon"], mandatory = TRUE))
    data_flag <- paste0(data_flag, .check_time(row["end_time"], mandatory = TRUE))
    data_flag <- paste0(data_flag, .check_lat(row["end_lat"], mandatory = FALSE))
    data_flag <- paste0(data_flag, .check_lon(row["end_lon"], mandatory = FALSE))
    data_flag <- paste0(data_flag, .check_nrods_or_nets(row["n_rods"], mandatory = TRUE, fishtype = "nrods"))
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
    data_flag <- paste0(data_flag, .check_time(row["start_time"]))
    data_flag <- paste0(data_flag, .check_lat(row["start_lat"]))
    data_flag <- paste0(data_flag, .check_lon(row["start_lon"]))
    data_flag <- paste0(data_flag, .check_time(row["end_time"]))
    data_flag <- paste0(data_flag, .check_lat(row["end_lat"]))
    data_flag <- paste0(data_flag, .check_lon(row["end_lon"]))
    #check trackid
    data_flag <- paste0(data_flag, .check_nrods_or_nets(row["n_rods"], mandatory = TRUE, fishtype = "nnets"))
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
    data_flag <- paste0(data_flag, .check_site(row["site"]))
    data_flag <- paste0(data_flag, .check_rt(row["rt_type"]))
    data_flag <- paste0(data_flag, .check_distance(row["dist_m"]))
    #chekc object_id???
    #check wpt
    data_flag <- paste0(data_flag, .check_lat(row["lat"]))
    data_flag <- paste0(data_flag, .check_lon(row["lon"]))
    data_flag <- paste0(data_flag, .check_date(row["start_date"]))
    data_flag <- paste0(data_flag, .check_time(row["start_time"]))
    data_flag <- paste0(data_flag, .check_date(row["end_date"]))
    data_flag <- paste0(data_flag, .check_time(row["end_time"]))
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


###################. Helper Functions  #############################
#' Checks that data quality control is done in the right place
check_folderlocation <- function(testing_mode = FALSE){

    #Checking that data entry is occurring in the right spot
    #checking device
  if (
    (Sys.info()[[6]] != "PedersenLab" | Sys.info()[[4]] != "DESKTOP-H8SPKU6") &
    !testing_mode #when in testing mode, the device used does not matter
      ){
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


  database_path <- "C:/Users/PedersenLab/Documents/ltmftn_project/ltmftn_database"
  #checking file directory
  if(getwd() == database_path & testing_mode) {
   stop("Testing qc functionality should not be done in the main database folder. Switch to a testing environment before proceeding")
  } else if ((getwd() != database_path) & !testing_mode){
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
  return()
}

