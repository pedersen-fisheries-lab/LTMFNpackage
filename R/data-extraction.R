#' Find the final recorded record of a given set of IDs from a equipment_log object
#'
#' @param log_data An object of the "equipment_log" class containing sequential records of equipment
#' @param ID a character vector of IDs to load
#' @param eventtype dunno
#' @param type dunno
#' @returns An equipment_log tibble containing the final recorded event for each ID, or NA if the ID did not occur in the data
#' @export find_equipment_event
find_equipment_event <- function(log_data, ID, eventtype = NULL,  type = c("first","last","all")){
  type <- match.arg(type)
  #put some error handling code here
  out <- dplyr::filter(log_data, ID == ID)
  out <- dplyr::group_by(out, ID)
  out <- dplyr::arrange(out, date)
  out <- dplyr::filter(out, date == max(date))
  out <- dplyr::ungroup(out)
  out
}

