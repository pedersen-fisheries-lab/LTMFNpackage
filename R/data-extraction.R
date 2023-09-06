last_event <- function(log_data, ID){
  #put some error handling code here
  out <- dplyr::filter(log_data, ID == ID)
  out <- dplyr::group_by(out, ID)
  out <- dplyr::arrange(out, date)
  out <- dplyr::filter(out, date == max(date))
  out <- dplyr::ungroup(out)
  out
}
