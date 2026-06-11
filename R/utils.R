
#' Test whether a vector is numeric or coercible to numeric
#'
#' @description `is_numeric_like()` tests whether an object is "coercible to
#'   numeric" by the particular standards of scrutiny. This means:
#'
#'   - Integer and double vectors are `TRUE`.
#'   - Logical vectors are `FALSE`, as are non-vector objects.
#'   - Other vectors (most likely strings) are `TRUE` if all their non-`NA`
#'   values can be coerced to non-`NA` numeric values, and `FALSE` otherwise.
#'   - Factors are first coerced to string, then tested.
#'   - Lists are tested like atomic vectors unless any of their elements have
#'   length greater 1, in which case they are always `FALSE`.
#'   - If all values are non-numeric, non-logical `NA`, the output is also `NA`.
#'
#'
#'   This code was adapted from the 'scrutiny' R package
#'   (https://cran.r-project.org/web/packages/scrutiny/index.html) and is used
#'   under the terms of the MIT+ license.
#'
#' @param x Object to be tested.
#'
#' @details
#'
#'   `is_numeric_like()` returns `FALSE` for logical vectors simply because
#'   these are displayed as strings, not as numbers, and the usual coercion
#'   rules would be misleading in this context. Likewise, the function treats
#'   factors like strings because that is how they are displayed: the fact that
#'   factors are stored as integers is irrelevant.
#'
#'   Why store numbers as strings or factors? Only these data types can preserve
#'   trailing zeros, and only if the data were originally entered as strings.
#'   See `vignette("wrangling")`, section *Trailing zeros*.
#'
#' @return Logical (length 1).
#'
is_numeric_like <- function(x) {
  if (is.numeric(x)) {
    return(TRUE)
  }
  if (
    is.logical(x) ||
    !rlang::is_vector(x) ||
    is.list(x) && !all(vapply(
      x, function(x) length(x) == 1L, logical(1L), USE.NAMES = FALSE
    ))
  ) {
    return(FALSE)
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(NA)
  }
  x <- suppressWarnings(as.numeric(x))
  !any(is.na(x))
}

load_gps_file <- function(file_path, type =  c("waypoints", "tracks")){
  type <- match.arg(type)
  gps_data <-   sf::st_read(
    file_path,
    layer = type
  ) |>
    dplyr::mutate(
      lat = sf::st_coordinates(geometry)[,2],
      lon = sf::st_coordinates(geometry)[,1],
      wpt = name,
      date_downloaded = lubridate::as_date(time)
    ) |>
    dplyr::select(wpt, lat, lon, date_downloaded) |>
    sf::st_drop_geometry()

  gps_data
}


#' Downloads the most recent template file to use for data entry
#'
#' @param folder_path the path of the folder to download the template to.
#' @export download_current_template
download_current_template <- function(folder_path) {

  template_file <- system.file("extdata", "LTMFTN_template_datasheet.xlsx", package = "LTMFNpackage")

  if(file.exists(file.path(folder_path, "LTMFTN_template_datasheet.xlsx"))){
    stop("Template file already exists at selected location. Delete old template or choose a new folder before proceeding")
}
  current_version <- readxl::read_excel(template_file,sheet = "entry_metadata")$template_version

  message(paste0(
  "You will be downloading version ",
  current_version,
  "of the template. It will be added to folder ",
  folder_path,
  ". Make sure that this is where you want the template, and that you have installed the most recent",
  " version of the LTMFNpackage from github before downloading a new template sheet."
  ))

  user_decision <- readline(prompt = "Would you like to proceed? (enter Y or N): ")

  if(user_decision %in% c("n","N") ){
    return(FALSE)
  } else if (user_decision %in% c("y", "Y")) {
    file.copy(from = template_file, to = folder_path, overwrite = FALSE)
    return(TRUE)
  } else {
    stop("Not a valid response. Template download not completed")
  }
}

