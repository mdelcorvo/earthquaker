
#' Cleans source data from NOAA
#'
#' \code{eq_clean_data} cleans date, latitude and longitude, and location name
#' from the source NOAA data
#'
#' @param data A data frame with raw data obtained from NOAA website (see below)
#'
#' @return A data frame with cleaned date, latitude, longitude and location
#' columns
#'
#' @details The function requires raw date obtained from NOAA site
#' \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}. It adds
#' a column DATE with cleaned date (Date format), transforms LATITUDE and
#' LONGITUDE columns as numeric objects and transforms LOCATION_NAME by removing
#' the country and transforming to title case.
#'
#' @examples
#' \dontrun{
#' data <- readr::read_delim("earthquakes.tsv.gz", delim = "\t")
#' clean_data <- eq_clean_data(data)
#' }
#'
#' @importFrom dplyr %>% mutate select
#' @importFrom lubridate ymd
#' @importFrom stringr str_pad
#'
#' @export
eq_clean_data <- function(data) {
     data <- data %>%
          dplyr::mutate_(
               year_fix = ~stringr::str_pad(as.character(abs(YEAR)), width = 4,
                                            side = "left", pad = "0"),
               date_paste = ~paste(year_fix, MONTH, DAY, sep = "-"),
               DATE = ~lubridate::ymd(date_paste, truncated = 2)) %>%
          dplyr::select_(quote(-year_fix), quote(-date_paste))

     lubridate::year(data$DATE) <- data$YEAR

     data <- data %>%
          dplyr::mutate_(LATITUDE = ~as.numeric(LATITUDE),
                         LONGITUDE = ~as.numeric(LONGITUDE))

     data <- eq_location_clean(data)

     data
}


#' Cleans earthquake location data
#'
#' @param data A data frame with raw data obtained from NOAA website
#'
#' @return A data frame with cleaned LOCATION_NAME column
#'
#' @details This function transforms NOAA data frame LOCATION_NAME column by
#' trimming the country name (if applicable) and converting to title case
#'
#' @note The function is not exported
#'
#' @examples
#' \dontrun{
#' data <- readr::read_delim("earthquakes.tsv.gz", delim = "\t")
#' clean_data <- eq_location_clean(data)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom stringr str_replace str_trim str_to_title
eq_location_clean <- function(data) {
     data <- data %>%
          dplyr::mutate_(LOCATION_NAME = ~LOCATION_NAME %>%
                              stringr::str_replace(paste0(COUNTRY, ":"), "") %>%
                              stringr::str_trim("both") %>%
                              stringr::str_to_title())
     data
}
