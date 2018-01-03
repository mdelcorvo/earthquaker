
#' Leaflet map of earthquakes
#'
#' This function creates a \code{leaflet} map of selected earthquakes based on
#' input NOAA earthquake cleaned data.
#'
#' @param data A data frame containing cleaned NOAA earthquake data
#' @param annot_col A character. The name of the column in the data that should
#' be used as descriptor.
#'
#' @return A leaflet map with earthquakes and annotations.
#' @export
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#'
#' @examples
#' \dontrun{
#' eq_map(data, annot_col = "LOCATION_NAME")
#' }
eq_map <- function(data, annot_col) {

     m <- leaflet::leaflet() %>%
          leaflet::addTiles() %>%
          leaflet::addCircleMarkers(lng = data$LONGITUDE, lat = data$LATITUDE,
                                    radius = data$EQ_PRIMARY, weight = 1,
                                    popup = data[[annot_col]])

     m
}


#' Creates a label for leaflet map
#'
#' This function creates a label for the \code{leaflet} map based on location
#' name, magnitude and casualties from NOAA earthquake data
#'
#' @param data A data frame containing cleaned NOAA earthquake data
#'
#' @return A character vector with labels
#'
#' @details The input \code{data.frame} needs to include columns LOCATION_NAME,
#' EQ_PRIMARY and TOTAL_DEATHS with the earthquake location, magintude and
#' total casualties respectively.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eq_create_label(data)
#' }
eq_create_label <- function(data) {
     popup_text <- with(data, {
          part1 <- ifelse(is.na(LOCATION_NAME), "",
                          paste("<strong>Location:</strong>",
                                LOCATION_NAME))
          part2 <- ifelse(is.na(EQ_PRIMARY), "",
                          paste("<br><strong>Magnitude</strong>",
                                EQ_PRIMARY))
          part3 <- ifelse(is.na(TOTAL_DEATHS), "",
                          paste("<br><strong>Total deaths:</strong>",
                                TOTAL_DEATHS))
          paste0(part1, part2, part3)
     })
}
