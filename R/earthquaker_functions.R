#' Parse LOCATION_NAME field of NOOA dataset
#'
#' This function parses the LOCATION_NAME stripping out the country name
#' and converting it to title case
#'
#'
#' @param location_name represents the detailed information about location
#'
#' @import stringr
#'
#' @return This function returns the clean version of the LOCATION_NAME
#' 
#' @examples
#' \dontrun{
#' eq_location_clean(raw$LOCATION_NAME)
#' }
#' 
eq_location_clean <- function(location_name)
{
  new_location <- strsplit(location_name,":")
  new_location <- lapply(new_location,function(x){return(x[-1])})
  new_location <- lapply(new_location,function(x){return(paste0(x,collapse=""))})
  new_location <- str_to_title(new_location)
  new_location <- gsub("  ","",new_location)

  return(new_location)
}

#' Clean raw data of NOOA dataset
#'
#' This function converts time information to Date class,
#' it converts the longitude and latitude information to numeric
#' and it applies the eq_location_clean() function to LOCATION_NAME
#'
#'
#' @param raw contains the original raw dataset
#'
#' @return This function returns the clean version of the dataset
#'
#' @import dplyr
#'
#' @importFrom lubridate year
#'
#' @import readr
#'
#' @importFrom testthat expect_that
#' @importFrom testthat is_a
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' eq_clean_data(raw)
#' }
#' 
eq_clean_data <- function(raw)
{
  clean <- raw %>%
    mutate(YEAR=as.character(YEAR),MONTH=as.character(MONTH),DAY=as.character(DAY),LATITUDE=as.numeric(LATITUDE),LONGITUDE=as.numeric(LONGITUDE),INTENSITY=as.numeric(INTENSITY),DEATHS=as.numeric(DEATHS))

  year <- as.character(clean$YEAR)
  month <- as.character(clean$MONTH)
  month[is.na(month)] <- "01"
  day <- as.character(clean$DAY)
  day[is.na(day)] <- "01"

  parse_negative <- year[as.numeric(year) < 0]
  negative_date <- vector("list",length=length(parse_negative))
  for (i in 1:length(parse_negative))
  {
    negative_date[[i]] <- as.Date(seq(as.Date("0000-01-01"),length.out=2,by=paste(parse_negative[i],"year"))[2])
  }
  negative_date <- as.Date(as.numeric(unlist(negative_date)),origin="0000-01-01")

  positive_date <- as.Date(paste(year[as.numeric(year) >= 0],month[as.numeric(year) >= 0],day[as.numeric(year) >= 0],sep="-"))

  clean$date <- c(negative_date,positive_date)

  clean$LOCATION_NAME <- eq_location_clean(as.character(clean$LOCATION_NAME))

  return(clean)
}


##################


#' Build the Timeline Geom to create earthquakes rapresentation over time
#'
#' This function visualizes the times at which earthquakes occur within
#' certain countries. In addition, it displays the magnitudes (i.e. Richter
#' scale value) and the number of deaths associated with each earthquake
#'
#'
#' @inheritParams ggproto
#'
#' @param required_aes contains date, intensity and deaths information
#'
#' @param default_aes contains vertical position and info about color
#'
#' @return This function returns a geom to print the earthquakes over time
#'
#' @import grid
#'
#' @import ggplot2
#'
#' @export
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                         required_aes = c("x","i","d"),
                         default_aes = ggplot2::aes(y=0.5,col_intensity="grey",col_death="blue"),
                         draw_key = ggplot2::draw_key_point,
                         draw_panel = function(data, panel_scales, coord) {

                           coords <- coord$transform(data, panel_scales)

                           circles1 <- data.frame(x=coords$x, y=coords$y, r=coords$i*0.005, fill=coords$col_intensity)
                           circles2 <- data.frame(x=coords$x, y=coords$y, r=coords$d*0.000001, fill=coords$col_death)
                           circles <- rbind(circles1, circles2)

                           circleGrob(x = circles$x, y = circles$y, r = circles$r,
                                      gp = gpar(col=as.character(circles$fill), fill=as.character(circles$fill), alpha=0.75))
                         })

#' Build the Timeline Geom to create earthquakes rapresentation over time
#'
#' This function applies a ggplot layer of a GeomTimeline geom to a map
#'
#'
#' @inheritParams geom_polygon
#'
#' @return This function applies the layer of a GeomTimeline geom
#'
#' @import ggplot2
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' geom_timeline(to_plot, aes(x = date, y = COUNTRY, i = INTENSITY, d = DEATHS))
#' }
#' 
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' Build the TimelineLabel Geom to create labels over earthquakes rapresentation over time
#'
#' This function adds labels of the earthquakes on a Timeline representation
#'
#'
#' @inheritParams ggproto
#'
#' @param required_aes contains x, y positions and name of the earthquake
#'
#' @param default_aes
#'
#' @return This function returns a geom to print the labels of earthquakes over time
#'
#' @import grid
#'
#' @import ggplot2
#'
#' @export
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                        required_aes = c("x","y","label"),
                        default_aes = ggplot2::aes(),
                        draw_key = ggplot2::draw_key_point,
                        draw_panel = function(data, panel_scales, coord) {

                          coords <- coord$transform(data, panel_scales)

                          textGrob(coords$label, x=coords$x, y=coords$y, rot=45,
                                    gp=gpar(fontsize=7, col="black"))

                        })

#' Build the TimelineLabel Geom to create labels of earthquakes rapresentation over time
#'
#' This function applies a ggplot layer of a GeomTimelineLabel geom to a map
#'
#'
#' @inheritParams geom_polygon
#'
#' @return This function applies the layer of a GeomTimelineLabel geom
#'
#' @import ggplot2
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' geom_timeline_label(to_plot, aes(x = date, y = COUNTRY, label = LOCATION_NAME))
#' }
#' 
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


##################


#' Display location of earthquakes
#'
#' The function maps the epicenters (LATITUDE/LONGITUDE) and annotates
#' each point in a pop up window containing annotation data stored in a
#' column of the data frame
#'
#'
#' @param clean contains the clean dataset
#'
#' @param annot_col contains the column to be used as pop up window
#'
#' @return This function prints the leaflet map of the earthquakes
#'
#' @import ggplot2
#'
#' @import leaflet
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' eq_map(clean, annot_col = "popup_text")
#' }
#'
eq_map <- function(clean, annot_col)
{
  clean <- clean[,c("LATITUDE","LONGITUDE","EQ_PRIMARY",annot_col)]
  clean$EQ_PRIMARY <- as.numeric(clean$EQ_PRIMARY)

  map <- leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = clean, radius = clean$EQ_PRIMARY,
                     lng = ~ LONGITUDE, lat = ~ LATITUDE,
                     popup = ~ get(annot_col))

  return(map)
}

#' Create HTML pop up names
#'
#' The function creates an HTML label for the leaflet map
#' containing the location cleaned by the eq_location_clean() function,
#' the magnitude (EQ_PRIMARY), and the total number of deaths (TOTAL_DEATHS)
#'
#'
#' @param clean contains the clean dataset
#'
#' @return This function returns the HTML pop up
#'
#' @export
eq_create_label <- function(clean)
{
  valid_values <- !is.na(clean$LOCATION_NAME) & !is.na(clean$EQ_PRIMARY) & !is.na(clean$TOTAL_DEATHS)

  label <- rep("", nrow(clean))
  label[valid_values] <- paste("<b>Location:</b>", as.character(clean$LOCATION_NAME)[valid_values], "<br />",
        "<b>Magnitude:</b>", as.character(clean$EQ_PRIMARY)[valid_values], "<br />",
        "<b>Total deaths:</b>", as.character(clean$TOTAL_DEATHS)[valid_values], "<br />")

  return(label)
}
