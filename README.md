# earthquaker - NOAA earthquake visualization package

An R package for the visualization of the NOAA earthquake data

## Description

The package includes several exported functions to handle NOAA data. The provided data set includes data on earthquakes starting year 2150 B.C. and contains dates, locations, magnitudes, severity (casualties, injuries...) and other details. 

In particular, it provides:

    Functions for visualizing data over time
    Functions for visualizing data over space
    Functions for cleaning data, parsing strings, converting fields in the appropriate format

## Example

After downloading data from the NOAA database, the package is able to process and visualize them using the following example:

```r
filename <- system.file("extdata/earthquakes.tsv.gz", package = "earthquakeVis")
data <- readr::read_delim(filename, delim = "\t")
data %>% eq_clean_data() %>%
     filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
     ggplot(aes(x = DATE,
                y = COUNTRY,
                color = as.numeric(TOTAL_DEATHS),
                size = as.numeric(EQ_PRIMARY)
     )) +
     geom_timeline() +
     geom_timeline_label(aes(label = LOCATION_NAME), n_max = 5) +
     theme_timeline() +
     labs(size = "Richter scale value", color = "# deaths") + 
     scale_x_date(limits = c(lubridate::ymd("2000-01-01"), 
                             lubridate::ymd("2020-01-01")))
```
## Author

[Marcello Del Corvo](https://github.com/mdelcorvo)
