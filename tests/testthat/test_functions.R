context("Test functions in the package")

filename <- system.file("extdata/earthquakes.tsv.gz", package = "earthquaker")
data <- readr::read_delim(filename, delim = "\t")

test_that("eq_clean_data returns data frame", {
     expect_is(eq_clean_data(data), "data.frame")
})

test_that("eq_clean_data$DATE is Date type", {
     expect_is(eq_clean_data(data)$DATE, "Date")
})

test_that("eq_clean_data returns numeric coordinates", {
     expect_is(eq_clean_data(data)$LATITUDE, "numeric")
     expect_is(eq_clean_data(data)$LONGITUDE, "numeric")
})

test_that("eq_location_clean returns a data frame", {
     expect_is(eq_location_clean(data), "data.frame")
})

test_that("geom_timeline returns ggplot object", {
     g <- data %>% eq_clean_data() %>%
          dplyr::filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
          ggplot2::ggplot(ggplot2::aes(x = DATE,
                                       y = COUNTRY,
                                       color = as.numeric(TOTAL_DEATHS),
                                       size = as.numeric(EQ_PRIMARY)
          )) +
          geom_timeline()
     expect_is(g, "ggplot")
})

test_that("geom_timeline_label returns ggplot object", {
     g <- data %>% eq_clean_data() %>%
          dplyr::filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
          ggplot2::ggplot(ggplot2::aes(x = DATE,
                                       y = COUNTRY,
                                       color = as.numeric(TOTAL_DEATHS),
                                       size = as.numeric(EQ_PRIMARY)
          )) +
          geom_timeline_label(aes(label = LOCATION_NAME))
     expect_is(g, "ggplot")
})

test_that("theme_timeline returns ggplot object", {
     g <- data %>% eq_clean_data() %>%
          dplyr::filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
          ggplot2::ggplot(ggplot2::aes(x = DATE,
                                       y = COUNTRY,
                                       color = as.numeric(TOTAL_DEATHS),
                                       size = as.numeric(EQ_PRIMARY)
          )) +
          theme_timeline()
     expect_is(g, "ggplot")
})

test_that("eq_map returns leaflet object", {
     l <- data %>%
          eq_clean_data() %>%
          dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
          dplyr::mutate(popup_text = eq_create_label(.)) %>%
          eq_map(annot_col = "popup_text")
     expect_is(l, "leaflet")
})

test_that("eq_create_label returns character vector", {
     expect_is(eq_create_label(data), "character")
})

test_that("eq_create_label has the same length as input df", {
     expect_equal(length(eq_create_label(data)), dim(data)[1])
})
