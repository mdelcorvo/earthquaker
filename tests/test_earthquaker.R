earthquakes <- readr::read_delim("signif.txt",delim="\t")
earthquakes <- capstone::eq_clean_data(earthquakes)

testthat::expect_that(earthquakes,testthat::is_a("data.frame"))



x <- as.Date("2000-01-01")
xmax <- as.Date("2017-01-01")
countries <- c("ITALY","USA")
n_max <- 10

to_plot <- earthquakes
to_plot <- dplyr::filter(to_plot, date >= x & date <=xmax & (COUNTRY %in% countries))
to_plot <- dplyr::filter(to_plot, !is.na(INTENSITY) & !is.na(DEATHS))
to_plot <- dplyr::mutate(to_plot, COUNTRY = factor(COUNTRY, levels = unique(COUNTRY)))

to_plot2 <- to_plot[order(to_plot$INTENSITY,decreasing = TRUE),]
to_plot2 <- to_plot2[1:min(n_max,nrow(to_plot2)),]

g <- ggplot2::ggplot(data = to_plot) +
  ggplot2::geom_segment(ggplot2::aes(x = x, xend = xmax, y = COUNTRY, yend = COUNTRY),
               alpha = 0.5, color = "gray") +
  capstone::geom_timeline(ggplot2::aes(x = date, y = COUNTRY, i = INTENSITY, d = DEATHS)) +
  ggplot2::geom_segment(data = to_plot2, ggplot2::aes(x = date, xend = date, y = COUNTRY, yend = as.numeric(COUNTRY) + 0.25),
               alpha = 0.5, color = "gray") +
  capstone::geom_timeline_label(data = to_plot2, ggplot2::aes(x = date, y = as.numeric(COUNTRY) + 0.4, label = LOCATION_NAME)) +
  ggplot2::theme_minimal()

testthat::expect_that(g,testthat::is_a("ggplot"))



g <- readr::read_delim("signif.txt", delim = "\t")
g <- capstone::eq_clean_data(g)
g <- dplyr::filter(g, COUNTRY == "MEXICO" & lubridate::year(date) >= 2000)
g <- dplyr::mutate(g, popup_text = capstone::eq_create_label(g))
g <- capstone::eq_map(g, annot_col = "popup_text")

testthat::expect_that(g,testthat::is_a("leaflet"))
