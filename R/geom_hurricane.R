#' Read storm observation from the `ebtrk_atlc_1988_2015.txt` file
#'
#' @param datadir Directory where the file is stored
#' @return The tidy data.frame representing the storms
#'
#' storm_id                date latitude longitude wind_speed  ne  nw  se
#' Katrina-2005 2005-08-29 12:00:00     29.5     -89.6         34 200 100 200
#' Katrina-2005 2005-08-29 12:00:00     29.5     -89.6         50 120  75 120
#' Katrina-2005 2005-08-29 12:00:00     29.5     -89.6         64  90  60  90
#' @export
read_storm_data <- function(datadir = system.file("extdata", package="CourseraRdataVisualizationFinal")) {
    ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                           4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
    ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                             "hour", "year", "latitude", "longitude",
                             "max_wind", "min_pressure", "rad_max_wind",
                             "eye_diameter", "pressure_1", "pressure_2",
                             paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                             paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                             paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                             "storm_type", "distance_to_land", "final")

    ext_tracks <- readr::read_fwf(file.path(datadir, "ebtrk_atlc_1988_2015.txt"),
                                  readr::fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                                  na = "-99")

    ext_tracks %>%
        dplyr::mutate(storm_id = paste(stringr::str_to_title(storm_name), year, sep = "-")) %>%
        dplyr::mutate(longitude = -longitude) %>%
        tidyr::unite(date, year, month, day, hour) %>%
        dplyr::mutate(date = lubridate::ymd_h(date)) %>%
        dplyr::select(storm_id, date, latitude, longitude, dplyr::starts_with("radius")) %>%
        tidyr::gather(wind_key, radius, -storm_id, -date, -latitude, -longitude) %>%
        tidyr::extract(wind_key, c("wind_speed", "wind_direction"), regex = "radius_([0-9]+)_([a-z]{2})", convert = TRUE) %>%
        tidyr::spread(wind_direction, radius) %>%
        dplyr::mutate(wind_speed = factor(wind_speed))

}

#' Compute the points representing the storm polygon
#'
#' @param x0 X coordinate of the center
#' @param y0 Y coordinate of the center
#' @param ne Radius of the north-east quarter
#' @param se Radius of the south-east quarter
#' @param sw Radius of the south-west quarter
#' @param nw Radius of the north-west quarter
#' @param scale_radii Scale of the polygon, default to 1
#' @return The resulting coordinates (data.frame containing columns `lat` and `lon` for coordiantes)
#' @export
polygon_points <- function(x0, y0, ne, nw, se, sw, scale_radii = 1) {
    angles <- 1:360
    r = c(rep(ne, 90), rep(se, 90), rep(sw, 90), rep(nw, 90))
    geosphere::destPoint(c(x0,y0), b = angles, d = r*1852*scale_radii) %>%
        as.data.frame %>%
        dplyr::select("lat", "lon")
}

#' Function to be use in ggplot2 to add a hurricane layer
#' @inheritParams ggplot2::geom_point
#' @export
geom_hurricane <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = TRUE, ...){
    ggplot2::layer(
        geom = GeomHurricane, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm,...)
    )
}


#' Geom prototype to draw the hurricane
GeomHurricane <- ggplot2::ggproto("GeomHurricane", ggplot2::Geom,
    required_aes = c("x", "y", "r_ne", "r_se", "r_nw", "r_sw"),
    default_aes = ggplot2::aes(color = "NA", fill = "grey20", size = 0.5, linetype = 1, alpha = 0.8, scale_radii = 1),
    draw_key = ggplot2::draw_key_polygon,
    draw_panel = function(data, panel_scales, coord) {
        print(paste("Start draw panel", data[1,]$x, data[1,]$y, data[1,]$r_ne, data[1,]$r_nw, data[1,]$r_se, data[1,]$r_sw))
        points <- polygon_points(data[1,]$x, data[1,]$y, data[1,]$r_ne, data[1,]$r_nw, data[1,]$r_se, data[1,]$r_sw) %>%
            dplyr::rename(x = lon, y = lat) %>%
            coord$transform(panel_scales)

        grid::polygonGrob(x = points$x,
                          y = points$y,
                          gp = grid::gpar(col = data[1,]$color, fill = data[1,]$fill, alpha = data[1,]$alpha))
    }
    )

#' Draw the Katrina hurricane
#'
#' @param datadir Directory where the file is stored
draw_katrina <- function(datadir = system.file("extdata", package="CourseraRdataVisualizationFinal")) {
    storm_data <- read_storm_data(datadir)
    katrina <- storm_data %>%
        dplyr::filter(storm_id == 'Katrina-2005' & date == lubridate::ymd_hm("2005-08-29 12:00"))
    library(ggmap)

    get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
        ggmap(extent = "device") +
        geom_hurricane(data = katrina,
                       aes(x = longitude, y = latitude,
                           r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                           fill = wind_speed,
                           color = wind_speed)) +
        scale_color_manual(name = "Wind speed (kts)",
                           values = c("red", "orange", "yellow")) +
        scale_fill_manual(name = "Wind speed (kts)",
                          values = c("red", "orange", "yellow"))
}
