storm_data = read_storm_data()
expect_that(colnames(storm_data),
            equals(c("storm_id", "date", "latitude", "longitude", "wind_speed", "ne", "nw", "se", "sw")))

katrina <- storm_data %>%
  dplyr::filter(storm_id == 'Katrina-2005' & date == lubridate::ymd_hm("2005-08-29 12:00"))

expect_equal(nrow(katrina), 3)
