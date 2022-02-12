# This file creates a small sample dataset of robberies in Memphis in Jan 2019

library(dplyr)

memphis_robberies_jan <- crimedata::get_crime_data(
  years = 2019,
  cities = "Memphis",
  type = "core"
) %>%
  filter(offense_code == "12A", lubridate::month(date_single) == 1) %>%
  select(uid, offense_type, date = date_single, longitude, latitude) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

usethis::use_data(memphis_robberies_jan, overwrite = TRUE)
