# This file creates a small sample dataset of robberies in Memphis in 2019

library(dplyr)

memphis_robberies <- crimedata::get_crime_data(
  years = 2019,
  cities = "Memphis",
  type = "core"
) %>%
  filter(offense_code == "12A") %>%
  select(uid, offense_type, date = date_single, longitude, latitude) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

usethis::use_data(memphis_robberies, overwrite = TRUE)
