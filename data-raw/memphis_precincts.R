# This script prepares the `memphis_precincts` dataset

memphis_precincts <- sf::read_sf("https://data.memphistn.gov/resource/rqqz-pj4u.geojson") |>
  dplyr::group_by(precinct) |>
  dplyr::summarise()

usethis::use_data(memphis_precincts, overwrite = TRUE)
