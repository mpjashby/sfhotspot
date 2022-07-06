## code to prepare `memphis_population` dataset goes here

library(sf)
library(tidycensus)
library(tidyverse)

memphis_boundary <- tigris::places(state = 47) %>%
  filter(NAME == "Memphis") %>%
  st_transform(32615)

memphis_population <- get_decennial(
  geography = "block",
  variables = "P1_001N",
  year = 2020,
  state = "47",
  county = "157",
  geometry = TRUE
) %>%
  st_transform(32615) %>%
  st_centroid() %>%
  st_intersection(memphis_boundary) %>%
  st_transform(4326) %>%
  select(geoid = GEOID, population = value, geometry)

usethis::use_data(memphis_population, overwrite = TRUE)
