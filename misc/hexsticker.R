library(hexSticker)
library(sf)
library(sfhotspot)
library(showtext)
library(tidyverse)

font_add_google("Red Hat Display", "roboto")

# These points were created by manually counting which cells in a grid in
# Inkscape overlapped with font outlines for the letters 'sf'
grid_points <- list(
  `1` = c(5:10, 18:21),
  `2` = c(3:12, 18:21),
  `3` = c(2:13, 18:21),
  `4` = c(1:5, 10:14, 18:21),
  `5` = c(1:4, 11:14, 18:21),
  `6` = c(10:14, 18:21),
  `7` = c(7:13, 18:21),
  `8` = c(4:13, 18:21),
  `9` = c(2:12, 18:21),
  `10` = c(2:10, 18:21),
  `11` = c(1:6, 18:21),
  `12` = c(1:4, 18:21),
  `13` = c(2:5, 10:13, 18:21),
  `14` = c(2:12, 16:24),
  `15` = c(3:12, 16:24),
  `16` = c(5:10, 18:21),
  `17` = 18:21,
  `18` = 18:22,
  `19` = 18:24,
  `20` = 19:24,
  `21` = 20:24
)

result <- map2_dfr(names(grid_points), grid_points, function (x, y) {
  tibble(
    lon = (y + runif(20 * length(y), min = -0.5, max = 0.5)),
    lat = (as.numeric(x) + runif(20 * length(y), min = -0.5, max = 0.5))
  )
}) %>%
  add_row(lon = -14, lat = -26) %>%
  add_row(lon = 41, lat = 32) %>%
  mutate(across(c(lon, lat), ~ . * 10000)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 27700) %>%
  hotspot_kde(cell_size = 20000, bandwidth = 25000, grid_type = "hex")

p <- ggplot(result) +
  geom_sf(aes(fill = kde), colour = "#c6dbef", size = 0.05) +
  scale_fill_gradient(low = "#08306b", high = "#6baed6", aesthetics = c("colour", "fill")) +
  theme_void() +
  theme_transparent() +
  theme(legend.position = "none")

sticker(
  p, s_x = 1, s_y = 1, s_width = 2.2, s_height = 2.2,
  package = "sfhotspot", p_y = 0.65, p_family = "roboto", p_fontface = "bold", p_size = 7,
  h_color = "#6baed6", h_size = 2.5,
  # url = "sfhotspot.lesscrime.info", u_color = "white", u_family = "roboto",
  white_around_sticker = TRUE,
  filename = "man/figures/logo.png"
)
