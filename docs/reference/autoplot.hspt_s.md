# Plot DBSCAN hotspot clusters

Plot the polygon clusters produced by
[`hotspot_dbscan()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dbscan.md)
with reasonable defaults. Polygons can be filled according to their
count, proportion or rank, and optionally labelled with the same values.

## Usage

``` r
# S3 method for class 'hspt_s'
autoplot(
  object,
  fill = c("n", "prop", "rank", "none"),
  label = "none",
  ...,
  basemap_type = "none",
  basemap_zoom = NULL,
  basemap_attribution = NULL
)

# S3 method for class 'hspt_s'
autolayer(object, fill = c("n", "prop", "rank", "none"), label = "none", ...)
```

## Arguments

- object:

  An object with class `hspt_s`, as produced by
  [`hotspot_dbscan()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dbscan.md).

- fill:

  A single string specifying the column used for the fill aesthetic:
  `"n"` (the default), `"prop"` or `"rank"`. Use `"none"` to draw
  unfilled polygons with borders coloured according to the first value
  in `label`, or according to `"n"` if labels are not shown.

- label:

  One or more strings specifying the labels shown in each cluster:
  `"none"` (the default), or any combination of `"n"`, `"prop"` and
  `"rank"`. Multiple labels are separated by newlines. Proportions are
  formatted as percentages and ranks as ordinal numbers.

- ...:

  Further arguments passed to
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html),
  e.g. `alpha`.

- basemap_type:

  A map type passed to the `type` argument of
  [`ggspatial::annotation_map_tile()`](https://paleolimbot.github.io/ggspatial/reference/annotation_map_tile.html),
  or `"none"` (the default) for no base map. A base map requires the
  suggested `ggspatial` package and an internet connection unless the
  required tiles are cached. When a base map is used, the hotspot layer
  is drawn with `alpha = 0.75`, overriding any `alpha` value supplied in
  `...`.

- basemap_zoom:

  The zoom level passed to
  [`ggspatial::annotation_map_tile()`](https://paleolimbot.github.io/ggspatial/reference/annotation_map_tile.html),
  or `NULL` to choose it automatically.

- basemap_attribution:

  Attribution for the tile provider, or `NULL` to use the statement
  known for `basemap_type`. A non-empty string is required for a custom
  or otherwise unknown map type.

## Value

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
returns a
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.
[`autolayer()`](https://ggplot2.tidyverse.org/reference/autolayer.html)
returns one or more layers that can be added to a
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Functions

- `autolayer(hspt_s)`: Create ggplot layers for DBSCAN hotspot clusters.
