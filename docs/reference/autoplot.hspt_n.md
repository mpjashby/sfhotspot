# Plot map of grid counts

Plot the output produced by
[`hotspot_count()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_count.md)
with reasonable default values. Weighted counts are plotted when the
object contains a `sum` column; otherwise unweighted counts in the `n`
column are plotted.

## Usage

``` r
# S3 method for class 'hspt_n'
autoplot(
  object,
  ...,
  basemap_type = "none",
  basemap_zoom = NULL,
  basemap_attribution = NULL
)

# S3 method for class 'hspt_n'
autolayer(object, ...)
```

## Arguments

- object:

  An object with class `hspt_n`, e.g. as produced by
  [`hotspot_count()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_count.md).

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
returns a layer that can be added to a
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Functions

- `autolayer(hspt_n)`: Create a ggplot layer of grid counts.
