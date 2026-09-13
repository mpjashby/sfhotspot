# Plot map of dual kernel-density values

Plot the output produced by
[`hotspot_dual_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dual_kde.md)
using a scale appropriate to the comparison method. Ratios use a
logarithmic diverging scale centred on one; logged ratios and
differences use diverging scales centred on zero; sums use a sequential
scale.

## Usage

``` r
# S3 method for class 'hspt_dk'
autoplot(
  object,
  ...,
  basemap_type = "none",
  basemap_zoom = NULL,
  basemap_attribution = NULL
)

# S3 method for class 'hspt_dk'
autolayer(object, ...)
```

## Arguments

- object:

  An object with class `hspt_dk`, e.g. as produced by
  [`hotspot_dual_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dual_kde.md).
  The object must have a valid `method` attribute.

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

- `autolayer(hspt_dk)`: Create a ggplot layer of dual density values.
