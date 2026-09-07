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
  fill = c("n", "prop", "rank"),
  label = c("none", "n", "prop", "rank"),
  ...
)

# S3 method for class 'hspt_s'
autolayer(
  object,
  fill = c("n", "prop", "rank"),
  label = c("none", "n", "prop", "rank"),
  ...
)
```

## Arguments

- object:

  An object with class `hspt_s`, as produced by
  [`hotspot_dbscan()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dbscan.md).

- fill:

  A single string specifying the column used for the fill aesthetic:
  `"n"` (the default), `"prop"` or `"rank"`.

- label:

  A single string specifying the label shown in each cluster: `"none"`
  (the default), `"n"`, `"prop"` or `"rank"`. Proportions are formatted
  as percentages and ranks as ordinal numbers.

- ...:

  Further arguments passed to
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html),
  e.g. `alpha`.

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
