# Plot map of changes in grid counts

Plot the output produced by
[`hotspot_change()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_change.md)
with reasonable defaults.

## Usage

``` r
# S3 method for class 'hspt_d'
autoplot(object, ...)

# S3 method for class 'hspt_d'
autolayer(object, ...)
```

## Arguments

- object:

  An object with class `hspt_d`, e.g. as produced by
  [`hotspot_change()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_change.md).

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
returns a layer that can be added to a
[ggplot2::ggplot](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Functions

- `autolayer(hspt_d)`: Create a ggplot layer of change in grid counts.
