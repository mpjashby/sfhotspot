# Plot map of grid counts

Plot the output produced by
[`hotspot_count()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_count.md)
with reasonable default values. Weighted counts are plotted when the
object contains a `sum` column; otherwise unweighted counts in the `n`
column are plotted.

## Usage

``` r
# S3 method for class 'hspt_n'
autoplot(object, ...)

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
