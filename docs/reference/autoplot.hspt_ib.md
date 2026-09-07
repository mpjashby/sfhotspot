# Plot isobands

Plot the output produced by
[`hotspot_isoband()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_isoband.md)
using a sequential or diverging discrete scale appropriate to the
original hotspot result and selected value. Legend entries use the
ordered `label` column containing concise, automatically formatted
ranges.

## Usage

``` r
# S3 method for class 'hspt_ib'
autoplot(object, ...)

# S3 method for class 'hspt_ib'
autolayer(object, ...)
```

## Arguments

- object:

  An object with class `hspt_ib`, as produced by
  [`hotspot_isoband()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_isoband.md).

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

- `autolayer(hspt_ib)`: Create a ggplot layer of isobands.
