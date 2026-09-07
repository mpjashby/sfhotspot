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
autoplot(object, ...)

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
