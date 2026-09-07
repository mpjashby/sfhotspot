# Plot map of hotspot classifications

Plot the output produced by
[`hotspot_classify()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify.md)
with reasonable defaults.

## Usage

``` r
# S3 method for class 'hspt_c'
autoplot(object, ...)

# S3 method for class 'hspt_c'
autolayer(object, ...)
```

## Arguments

- object:

  An object with class `hspt_c`, e.g. as produced by
  [`hotspot_classify()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify.md).

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

- `autolayer(hspt_c)`: Create a ggplot layer of hotspot classifications.
