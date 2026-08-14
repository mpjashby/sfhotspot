# Plot map of hotspot classifications

Plot the output produced by
[`hotspot_classify`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify.md)
with reasonable default values.

## Usage

``` r
# S3 method for class 'hspt_c'
autoplot(object, ...)
```

## Arguments

- object:

  An object with the class `hspt_c`, e.g. as produced by
  [`hotspot_classify`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify.md).

- ...:

  Currently ignored, but may be used for further options in future.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

This function returns a `ggplot` object, meaning you can further control
the appearance of the plot by adding calls to further `ggplot2`
functions.
