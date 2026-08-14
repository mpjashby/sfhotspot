# Plot map of changes in grid counts

Plot the output produced by
[`hotspot_change`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_change.md)
with reasonable default values.

## Usage

``` r
# S3 method for class 'hspt_d'
autoplot(object, ...)

# S3 method for class 'hspt_d'
autolayer(object, ...)
```

## Arguments

- object:

  An object with the class `hspt_d`, e.g. as produced by
  [`hotspot_change`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_change.md).

- ...:

  Currently ignored, but may be used for further options in future.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

This function returns a `ggplot` object, meaning you can further control
the appearance of the plot by adding calls to further `ggplot2`
functions.

## Functions

- `autolayer(hspt_d)`: Create a ggplot layer of change in grid counts
