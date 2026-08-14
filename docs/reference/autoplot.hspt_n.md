# Plot map of grid counts

Plot the output produced by
[`hotspot_count`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_count.md)
with reasonable default values.

## Usage

``` r
# S3 method for class 'hspt_n'
autoplot(object, ...)

# S3 method for class 'hspt_n'
autolayer(object, ...)
```

## Arguments

- object:

  An object with the class `hspt_n`, e.g. as produced by
  [`hotspot_count`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_count.md).

- ...:

  further arguments passed to
  [`geom_sf`](https://ggplot2.tidyverse.org/reference/ggsf.html), e.g.
  `alpha`.

## Value

A [`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) object
or layer that can be used as part of a
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html) stack.

`autoplot` returns a `ggplot` object, meaning you can further control
the appearance of the plot by adding calls to further `ggplot2`
functions.

## Functions

- `autolayer(hspt_n)`: Create a ggplot layer of grid counts
