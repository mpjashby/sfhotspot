# Plot map of Getis-Ord Gi\* results

If `object` contains a `kde` column, density is shown only in cells in
which the Gi\*/Gi result passes the specified significance and sign
conditions. The `pvalue` column is used as supplied and is not adjusted
by the plotting methods. Cells that do not satisfy the conditions are
transparent. If `object` does not contain a `kde` column, the Gi\*/Gi
value is plotted using a diverging scale centred on zero and
`critical_p` and `sign` do not affect the mapped values.

## Usage

``` r
# S3 method for class 'hspt_g'
autoplot(object, critical_p = 0.05, sign = c("both", "hot", "cold"), ...)

# S3 method for class 'hspt_g'
autolayer(object, critical_p = 0.05, sign = c("both", "hot", "cold"), ...)
```

## Arguments

- object:

  An object with class `hspt_g`, e.g. as produced by
  [`hotspot_gistar()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_gistar.md).

- critical_p:

  A single numeric value specifying the largest p-value to treat as
  statistically significant when plotting density.

- sign:

  Which significant results should show density: `"both"` (the default),
  `"hot"` for positive Gi\*/Gi values, or `"cold"` for negative values.

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

- `autolayer(hspt_g)`: Create a ggplot layer of Getis-Ord Gi\* results.
