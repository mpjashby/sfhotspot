# Estimate the relationship between the kernel density of two layers of points

Estimate the relationship between the kernel density of two layers of
points

## Usage

``` r
hotspot_dual_kde(
  x,
  y,
  cell_size = NULL,
  grid_type = "rect",
  bandwidth = NULL,
  bandwidth_adjust = 1,
  method = "ratio",
  grid = NULL,
  weights = NULL,
  transform = TRUE,
  quiet = FALSE,
  ...
)
```

## Arguments

- x, y:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data frames
  containing points.

- cell_size:

  `numeric` value specifying the size of each equally spaced grid cell,
  using the same units (metres, degrees, etc.) as used in the `sf` data
  frame given in the `x` argument. Ignored if `grid` is not `NULL`. If
  this argument and `grid` are `NULL` (the default), the cell size will
  be calculated automatically (see Details).

- grid_type:

  `character` specifying whether the grid should be made up of squares
  (`"rect"`, the default) or hexagons (`"hex"`). Ignored if `grid` is
  not `NULL`.

- bandwidth:

  either a single `numeric` value specifying the bandwidth to be used in
  calculating the kernel density estimates, or a list of exactly 2 such
  values. If this argument is `NULL` (the default), a single bandwidth
  will be determined automatically using the result of
  [`bandwidth.nrd`](https://rdrr.io/pkg/MASS/man/bandwidth.nrd.html)
  called on the co-ordinates of the points in `x`, then used for both
  `x` and `y`. If this argument is `list(NULL, NULL)`, separate
  bandwidths will be determined automatically for `x` and `y` based on
  each layer. For lon/lat data that are transformed automatically,
  bandwidths are specified or calculated in the units of the projected
  co-ordinate reference system.

- bandwidth_adjust:

  single positive `numeric` value by which the value of `bandwidth` for
  both `x` and `y` will be multiplied, or a list of two such values.
  Useful for setting the bandwidth relative to the default.

- method:

  `character` specifying the method by which the densities, `d()`, of
  `x` and `y` will be related:

  `ratio`

  : (the default) calculates the density of `x` divided by the density
    of `y`, i.e. `d(x) / d(y)`.

  `log`

  : calculates the natural logarithm of the density of `x` divided by
    the density of `y`, i.e. `log(d(x) / d(y))`.

  `diff`

  : calculates the difference between the density of `x` and the density
    of `y`, i.e. `d(x) - d(y)`.

  `sum`

  : calculates the sum of the density of `x` and the density of `y`,
    i.e. `d(x) + d(y)`.

  The result of this calculation will be returned in the `kde` column of
  the return value.

- grid:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data frame
  containing polygons, which will be used as the grid for which
  densities are estimated. Both `x` and `y` must overlap the grid and
  use the same co-ordinate reference system.

- weights:

  `NULL` (the default) or a vector of length two giving either `NULL` or
  the name of a column in each of `x` and `y` to be used as weights for
  weighted counts and KDE values.

- transform:

  the underlying SpatialKDE package cannot calculate kernel density for
  lon/lat data, so this must be transformed to use a projected
  co-ordinate reference system. If this argument is `TRUE` (the default)
  and `sf::st_is_longlat(x)` is `TRUE`, `x`, `y` and `grid` (if
  provided) will be transformed to a common projected co-ordinate
  reference system chosen using
  [`st_transform_auto`](https://pkgs.lesscrime.info/sfhotspot/reference/st_transform_auto.md)
  before bandwidths and kernel densities are estimated. The result is
  returned using the original co-ordinate reference system. Set this
  argument to `FALSE` to suppress automatic transformation of the data.

- quiet:

  if set to `TRUE`, messages reporting the values of any parameters set
  automatically will be suppressed. The default is `FALSE`.

- ...:

  Further arguments passed to
  [`kde`](https://rdrr.io/pkg/SpatialKDE/man/kde.html).

## Value

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) tibble of
grid cells with corresponding point counts and dual kernel density
estimates for each cell. This can be plotted using
[`autoplot`](https://ggplot2.tidyverse.org/reference/autoplot.html).

This function creates a regular two-dimensional grid of cells (unless a
custom grid is specified with `grid`), calculates the density of points
in each cell for each of `x` and `y` using functions from the
`SpatialKDE` package, then produces a value representing a relation
between the two densities. The count of points in each cell is also
returned.

Dual kernel density values can be useful for understanding the
relationship between the distributions of two sets of point locations.
For example:

- The ratio between two densities representing the locations of
  burglaries and the locations of houses can show the distribution of
  the risk (incidence rate) of burglaries. The logged ratio may be
  useful to show relationships where one set of points has an extremely
  skewed distribution.

- The difference between two densities can show the change in
  distributions between two points in time.

- The sum of two densities can be used to estimate the total density of
  two types of point, e.g. the locations of occurrences of two diseases.

### Coverage of the output data

The grid produced by this function covers the convex hull of the points
in `x`. This means the result may include KDE values for cells that are
outside the area for which data were provided, which could be
misleading. To handle this, consider cropping the output layer to the
area for which data are available. For example, if you only have crime
data for a particular district, crop the output dataset to the district
boundary using
[`st_intersection`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html).

### Automatic cell-size selection

If no cell size is given then the cell size will be set so that there
are 50 cells on the shorter side of the grid. If the `x` SF object is
projected in metres or feet, the number of cells will be adjusted
upwards so that the cell size is a multiple of 100.

## References

Yin, P. (2020). Kernels and Density Estimation. *The Geographic
Information Science & Technology Body of Knowledge* (1st Quarter 2020
Edition), John P. Wilson (ed.).
doi:[doi:10.22224/gistbok/2020.1.12](https://doi.org/10.22224/gistbok/2020.1.12)

## Examples

``` r
# See also the examples for `hotspot_kde()` for examples of how to specify
# `cell_size`, `bandwidth`, etc.

library(sf)

# Transform data to UTM zone 15N so that cell_size and bandwidth can be set
# in metres
memphis_robberies_utm <- st_transform(memphis_robberies, 32615)
memphis_population_utm <- st_transform(memphis_population, 32615)

# Calculate burglary risk based on residential population. `weights` is set
# to `c(NULL, population)` so that the robberies layer is not weighted and
# the population layer is weighted according to the number of residents in
# each census block.
# \donttest{
hotspot_dual_kde(
  memphis_robberies_utm,
  memphis_population_utm,
  bandwidth = list(NULL, NULL),
  weights = c(NULL, population)
)
#> Cell size set to 500 metres automatically
#> Bandwidth set automatically based on rule of thumb.
#> ℹ Bandwidth for `x` = 5,592 metres.
#> Bandwidth set automatically based on rule of thumb.
#> ℹ Bandwidth for `y` = 5,175 metres.
#> Simple feature collection with 3302 features and 2 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 761931.5 ymin: 3876436 xmax: 797931.5 ymax: 3906436
#> Projected CRS: WGS 84 / UTM zone 15N
#> # A tibble: 3,302 × 3
#>        n     kde                                                        geometry
#>  * <dbl>   <dbl>                                                   <POLYGON [m]>
#>  1     0 0.00228 ((762931.5 3876436, 762931.5 3876936, 763431.5 3876936, 763431…
#>  2     0 0.00214 ((763431.5 3876436, 763431.5 3876936, 763931.5 3876936, 763931…
#>  3     0 0.00203 ((763931.5 3876436, 763931.5 3876936, 764431.5 3876936, 764431…
#>  4     0 0.00194 ((764431.5 3876436, 764431.5 3876936, 764931.5 3876936, 764931…
#>  5     0 0.00188 ((764931.5 3876436, 764931.5 3876936, 765431.5 3876936, 765431…
#>  6     0 0.00185 ((765431.5 3876436, 765431.5 3876936, 765931.5 3876936, 765931…
#>  7     0 0.00184 ((765931.5 3876436, 765931.5 3876936, 766431.5 3876936, 766431…
#>  8     0 0.00185 ((766431.5 3876436, 766431.5 3876936, 766931.5 3876936, 766931…
#>  9     0 0.00188 ((766931.5 3876436, 766931.5 3876936, 767431.5 3876936, 767431…
#> 10     0 0.00195 ((767431.5 3876436, 767431.5 3876936, 767931.5 3876936, 767931…
#> # ℹ 3,292 more rows
# }
```
