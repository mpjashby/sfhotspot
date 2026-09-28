# Identify change in hotspots over time

Identify change in the number of points (typically representing events)
between two periods (before and after a specified date) or in two groups
(e.g. on weekdays or at weekends).

## Usage

``` r
hotspot_change(
  data,
  time = NULL,
  boundary = NULL,
  groups = NULL,
  cell_size = NULL,
  grid_type = "rect",
  grid = NULL,
  quiet = FALSE
)
```

## Arguments

- data:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data frame
  containing points.

- time:

  Name of the column in `data` containing `Date` or `POSIXt` values
  representing the date associated with each point. Ignored if `groups`
  is not `NULL`. If this argument is `NULL` and `data` contains a single
  column of `Date` or `POSIXt` values, that column will be used
  automatically.

- boundary:

  A single `Date` or `POSIXt` value representing the point after which
  points should be treated as having occurred in the second time period.
  See 'Details'.

- groups:

  Name of a column in `data` containing exactly two unique non-missing
  values, which will be used to identify whether each row should be
  counted in the first (before) or second (after) groups. Which groups
  to use will be determined by calling `sort(unique(groups))`. If
  `groups` is not a factor, a message will be printed confirming which
  value has been used for which group. See 'Details'.

- cell_size:

  `numeric` value specifying the size of each equally spaced grid cell,
  using the same units (metres, degrees, etc.) as used in the `sf` data
  frame given in the `data` argument. Ignored if `grid` is not `NULL`.
  If this argument and `grid` are `NULL` (the default), the cell size
  will be calculated automatically (see Details).

- grid_type:

  `character` specifying whether the grid should be made up of squares
  (`"rect"`, the default) or hexagons (`"hex"`). Ignored if `grid` is
  not `NULL`.

- grid:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data frame
  containing points containing polygons, which will be used as the grid
  for which counts are made.

- quiet:

  if set to `TRUE`, messages reporting the values of any parameters set
  automatically will be suppressed. The default is `FALSE`.

## Value

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) tibble of
regular grid cells with corresponding hot-spot classifications for each
cell. This can be plotted using
[`autoplot`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## Details

This function creates a regular two-dimensional grid of cells (unless a
custom grid is specified with `grid`) and calculates the difference
between the number of points in each grid cell:

- before and after a set point in time, if `boundary` is specified,

- between two groups of points, if a column of grouping values is
  specified with `groups`,

- before and after the mid-point of the dates/times present in the data,
  if both `boundary` and `groups` are `NULL` (the default).

If both `boundary` and `groups` are not `NULL`, the value of `boundary`
will be ignored.

### Coverage of the output data

The grid produced by this function covers the convex hull of the input
data layer. This means the result may include zero counts for cells that
are outside the area for which data were provided, which could be
misleading. To handle this, consider cropping the output layer to the
area for which data are available. For example, if you only have crime
data for a particular district, crop the output dataset to the district
boundary using
[`st_intersection`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html).

### Automatic cell-size selection

If no cell size is given then the cell size will be set so that there
are 50 cells on the shorter side of the grid. If the `data` SF object is
projected in metres or feet, the number of cells will be adjusted
upwards so that the cell size is a multiple of 100.

## See also

[`hotspot_dual_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dual_kde.md)
for comparing the density of two layers, which will often be more useful
than comparing counts if the point locations represent and underlying
continuous distribution.

## Examples

``` r

# Compare counts from the first half of the period covered by the data to
# counts from the second half
# \donttest{
hotspot_change(memphis_robberies)
#> Boundary point set as 09:30 hours on 02 July 2019 automatically
#> Cell size set to 0.00524 degrees automatically
#> Simple feature collection with 2926 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -90.1261 ymin: 34.99475 xmax: -89.72786 ymax: 35.26199
#> Geodetic CRS:  WGS 84
#> # A tibble: 2,926 × 4
#>    n_before n_after change                                              geometry
#>  *    <dbl>   <dbl>  <dbl>                                         <POLYGON [°]>
#>  1        0       0      0 ((-90.08418 34.99475, -90.07894 34.99475, -90.07894 …
#>  2        0       0      0 ((-90.07894 34.99475, -90.0737 34.99475, -90.0737 34…
#>  3        0       0      0 ((-90.0737 34.99475, -90.06846 34.99475, -90.06846 3…
#>  4        0       0      0 ((-90.06846 34.99475, -90.06322 34.99475, -90.06322 …
#>  5        0       0      0 ((-90.06322 34.99475, -90.05798 34.99475, -90.05798 …
#>  6        0       0      0 ((-90.05798 34.99475, -90.05274 34.99475, -90.05274 …
#>  7        0       0      0 ((-90.05274 34.99475, -90.0475 34.99475, -90.0475 34…
#>  8        0       0      0 ((-90.0475 34.99475, -90.04226 34.99475, -90.04226 3…
#>  9        0       0      0 ((-90.04226 34.99475, -90.03702 34.99475, -90.03702 …
#> 10        0       0      0 ((-90.03702 34.99475, -90.03178 34.99475, -90.03178 …
#> # ℹ 2,916 more rows
# }

# Create a grouping variable, then compare counts across values of that
# variable
# \donttest{
memphis_robberies$weekend <-
  weekdays(memphis_robberies$date) %in% c("Saturday", "Sunday")
hotspot_change(memphis_robberies, groups = weekend)
#> Comparing periods based on values of `weekend`:
#> • Rows with `weekend == FALSE` used as 'before' period
#> • Rows with `weekend == TRUE` used as 'after' period
#> Cell size set to 0.00524 degrees automatically
#> Simple feature collection with 2926 features and 3 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -90.1261 ymin: 34.99475 xmax: -89.72786 ymax: 35.26199
#> Geodetic CRS:  WGS 84
#> # A tibble: 2,926 × 4
#>    n_before n_after change                                              geometry
#>  *    <dbl>   <dbl>  <dbl>                                         <POLYGON [°]>
#>  1        0       0      0 ((-90.08418 34.99475, -90.07894 34.99475, -90.07894 …
#>  2        0       0      0 ((-90.07894 34.99475, -90.0737 34.99475, -90.0737 34…
#>  3        0       0      0 ((-90.0737 34.99475, -90.06846 34.99475, -90.06846 3…
#>  4        0       0      0 ((-90.06846 34.99475, -90.06322 34.99475, -90.06322 …
#>  5        0       0      0 ((-90.06322 34.99475, -90.05798 34.99475, -90.05798 …
#>  6        0       0      0 ((-90.05798 34.99475, -90.05274 34.99475, -90.05274 …
#>  7        0       0      0 ((-90.05274 34.99475, -90.0475 34.99475, -90.0475 34…
#>  8        0       0      0 ((-90.0475 34.99475, -90.04226 34.99475, -90.04226 3…
#>  9        0       0      0 ((-90.04226 34.99475, -90.03702 34.99475, -90.03702 …
#> 10        0       0      0 ((-90.03702 34.99475, -90.03178 34.99475, -90.03178 …
#> # ℹ 2,916 more rows
# }
```
