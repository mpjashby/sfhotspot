# Count points in cells in a two-dimensional grid

Count points in cells in a two-dimensional grid

## Usage

``` r
hotspot_count(
  data,
  cell_size = NULL,
  grid_type = "rect",
  grid = NULL,
  weights = NULL,
  quiet = FALSE
)
```

## Arguments

- data:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data frame
  containing points.

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
  containing polygons, which will be used as the grid for which counts
  are made.

- weights:

  `NULL` or the name of a column in `data` to be used as weights for
  weighted counts.

- quiet:

  if set to `TRUE`, messages reporting the values of any parameters set
  automatically will be suppressed. The default is `FALSE`.

## Value

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) tibble of
regular grid cells with corresponding point counts for each cell. This
can be plotted using
[`autoplot`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## Details

This function counts the number of points in each cell in a regular
grid. If a column name in `data` is supplied with the `weights`
argument, weighted counts will also be produced.

### Automatic cell-size selection

If `grid` is `NULL` and no cell size is given, the cell size will be set
so that there are 50 cells on the shorter side of the grid. If the
`data` SF object is projected in metres or feet, the number of cells
will be adjusted upwards so that the cell size is a multiple of 100.

## Examples

``` r

# Set cell size automatically
# \donttest{
hotspot_count(memphis_robberies_jan)
#> Cell size set to 0.00512 degrees automatically
#> Simple feature collection with 2505 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -90.1261 ymin: 34.99475 xmax: -89.77282 ymax: 35.25587
#> Geodetic CRS:  WGS 84
#> # A tibble: 2,505 × 2
#>        n                                                                geometry
#>  * <dbl>                                                           <POLYGON [°]>
#>  1     0 ((-90.03394 34.99475, -90.02882 34.99475, -90.02882 34.99987, -90.0339…
#>  2     0 ((-90.02882 34.99475, -90.0237 34.99475, -90.0237 34.99987, -90.02882 …
#>  3     0 ((-90.0237 34.99475, -90.01858 34.99475, -90.01858 34.99987, -90.0237 …
#>  4     0 ((-90.01858 34.99475, -90.01346 34.99475, -90.01346 34.99987, -90.0185…
#>  5     1 ((-90.01346 34.99475, -90.00834 34.99475, -90.00834 34.99987, -90.0134…
#>  6     0 ((-90.00834 34.99475, -90.00322 34.99475, -90.00322 34.99987, -90.0083…
#>  7     0 ((-90.00322 34.99475, -89.9981 34.99475, -89.9981 34.99987, -90.00322 …
#>  8     0 ((-89.9981 34.99475, -89.99298 34.99475, -89.99298 34.99987, -89.9981 …
#>  9     0 ((-89.99298 34.99475, -89.98786 34.99475, -89.98786 34.99987, -89.9929…
#> 10     0 ((-90.05442 34.99987, -90.0493 34.99987, -90.0493 35.00499, -90.05442 …
#> # ℹ 2,495 more rows
# }

# Transform data to UTM zone 15N so that cell_size and bandwidth can be set
# in metres
library(sf)
#> Linking to GEOS 3.13.0, GDAL 3.8.5, PROJ 9.5.1; sf_use_s2() is TRUE
memphis_robberies_utm <- st_transform(memphis_robberies_jan, 32615)

# Manually set grid-cell size in metres, since the `memphis_robberies_utm`
# dataset uses a co-ordinate reference system (UTM zone 15 north) that is
# specified in metres
# \donttest{
hotspot_count(memphis_robberies_utm, cell_size = 200)
#> Simple feature collection with 16133 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 762136.2 ymin: 3876586 xmax: 794136.2 ymax: 3905386
#> Projected CRS: WGS 84 / UTM zone 15N
#> # A tibble: 16,133 × 2
#>        n                                                                geometry
#>  * <dbl>                                                           <POLYGON [m]>
#>  1     0 ((771936.2 3876586, 771936.2 3876786, 772136.2 3876786, 772136.2 38765…
#>  2     0 ((772136.2 3876586, 772136.2 3876786, 772336.2 3876786, 772336.2 38765…
#>  3     0 ((772336.2 3876586, 772336.2 3876786, 772536.2 3876786, 772536.2 38765…
#>  4     0 ((772536.2 3876586, 772536.2 3876786, 772736.2 3876786, 772736.2 38765…
#>  5     1 ((772736.2 3876586, 772736.2 3876786, 772936.2 3876786, 772936.2 38765…
#>  6     0 ((772936.2 3876586, 772936.2 3876786, 773136.2 3876786, 773136.2 38765…
#>  7     0 ((773136.2 3876586, 773136.2 3876786, 773336.2 3876786, 773336.2 38765…
#>  8     0 ((773336.2 3876586, 773336.2 3876786, 773536.2 3876786, 773536.2 38765…
#>  9     0 ((773536.2 3876586, 773536.2 3876786, 773736.2 3876786, 773736.2 38765…
#> 10     0 ((771136.2 3876786, 771136.2 3876986, 771336.2 3876986, 771336.2 38767…
#> # ℹ 16,123 more rows
# }
```
