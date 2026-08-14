# Estimate two-dimensional kernel density of points

Estimate two-dimensional kernel density of points

## Usage

``` r
hotspot_kde(
  data,
  cell_size = NULL,
  grid_type = "rect",
  bandwidth = NULL,
  bandwidth_adjust = 1,
  grid = NULL,
  weights = NULL,
  transform = TRUE,
  quiet = FALSE,
  ...
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

- bandwidth:

  `numeric` value specifying the bandwidth to be used in calculating the
  kernel density estimates. If this argument is `NULL` (the default),
  the bandwidth will be determined automatically using the result of
  [`bandwidth.nrd`](https://rdrr.io/pkg/MASS/man/bandwidth.nrd.html)
  called on the co-ordinates of `data`.

- bandwidth_adjust:

  single positive `numeric` value by which the value of `bandwidth` is
  multiplied. Useful for setting the bandwidth relative to the default.

- grid:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data frame
  containing polygons, which will be used as the grid for which
  densities are estimated.

- weights:

  `NULL` or the name of a column in `data` to be used as weights for
  weighted counts and KDE values.

- transform:

  the underlying SpatialKDE package cannot calculate kernel density for
  lon/lat data, so this must be transformed to use a projected
  co-ordinate reference system. If this argument is `TRUE` (the default)
  and `sf::st_is_longlat(data)` is `TRUE`, `data` (and `grid` if
  provided) will be transformed automatically using
  `link{st_transform_auto}` before the kernel density is estimated and
  transformed back afterwards. Set this argument to `FALSE` to suppress
  automatic transformation of the data.

- quiet:

  if set to `TRUE`, messages reporting the values of any parameters set
  automatically will be suppressed. The default is `FALSE`.

- ...:

  Further arguments passed to
  [`kde`](https://rdrr.io/pkg/SpatialKDE/man/kde.html).

## Value

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) tibble of
grid cells with corresponding point counts and kernel density estimates
for each cell. This can be plotted using
[`autoplot`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## Details

This function creates a regular two-dimensional grid of cells (unless a
custom grid is specified with `grid`) and calculates the density of
points in each cell on that grid using functions from the `SpatialKDE`
package. The count of points in each cell is also returned.

### Coverage of the output data

The grid produced by this function covers the convex hull of the input
data layer. This means the result may include KDE values for cells that
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

## References

Yin, P. (2020). Kernels and Density Estimation. *The Geographic
Information Science & Technology Body of Knowledge* (1st Quarter 2020
Edition), John P. Wilson (ed.).
doi:[doi:10.22224/gistbok/2020.1.12](https://doi.org/10.22224/gistbok/2020.1.12)

## Examples

``` r
library(sf)

# Transform data to UTM zone 15N so that cell_size and bandwidth can be set
# in metres
memphis_robberies_utm <- st_transform(memphis_robberies_jan, 32615)

# Automatically set grid-cell size, bandwidth and neighbour distance
# \donttest{
hotspot_kde(memphis_robberies_utm)
#> Cell size set to 500 metres automatically
#> Bandwidth set automatically based on rule of thumb.
#> ℹ Bandwidth = 8,877 metres.
#> Simple feature collection with 2715 features and 2 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 761986.2 ymin: 3876436 xmax: 794486.2 ymax: 3905936
#> Projected CRS: WGS 84 / UTM zone 15N
#> # A tibble: 2,715 × 3
#>        n   kde                                                          geometry
#>  * <dbl> <dbl>                                                     <POLYGON [m]>
#>  1     0 11.1  ((770486.2 3876436, 770486.2 3876936, 770986.2 3876936, 770986.2…
#>  2     0 11.2  ((770986.2 3876436, 770986.2 3876936, 771486.2 3876936, 771486.2…
#>  3     0 11.2  ((771486.2 3876436, 771486.2 3876936, 771986.2 3876936, 771986.2…
#>  4     0 11.1  ((771986.2 3876436, 771986.2 3876936, 772486.2 3876936, 772486.2…
#>  5     1 10.9  ((772486.2 3876436, 772486.2 3876936, 772986.2 3876936, 772986.2…
#>  6     0 10.5  ((772986.2 3876436, 772986.2 3876936, 773486.2 3876936, 773486.2…
#>  7     0 10.0  ((773486.2 3876436, 773486.2 3876936, 773986.2 3876936, 773986.2…
#>  8     0  9.45 ((773986.2 3876436, 773986.2 3876936, 774486.2 3876936, 774486.2…
#>  9     0  8.76 ((774486.2 3876436, 774486.2 3876936, 774986.2 3876936, 774986.2…
#> 10     0 10.1  ((768486.2 3876936, 768486.2 3877436, 768986.2 3877436, 768986.2…
#> # ℹ 2,705 more rows
# }

# Manually set grid-cell size and bandwidth in metres, since the
# `memphis_robberies_utm` dataset uses a co-ordinate reference system (UTM
# zone 15 north) that is specified in metres
# \donttest{
hotspot_kde(memphis_robberies_utm, cell_size = 200, bandwidth = 1000)
#> Simple feature collection with 16133 features and 2 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 762136.2 ymin: 3876586 xmax: 794136.2 ymax: 3905386
#> Projected CRS: WGS 84 / UTM zone 15N
#> # A tibble: 16,133 × 3
#>        n    kde                                                         geometry
#>  * <dbl>  <dbl>                                                    <POLYGON [m]>
#>  1     0 0.0658 ((771936.2 3876586, 771936.2 3876786, 772136.2 3876786, 772136.…
#>  2     0 0.315  ((772136.2 3876586, 772136.2 3876786, 772336.2 3876786, 772336.…
#>  3     0 0.618  ((772336.2 3876586, 772336.2 3876786, 772536.2 3876786, 772536.…
#>  4     0 0.867  ((772536.2 3876586, 772536.2 3876786, 772736.2 3876786, 772736.…
#>  5     1 1.00   ((772736.2 3876586, 772736.2 3876786, 772936.2 3876786, 772936.…
#>  6     0 0.997  ((772936.2 3876586, 772936.2 3876786, 773136.2 3876786, 773136.…
#>  7     0 0.821  ((773136.2 3876586, 773136.2 3876786, 773336.2 3876786, 773336.…
#>  8     0 0.518  ((773336.2 3876586, 773336.2 3876786, 773536.2 3876786, 773536.…
#>  9     0 0.208  ((773536.2 3876586, 773536.2 3876786, 773736.2 3876786, 773736.…
#> 10     0 0.183  ((771136.2 3876786, 771136.2 3876986, 771336.2 3876986, 771336.…
#> # ℹ 16,123 more rows
# }
```
