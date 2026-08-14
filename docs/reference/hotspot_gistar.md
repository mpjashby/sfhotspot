# Identify significant spatial clusters of points

Identify hotspot and coldspot locations, that is cells in a regular grid
in which there are more/fewer points than would be expected if the
points were distributed randomly.

## Usage

``` r
hotspot_gistar(
  data,
  cell_size = NULL,
  grid_type = "rect",
  kde = TRUE,
  bandwidth = NULL,
  bandwidth_adjust = 1,
  grid = NULL,
  weights = NULL,
  nb_dist = NULL,
  include_self = TRUE,
  p_adjust_method = NULL,
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

- kde:

  `TRUE` (the default) or `FALSE` indicating whether kernel density
  estimates (KDE) should be produced for each grid cell.

- bandwidth:

  `numeric` value specifying the bandwidth to be used in calculating the
  kernel density estimates. If this argument is `NULL` (the default),
  the bandwidth will be specified automatically using the mean result of
  [`bandwidth.nrd`](https://rdrr.io/pkg/MASS/man/bandwidth.nrd.html)
  called on the `x` and `y` co-ordinates separately.

- bandwidth_adjust:

  single positive `numeric` value by which the value of `bandwidth` is
  multiplied. Useful for setting the bandwidth relative to the default.

- grid:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data frame
  containing polygons, which will be used as the grid for which counts
  are made.

- weights:

  `NULL` or the name of a column in `data` to be used as weights for
  weighted counts and KDE values.

- nb_dist:

  The distance around a cell that contains the neighbours of that cell,
  which are used in calculating the statistic. If this argument is
  `NULL` (the default), `nb_dist` is set as `cell_size * sqrt(2)` so
  that only the cells immediately adjacent to each cell are treated as
  being its neighbours.

- include_self:

  Should points in a given cell be counted as well as counts in
  neighbouring cells when calculating the values of *G*_(*i*)^(\*) (if
  `include_self = TRUE`, the default) or *G*_(*i*)^(\*) (if
  `include_self = FALSE`) values? You are unlikely to want to change the
  default value.

- p_adjust_method:

  The method to be used to adjust *p*-values for multiple comparisons.
  `NULL` (the default) uses the default method used by
  [`p.adjust`](https://rdrr.io/r/stats/p.adjust.html), but any of the
  character values in
  [`stats::p.adjust.methods`](https://rdrr.io/r/stats/p.adjust.html) may
  be specified.

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
  [`kde`](https://rdrr.io/pkg/SpatialKDE/man/kde.html) or ignored if
  `kde = FALSE`.

## Value

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) tibble of
regular grid cells with corresponding point counts, *G*_(*i*)^(\*) or
*G*_(*i*)^(\*) values and (optionally) kernel density estimates for each
cell. Values greater than zero indicate more points than would be
expected for randomly distributed points and values less than zero
indicate fewer points. Critical values of *G*_(*i*)^(\*) and
*G*_(*i*)^(\*) are given in the manual page for
[`localG`](https://r-spatial.github.io/spdep/reference/localG.html).

The output from this function can be plotted in the same way as for
other SF objects, for which see
[`vignette("sf5", package = "sf")`](https://r-spatial.github.io/sf/articles/sf5.html).

## Details

This function calculates the Getis-Ord *G*_(*i*)^(\*) (gi-star) or
*G*_(*i*)^(\*) \\Z\\-score statistic for identifying clusters of point
locations. The underlying implementation uses the
[`localG`](https://r-spatial.github.io/spdep/reference/localG.html)
function to calculate the \\Z\\ scores and then
[`p.adjustSP`](https://r-spatial.github.io/spdep/reference/p.adjustSP.html)
function to adjust the corresponding \\p\\-values for multiple
comparison. The function also returns counts of points in each cell and
(by default but optionally) kernel density estimates using the
[`kde`](https://rdrr.io/pkg/SpatialKDE/man/kde.html) function.

### Coverage of the output data

The grid produced by this function covers the convex hull of the input
data layer. This means the result may include *G*_(*i*)^(\*) or
*G*_(*i*)^(\*) values for cells that are outside the area for which data
were provided, which could be misleading. To handle this, consider
cropping the output layer to the area for which data are available. For
example, if you only have crime data for a particular district, crop the
output dataset to the district boundary using
[`st_intersection`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html).

### Automatic cell-size selection

If no cell size is given then the cell size will be set so that there
are 50 cells on the shorter side of the grid. If the `data` SF object is
projected in metres or feet, the number of cells will be adjusted
upwards so that the cell size is a multiple of 100.

## References

Getis, A. & Ord, J. K. (1992). The Analysis of Spatial Association by
Use of Distance Statistics. *Geographical Analysis*, 24(3), 189-206.
doi:[doi:10.1111/j.1538-4632.1992.tb00261.x](https://doi.org/10.1111/j.1538-4632.1992.tb00261.x)

## Examples

``` r
library(sf)

# Transform data to UTM zone 15N so that cell_size and bandwidth can be set
# in metres
memphis_robberies_utm <- st_transform(memphis_robberies_jan, 32615)

# Automatically set grid-cell size, bandwidth and neighbour distance
# \donttest{
hotspot_gistar(memphis_robberies_utm)
#> Cell size set to 500 metres automatically
#> Bandwidth set automatically based on rule of thumb.
#> ℹ Bandwidth = 8,877 metres.
#> Simple feature collection with 2715 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 761986.2 ymin: 3876436 xmax: 794486.2 ymax: 3905936
#> Projected CRS: WGS 84 / UTM zone 15N
#> # A tibble: 2,715 × 5
#>        n   kde gistar pvalue                                            geometry
#>    <dbl> <dbl>  <dbl>  <dbl>                                       <POLYGON [m]>
#>  1     0 11.1  -0.546  0.585 ((770486.2 3876436, 770486.2 3876936, 770986.2 387…
#>  2     0 11.2  -0.598  0.550 ((770986.2 3876436, 770986.2 3876936, 771486.2 387…
#>  3     0 11.2  -0.598  0.550 ((771486.2 3876436, 771486.2 3876936, 771986.2 387…
#>  4     0 11.1   0.716  0.474 ((771986.2 3876436, 771986.2 3876936, 772486.2 387…
#>  5     1 10.9   0.716  0.474 ((772486.2 3876436, 772486.2 3876936, 772986.2 387…
#>  6     0 10.5   0.716  0.474 ((772986.2 3876436, 772986.2 3876936, 773486.2 387…
#>  7     0 10.0  -0.598  0.550 ((773486.2 3876436, 773486.2 3876936, 773986.2 387…
#>  8     0  9.45 -0.598  0.550 ((773986.2 3876436, 773986.2 3876936, 774486.2 387…
#>  9     0  8.76 -0.546  0.585 ((774486.2 3876436, 774486.2 3876936, 774986.2 387…
#> 10     0 10.1   0.893  0.372 ((768486.2 3876936, 768486.2 3877436, 768986.2 387…
#> # ℹ 2,705 more rows
# }

# Manually set grid-cell size in metres, since the `memphis_robberies`
# dataset uses a co-ordinate reference system (UTM zone 15 north) that is
# specified in metres
# \donttest{
hotspot_gistar(memphis_robberies_utm, cell_size = 200)
#> Bandwidth set automatically based on rule of thumb.
#> ℹ Bandwidth = 8,877 metres.
#> Simple feature collection with 16133 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 762136.2 ymin: 3876586 xmax: 794136.2 ymax: 3905386
#> Projected CRS: WGS 84 / UTM zone 15N
#> # A tibble: 16,133 × 5
#>        n   kde gistar  pvalue                                           geometry
#>    <dbl> <dbl>  <dbl>   <dbl>                                      <POLYGON [m]>
#>  1     0  11.2 -0.243 0.808   ((771936.2 3876586, 771936.2 3876786, 772136.2 38…
#>  2     0  11.1 -0.266 0.790   ((772136.2 3876586, 772136.2 3876786, 772336.2 38…
#>  3     0  11.1 -0.266 0.790   ((772336.2 3876586, 772336.2 3876786, 772536.2 38…
#>  4     0  11.0  3.20  0.00135 ((772536.2 3876586, 772536.2 3876786, 772736.2 38…
#>  5     1  10.8  3.20  0.00135 ((772736.2 3876586, 772736.2 3876786, 772936.2 38…
#>  6     0  10.7  3.20  0.00135 ((772936.2 3876586, 772936.2 3876786, 773136.2 38…
#>  7     0  10.5 -0.266 0.790   ((773136.2 3876586, 773136.2 3876786, 773336.2 38…
#>  8     0  10.3 -0.266 0.790   ((773336.2 3876586, 773336.2 3876786, 773536.2 38…
#>  9     0  10.1 -0.243 0.808   ((773536.2 3876586, 773536.2 3876786, 773736.2 38…
#> 10     0  11.6 -0.243 0.808   ((771136.2 3876786, 771136.2 3876986, 771336.2 38…
#> # ℹ 16,123 more rows
# }

# Automatically set grid-cell size and bandwidth for lon/lat data. The data
# will be transformed automatically before KDE values are calculated and the
# result will then be transformed back to the original CRS.
# \donttest{
hotspot_gistar(memphis_robberies)
#> Cell size set to 0.00524 degrees automatically
#> Data transformed to "WGS 84 / UTM zone 16N" co-ordinate system.
#> ℹ CRS code: "EPSG:32616".
#> ℹ Unit of measurement: metre.
#> Bandwidth set automatically based on rule of thumb.
#> ℹ Bandwidth = 5,588 metres.
#> Simple feature collection with 2926 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -90.1261 ymin: 34.99475 xmax: -89.72786 ymax: 35.26199
#> Geodetic CRS:  WGS 84
#> # A tibble: 2,926 × 5
#>        n   kde gistar pvalue                                            geometry
#>    <dbl> <dbl>  <dbl>  <dbl>                                       <POLYGON [°]>
#>  1     0  15.7 -0.992  0.321 ((-90.08418 34.99475, -90.07894 34.99475, -90.0789…
#>  2     0  16.9 -0.850  0.395 ((-90.07894 34.99475, -90.0737 34.99475, -90.0737 …
#>  3     0  18.4 -0.733  0.463 ((-90.0737 34.99475, -90.06846 34.99475, -90.06846…
#>  4     0  20.1 -0.887  0.375 ((-90.06846 34.99475, -90.06322 34.99475, -90.0632…
#>  5     0  22.5 -0.887  0.375 ((-90.06322 34.99475, -90.05798 34.99475, -90.0579…
#>  6     0  26.3 -0.475  0.635 ((-90.05798 34.99475, -90.05274 34.99475, -90.0527…
#>  7     0  31.2 -0.614  0.539 ((-90.05274 34.99475, -90.0475 34.99475, -90.0475 …
#>  8     0  36.6 -0.733  0.463 ((-90.0475 34.99475, -90.04226 34.99475, -90.04226…
#>  9     0  42.1 -0.992  0.321 ((-90.04226 34.99475, -90.03702 34.99475, -90.0370…
#> 10     0  47.1 -0.992  0.321 ((-90.03702 34.99475, -90.03178 34.99475, -90.0317…
#> # ℹ 2,916 more rows
# }
```
