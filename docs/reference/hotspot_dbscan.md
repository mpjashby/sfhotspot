# Identify hotspots using DBSCAN

Identify clusters of points using density-based spatial clustering and
represent each cluster as a buffered convex or concave hull. Clusters
are ranked by the number of points they contain.

## Usage

``` r
hotspot_dbscan(
  data,
  eps = NULL,
  min_pts = 5,
  density_adjust = 2,
  hull = c("concave", "convex"),
  hull_ratio = 0.75,
  transform = TRUE,
  quiet = FALSE,
  ...
)
```

## Arguments

- data:

  An [sf::sf](https://r-spatial.github.io/sf/reference/sf.html) object
  containing point geometries.

- eps:

  A single positive number specifying the DBSCAN neighbourhood radius in
  the units of the analysis co-ordinate reference system (CRS). If
  `NULL`, the radius is calculated automatically from the mean point
  density within the convex hull of `data`.

- min_pts:

  A single integer specifying the minimum number of points in an `eps`
  neighbourhood, including the point itself, for a point to be a core
  point. The default is `5`.

- density_adjust:

  A single positive number controlling automatic selection of `eps`.
  Ignored unless `eps = NULL`. A value of `1` (the background-density
  reference) identifies neighbourhoods with approximately at least the
  mean density within the convex hull of `data`; the default of `2`
  corresponds to approximately twice that density.

- hull:

  The type of hull used to represent each cluster: `"convex"` or
  `"concave"` (the default).

- hull_ratio:

  For concave hulls, a number from zero to one specifying the fraction
  convex. Zero produces a maximally concave hull and one produces a
  convex hull. The default is `0.75`. Ignored when `hull = "convex"`.

- transform:

  DBSCAN uses Euclidean distances and therefore requires a projected
  CRS. If `TRUE` (the default), geographic input is transformed
  automatically with
  [`st_transform_auto()`](https://pkgs.lesscrime.info/sfhotspot/reference/st_transform_auto.md)
  before analysis and the result is transformed back afterwards. If
  `FALSE`, geographic input produces an error.

- quiet:

  If `TRUE`, suppress informative messages about automatically selected
  parameters and geometry preparation.

- ...:

  Further arguments passed to
  [`dbscan::dbscan()`](https://rdrr.io/pkg/dbscan/man/dbscan.html), such
  as `borderPoints` and nearest-neighbour search controls. The arguments
  `x`, `eps`, `minPts`, and `weights` cannot be supplied through `...`.

## Value

An `sf` tibble with class `hspt_s` and one row per non-noise cluster. It
contains `cluster`, the original DBSCAN cluster identifier; `rank`, the
priority rank; `n`, the number of input point coordinates intersecting
the polygon; `prop`, `n` as a proportion of all input point coordinates;
and `geometry`. Since cluster polygons may overlap, the sum of `prop`
can exceed one. Ranking is by decreasing `n`, then increasing polygon
area, then increasing cluster identifier.

## Details

When `eps = NULL`, the neighbourhood radius is calculated as

`sqrt(((min_pts - 1) * A) / (pi * n * density_adjust))`,

where `A` is the area of the convex hull of the input point coordinates
and `n` is the number of coordinates. This means that the expected
number of points in an `eps` neighbourhood is approximately
`density_adjust` times the mean density of points within the convex
hull. A value of `density_adjust = 1` identifies clusters with at least
the mean density; the default of `2` requires approximately twice the
mean density, and still larger values identify clusters with higher
density.

`MULTIPOINT` geometries are cast to individual points before analysis.
The output `n` column counts all input point coordinates intersecting
each final cluster polygon, not only points assigned to that DBSCAN
cluster. DBSCAN includes border points in clusters by default; supplying
`borderPoints = FALSE` through `...` instead performs DBSCAN\* and
excludes border points from the points used to construct the hull.

Each cluster geometry is the selected hull of all assigned points,
buffered by `eps` and clipped to the convex hull of all input points.
Different cluster polygons may overlap after hull construction and
buffering.

## Examples

``` r
# \donttest{
hotspot_dbscan(memphis_robberies_jan)
#> Data transformed to "WGS 84 / UTM zone 16N" co-ordinate system.
#> ℹ CRS code: "EPSG:32616".
#> ℹ Unit of measurement: metre.
#> Neighbourhood distance set automatically from mean point density.
#> ℹ `eps` = 1,389 metres.
#> Simple feature collection with 9 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -90.06722 ymin: 35.00849 xmax: -89.82979 ymax: 35.23714
#> Geodetic CRS:  WGS 84
#> # A tibble: 9 × 5
#>   cluster  rank     n   prop                                            geometry
#> *   <int> <int> <int>  <dbl>                                       <POLYGON [°]>
#> 1       1     1    54 0.262  ((-89.96788 35.11665, -89.9677 35.11727, -89.96748…
#> 2       2     2    28 0.136  ((-89.96698 35.16729, -89.96683 35.16787, -89.9666…
#> 3       5     3    15 0.0728 ((-90.02832 35.15135, -90.02777 35.15194, -90.0267…
#> 4       9     4    12 0.0583 ((-90.0096 35.08838, -90.00994 35.08898, -90.01024…
#> 5       4     5     8 0.0388 ((-90.03014 35.15259, -90.03093 35.15252, -90.0317…
#> 6       7     6     8 0.0388 ((-90.01617 35.18858, -90.01696 35.18852, -90.0177…
#> 7       3     7     8 0.0388 ((-90.00994 35.01624, -90.01026 35.01564, -90.0106…
#> 8       6     8     7 0.0340 ((-90.05482 35.13471, -90.0556 35.13485, -90.05637…
#> 9       8     9     7 0.0340 ((-90.02285 35.09651, -90.02369 35.09657, -90.0245…

hotspot_dbscan(
  memphis_robberies_jan,
  density_adjust = 3,
  hull = "convex"
)
#> Data transformed to "WGS 84 / UTM zone 16N" co-ordinate system.
#> ℹ CRS code: "EPSG:32616".
#> ℹ Unit of measurement: metre.
#> Neighbourhood distance set automatically from mean point density.
#> ℹ `eps` = 1,134 metres.
#> Simple feature collection with 12 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -90.06443 ymin: 35.01279 xmax: -89.84658 ymax: 35.23518
#> Geodetic CRS:  WGS 84
#> # A tibble: 12 × 5
#>    cluster  rank     n   prop                                           geometry
#>  *   <int> <int> <int>  <dbl>                                      <POLYGON [°]>
#>  1       1     1    26 0.126  ((-89.90983 35.04011, -89.91028 35.03973, -89.910…
#>  2       2     2    25 0.121  ((-89.94609 35.14062, -89.9467 35.14042, -89.9473…
#>  3       4     3    15 0.0728 ((-89.9748 35.13597, -89.97469 35.13544, -89.9746…
#>  4      10     4     9 0.0437 ((-89.88446 35.04646, -89.88489 35.04606, -89.885…
#>  5      11     5     9 0.0437 ((-89.98971 35.06596, -89.99037 35.06587, -89.991…
#>  6       7     6     8 0.0388 ((-89.99285 35.16218, -89.99273 35.16165, -89.992…
#>  7      12     7     7 0.0340 ((-89.84942 35.0513, -89.84967 35.05081, -89.8499…
#>  8       9     8     7 0.0340 ((-89.92761 35.10885, -89.92758 35.10831, -89.927…
#>  9       5     9     6 0.0291 ((-90.02838 35.15411, -90.02781 35.15386, -90.027…
#> 10       6    10     6 0.0291 ((-89.99173 35.20128, -89.99186 35.20076, -89.992…
#> 11       3    11     6 0.0291 ((-90.01217 35.01801, -90.0125 35.01755, -90.0128…
#> 12       8    12     6 0.0291 ((-90.00598 35.11793, -90.00541 35.11766, -90.004…
# }
```
