# Identify hotspots using DBSCAN

Identify clusters of points using density-based spatial clustering and
represent each cluster as a buffered convex or concave hull. Clusters
are ranked by the number of points they contain.

## Usage

``` r
hotspot_dbscan(
  data,
  eps = NULL,
  min_pts = NULL,
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
  `NULL`, the radius is calculated automatically from nearest-neighbour
  distances. See **Automatic parameter selection**.

- min_pts:

  A single integer specifying the minimum number of points in an `eps`
  neighbourhood, including the point itself, for a point to be a core
  point. If `NULL`, the value is calculated automatically from the
  number of point coordinates in `data`. See **Automatic parameter
  selection**.

- density_adjust:

  A single positive number controlling automatic selection of `eps`.
  Ignored unless `eps = NULL`. A value of `1` (the reference
  neighbourhood density) uses the median nearest-neighbour distance; the
  default of `2` requires approximately twice that density.

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

## Automatic parameter selection

When `min_pts = NULL`, it is calculated as

`min(n, 50, max(5, ceiling(sqrt(n))))`,

where `n` is the number of point coordinates after empty geometries have
been removed and `MULTIPOINT` geometries have been expanded. For
datasets with two to four coordinates, all coordinates are required. At
least two coordinates are needed for automatic selection. This rule
increases the evidence required to identify a hotspot in larger
datasets, while the upper limit prevents the required number of
neighbours becoming excessively large.

When `eps = NULL`, the function calculates the distance from every point
to its `(min_pts - 1)`th nearest other point. The median of those
distances is a typical local neighbourhood radius. The value used for
clustering is

`eps = median_neighbour_distance / sqrt(density_adjust)`.

`min_pts - 1` other points are used because DBSCAN counts the focal
point itself. Since circular area is proportional to the square of its
radius, the default `density_adjust = 2` searches for the required
number of points in approximately half the typical neighbourhood area,
corresponding to approximately twice the typical local point density.
Larger values identify denser concentrations; values below one allow
less-dense concentrations.

Automatic values provide a starting point for exploratory analysis.
DBSCAN results can be sensitive to both parameters, so users should
consider whether the resulting neighbourhood size and minimum density
are meaningful for their application.

## Examples

``` r
# \donttest{
hotspot_dbscan(memphis_robberies_jan)
#> Minimum points set automatically from the number of point coordinates.
#> ℹ `min_pts` = 15.
#> Data transformed to "WGS 84 / UTM zone 16N" co-ordinate system.
#> ℹ CRS code: "EPSG:32616".
#> ℹ Unit of measurement: metre.
#> Neighbourhood distance set automatically from nearest-neighbour distances.
#> ℹ `eps` = 2,077 metres; `density_adjust` = 2.
#> Simple feature collection with 3 features and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -90.06077 ymin: 35.02829 xmax: -89.88925 ymax: 35.2057
#> Geodetic CRS:  WGS 84
#> # A tibble: 3 × 5
#>   cluster  rank     n  prop                                             geometry
#> *   <int> <int> <int> <dbl>                                        <POLYGON [°]>
#> 1       2     1    40 0.194 ((-90.05603 35.14974, -90.05569 35.15071, -90.05529…
#> 2       1     2    31 0.150 ((-89.95894 35.05351, -89.95971 35.05425, -89.96043…
#> 3       3     3    30 0.146 ((-89.96698 35.18006, -89.96726 35.181, -89.96749 3…

hotspot_dbscan(
  memphis_robberies_jan,
  density_adjust = 3,
  hull = "convex"
)
#> Minimum points set automatically from the number of point coordinates.
#> ℹ `min_pts` = 15.
#> Data transformed to "WGS 84 / UTM zone 16N" co-ordinate system.
#> ℹ CRS code: "EPSG:32616".
#> ℹ Unit of measurement: metre.
#> Neighbourhood distance set automatically from nearest-neighbour distances.
#> ℹ `eps` = 1,696 metres; `density_adjust` = 3.
#> Simple feature collection with 1 feature and 4 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -89.9706 ymin: 35.14672 xmax: -89.8954 ymax: 35.20228
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 5
#>   cluster  rank     n  prop                                             geometry
#> *   <int> <int> <int> <dbl>                                        <POLYGON [°]>
#> 1       1     1    26 0.126 ((-89.92806 35.14692, -89.92904 35.14681, -89.93002…
# }
```
