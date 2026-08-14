# Changelog

## sfhotspot 1.1.0

- [`hotspot_clip()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_clip.md)
  can now clip polygon results produced by the `hotspot_*()` functions,
  as well as point data
  ([\#65](https://github.com/mpjashby/sfhotspot/issues/65)).
- [`hotspot_clip()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_clip.md)
  now preserves the specialised `hspt_*` class of results produced by
  other package functions
  ([\#71](https://github.com/mpjashby/sfhotspot/issues/71)).
- [`hotspot_clip()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_clip.md)
  no longer reports that zero rows were removed when all input features
  fall within the clipping boundary
  ([\#66](https://github.com/mpjashby/sfhotspot/issues/66)).
- [`hotspot_gistar()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_gistar.md)
  now calculates KDE values for longitude/latitude data by automatically
  transforming them to a projected co-ordinate reference system and
  transforming the results back afterwards
  ([\#68](https://github.com/mpjashby/sfhotspot/issues/68)).
- [`hotspot_dual_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dual_kde.md)
  now calculates valid KDE values for longitude/latitude data while
  using a common automatically selected bandwidth for both layers, and
  checks that both point layers overlap the analysis grid
  ([\#67](https://github.com/mpjashby/sfhotspot/issues/67)).

## sfhotspot 1.0.0

CRAN release: 2025-07-29

- New function
  [`hotspot_clip()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_clip.md)
  added to extract points from an SF object inside the boundary of a
  polygon ([\#57](https://github.com/mpjashby/sfhotspot/issues/57)).
- By default lat/lon data is now transformed to use a projected
  co-ordinate reference system before kernel density estimation and then
  transformed back afterwards (previously trying to run KDE on lat/lon
  data resulted in an error). Datasets can also be automatically
  transformed to/from lat/lon using
  [`st_transform_auto()`](https://pkgs.lesscrime.info/sfhotspot/reference/st_transform_auto.md)
  ([\#48](https://github.com/mpjashby/sfhotspot/issues/48)).
- Functions now return useful error if provided with an empty dataset
  ([\#58](https://github.com/mpjashby/sfhotspot/issues/58)).
- Adjusted bandwidth now reported accurately
  ([\#56](https://github.com/mpjashby/sfhotspot/issues/56)).
- Removed dependency on the rmarkdown package in favour of using the
  quarto package to generate vignettes.

## sfhotspot 0.9.2

- [`hotspot_grid()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_grid.md)
  handles certain invalid polygon geometries
  ([\#54](https://github.com/mpjashby/sfhotspot/issues/54)).

## sfhotspot 0.9.1

CRAN release: 2025-02-19

- [`hotspot_grid()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_grid.md)
  handles non-multipolygon input geometries
  ([\#46](https://github.com/mpjashby/sfhotspot/issues/46)).
- [`hotspot_dual_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dual_kde.md),
  [`hotspot_gistar()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_gistar.md)
  and
  [`hotspot_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_kde.md)
  now warn if KDE bandwidth is smaller than cell size
  ([\#29](https://github.com/mpjashby/sfhotspot/issues/29)).
- `count_points_in_polygons()` (which is used internally to count points
  in all the `hotspot_*()` family of functions) now respects
  `quiet = TRUE`
  ([\#52](https://github.com/mpjashby/sfhotspot/issues/52)).
- Conditions now formatted with the cli package and following the
  Tidyverse Style Guide for errors
  ([\#47](https://github.com/mpjashby/sfhotspot/issues/47)).

## sfhotspot 0.9.0

CRAN release: 2025-02-10

- `count_points_in_polygons()` now passes through columns in the
  original dataset, which makes
  [`hotspot_count()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_count.md)
  more useful ([\#41](https://github.com/mpjashby/sfhotspot/issues/41)).
- [`hotspot_grid()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_grid.md)
  if provided with polygons now bases the grid on the boundary of the
  polygons rather than the convex hull of the boundary
  ([\#42](https://github.com/mpjashby/sfhotspot/issues/42)).
- New dataset `memphis_precincts` showing Memphis Police Department
  precincts, which is required to test the new functionality of
  [`hotspot_grid()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_grid.md).
- [`hotspot_gistar()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_gistar.md)
  now extracts nearest neighbour distance from provided grid and does
  not wrongly rely on (and report) an automatically generated cell size
  ([\#38](https://github.com/mpjashby/sfhotspot/issues/38)).
- Warnings about grids containing very large numbers of cells is now
  printed before the cells are created, helping explain why code may be
  running slower than expected
  ([\#33](https://github.com/mpjashby/sfhotspot/issues/33)).
- Improved error message produced when point data and provided grid do
  not overlap ([\#39](https://github.com/mpjashby/sfhotspot/issues/39)).
- Suppressed progress bar previously included in README
  ([\#36](https://github.com/mpjashby/sfhotspot/issues/36)).

## sfhotspot 0.8.0

CRAN release: 2023-09-19

- All functions can now handle SF objects in which the geometry column
  has a name other than `geometry`
  ([\#30](https://github.com/mpjashby/sfhotspot/issues/30)).
- [`hotspot_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_kde.md)
  progress bar is now suppressed by `quiet = TRUE`
  ([\#25](https://github.com/mpjashby/sfhotspot/issues/25)).
- When KDE bandwidth is selected automatically based on a rule of thumb,
  the reported bandwidth now takes into account the value of
  `bandwidth_adjust`
  ([\#32](https://github.com/mpjashby/sfhotspot/issues/32)).

## sfhotspot 0.7.1

CRAN release: 2023-02-10

- Added warning if automatically created grids contain more than 100,000
  cells, since this can cause other functions in the package to run very
  slowly ([\#24](https://github.com/mpjashby/sfhotspot/issues/24)).
- Fixed bug in which cell size could be incorrectly rounded to zero
  ([\#26](https://github.com/mpjashby/sfhotspot/issues/26)).
- Removed `covr` dependency.

## sfhotspot 0.7.0

CRAN release: 2022-09-11

- New function
  [`hotspot_change()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_change.md)
  and corresponding methods for
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  and
  [`autolayer()`](https://ggplot2.tidyverse.org/reference/autolayer.html)
  for measuring change in the frequency of events between two time
  periods ([\#14](https://github.com/mpjashby/sfhotspot/issues/14)).

## sfhotspot 0.6.1

- Fixed bug in which the message produced when setting bandwidth
  automatically referred to the unadjusted rather than adjusted
  bandwidth ([\#22](https://github.com/mpjashby/sfhotspot/issues/22)).

## sfhotspot 0.6.0

- New function
  [`hotspot_dual_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dual_kde.md)
  for estimating different relations between the density of two point
  layers ([\#1](https://github.com/mpjashby/sfhotspot/issues/1)).
- New dataset `memphis_population` showing the 2020 population of the
  centroids of census blocks in Memphis, TN.

## sfhotspot 0.5.0

- Users can now specify weights for counts of points and kernel density
  estimation via the `weights` argument to
  [`hotspot_count()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_count.md),
  [`hotspot_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_kde.md)
  and
  [`hotspot_gistar()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_gistar.md).

## sfhotspot 0.4.0

- Users can now provide their own grid using the `grid` argument to the
  `hotspot_*()` family of functions.
- New function
  [`hotspot_grid()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_grid.md)
  added so users can create a rectangular or hexagonal grid separately
  from counting points, calculating KDE values, etc. This may be useful
  to use the same grid for different datasets covering a similar area.

## sfhotspot 0.3.1

- Fixed bug where `...` arguments were not passed on to
  [`SpatialKDE::kde()`](https://rdrr.io/pkg/SpatialKDE/man/kde.html) as
  specified in the documentation.

## sfhotspot 0.3.0

- Added
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  methods for plotting the results produced by
  [`hotspot_count()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_count.md),
  [`hotspot_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_kde.md)
  and
  [`hotspot_classify()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify.md).
- Added `bandwidth_adjust` argument to
  [`hotspot_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_kde.md)
  and
  [`hotspot_gistar()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_gistar.md)
  so that bandwidth can be set relative to the default.

## sfhotspot 0.2.2

- Added checking for empty geometries and co-ordinates on Null Island
  (or equivalent zero co-ordinates in other co-ordinate systems). Empty
  geometries produce an error and zero co-ordinates produce a warning.

## sfhotspot 0.2.1

- Fixed a bug where a grid of cells could not be created for an SF
  object with no CRS defined (and improved the error message when trying
  to calculate KDE values in this circumstance).
- Fixed a bug where the grid returned by the `hotspot_*()` family of
  functions was not clipped to the convex hull of the data, as specified
  in the documentation.

## sfhotspot 0.2.0

CRAN release: 2022-02-14

- Added
  [`hotspot_classify()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify.md)
  and
  [`hotspot_classify_params()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify_params.md)
  functions.
- Added references to function documentation pages and the DESCRIPTION
  file.
- Changed `\dontrun{}` in some of the documentation examples to
  `\donttest{}`.

## sfhotspot 0.1.1

- Fixed a problem with some of the examples in the documentation taking
  too long to run, which caused them to fail CRAN checks.

## sfhotspot 0.1.0

- Initial release of the package with the functions
  [`hotspot_count()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_count.md),
  [`hotspot_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_kde.md)
  and
  [`hotspot_gistar()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_gistar.md).
