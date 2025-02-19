# sfhotspot 0.9.1

* `hotspot_grid()` handles non-multipolygon input geometries (#46).
* `hotspot_dual_kde()`, `hotspot_gistar()` and `hotspot_kde()` now warn if 
  KDE bandwidth is smaller than cell size (#29).
* `count_points_in_polygons()` (which is used internally to count points in
  all the `hotspot_*()` family of functions) now respects `quiet = TRUE` (#52).
* Conditions now formatted with the cli package and following the Tidyverse
  Style Guide for errors (#47).


# sfhotspot 0.9.0

* `count_points_in_polygons()` now passes through columns in the original 
  dataset, which makes `hotspot_count()` more useful (#41).
* `hotspot_grid()` if provided with polygons now bases the grid on the
  boundary of the polygons rather than the convex hull of the boundary (#42).
* New dataset `memphis_precincts` showing Memphis Police Department precincts,
  which is required to test the new functionality of `hotspot_grid()`.
* `hotspot_gistar()` now extracts nearest neighbour distance from provided grid
  and does not wrongly rely on (and report) an automatically generated cell
  size (#38).
* Warnings about grids containing very large numbers of cells is now printed
  before the cells are created, helping explain why code may be running slower
  than expected (#33).
* Improved error message produced when point data and provided grid do not
  overlap (#39).
* Suppressed progress bar previously included in README (#36).


# sfhotspot 0.8.0

* All functions can now handle SF objects in which the geometry column has a
  name other than `geometry` (#30).
* `hotspot_kde()` progress bar is now suppressed by `quiet = TRUE` (#25).
* When KDE bandwidth is selected automatically based on a rule of thumb, the
  reported bandwidth now takes into account the value of `bandwidth_adjust` 
  (#32).


# sfhotspot 0.7.1

* Added warning if automatically created grids contain more than 100,000 cells,
  since this can cause other functions in the package to run very slowly (#24).
* Fixed bug in which cell size could be incorrectly rounded to zero (#26).
* Removed `covr` dependency.


# sfhotspot 0.7.0

* New function `hotspot_change()` and corresponding methods for `autoplot()` and
  `autolayer()` for measuring change in the frequency of events between two time
  periods (#14).


# sfhotspot 0.6.1

* Fixed bug in which the message produced when setting bandwidth automatically
  referred to the unadjusted rather than adjusted bandwidth (#22).


# sfhotspot 0.6.0
* New function `hotspot_dual_kde()` for estimating different relations between
  the density of two point layers (#1).
* New dataset `memphis_population` showing the 2020 population of the centroids 
  of census blocks in Memphis, TN.


# sfhotspot 0.5.0

* Users can now specify weights for counts of points and kernel density 
  estimation via the `weights` argument to `hotspot_count()`, `hotspot_kde()` 
  and `hotspot_gistar()`.


# sfhotspot 0.4.0

* Users can now provide their own grid using the `grid` argument to the 
  `hotspot_*()` family of functions.
* New function `hotspot_grid()` added so users can create a rectangular or
  hexagonal grid separately from counting points, calculating KDE values, etc.
  This may be useful to use the same grid for different datasets covering a
  similar area.


# sfhotspot 0.3.1

* Fixed bug where `...` arguments were not passed on to `SpatialKDE::kde()` as
  specified in the documentation.


# sfhotspot 0.3.0

* Added `autoplot()` methods for plotting the results produced by 
  `hotspot_count()`, `hotspot_kde()` and `hotspot_classify()`.
* Added `bandwidth_adjust` argument to `hotspot_kde()` and `hotspot_gistar()` so
  that bandwidth can be set relative to the default.


# sfhotspot 0.2.2

* Added checking for empty geometries and co-ordinates on Null Island (or
  equivalent zero co-ordinates in other co-ordinate systems). Empty geometries
  produce an error and zero co-ordinates produce a warning.


# sfhotspot 0.2.1

* Fixed a bug where a grid of cells could not be created for an SF object with 
  no CRS defined (and improved the error message when trying to calculate KDE 
  values in this circumstance).
* Fixed a bug where the grid returned by the `hotspot_*()` family of functions 
  was not clipped to the convex hull of the data, as specified in the 
  documentation.


# sfhotspot 0.2.0

* Added `hotspot_classify()` and `hotspot_classify_params()` functions.
* Added references to function documentation pages and the DESCRIPTION file.
* Changed `\dontrun{}` in some of the documentation examples to `\donttest{}`.


# sfhotspot 0.1.1

* Fixed a problem with some of the examples in the documentation taking too long
  to run, which caused them to fail CRAN checks.


# sfhotspot 0.1.0

* Initial release of the package with the functions `hotspot_count()`, 
  `hotspot_kde()` and `hotspot_gistar()`.
