# sfhotspot 0.9.0

* New dataset `memphis_precincts` showing Memphis Police Department precincts,
  which is required to test the new functionality of `hotspot_grid()`.
* Fixed bug where columns present in polygons objects provided to 
  `count_points_in_polygons()` (e.g. by `hotspot_count()`) were removed, which
  made `hotspot_count()` less useful for counting points in polygons with names
  or other attributes stored in the original polygon object (#41).
* Fixed bug where `hotspot_grid()` created a grid based on the convex hull of
  the input geometry even if the input was a polygon rather than points (#42).
* Fixed bug where warning about grids with very large numbers of cells was
  printed only after the grid was (slowly) created (#33).
* Fixed bug where `hotspot_gistar()` erroneously reported an automatically 
  generated cell size even when a grid was supplied (#38).
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
