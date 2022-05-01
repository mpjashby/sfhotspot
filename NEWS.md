# sfhotspot 0.2.1

* Fixed a bug where a grid of cells could not be created for an SF object with 
  no CRS defined (and improved the error message when trying to calculate KDE 
  values in this circumstance).
* Fixed a bug where the grid returned by the `hotspot*()` family of functions 
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
