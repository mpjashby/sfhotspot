# Classify hot-spots

Classify cells in a grid based on changes in the clustering of points
(typically representing events) in a two-dimensional regular grid over
time.

## Usage

``` r
hotspot_classify(
  data,
  time = NULL,
  period = NULL,
  start = NULL,
  cell_size = NULL,
  grid_type = "rect",
  grid = NULL,
  collapse = FALSE,
  params = hotspot_classify_params(),
  quiet = FALSE
)
```

## Arguments

- data:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data frame
  containing points.

- time:

  Name of the column in `data` containing `Date` or `POSIXt` values
  representing the date associated with each point. If this argument is
  `NULL` and `data` contains a single column of `Date` or `POSIXt`
  values, that column will be used automatically.

- period:

  A character value containing a number followed by a unit of time, e.g.
  for example, "12 months" or "3.5 days", where the unit of time is one
  of second, minute, hour, day, week, month, quarter or year (or their
  plural forms).

- start:

  A `Date` or `POSIXt` value specifying when the first temporal period
  should start. If `NULL` (the default), the first period will start at
  the beginning of the earliest date found in the data (if `period` is
  specified in days, weeks, months, quarters or years) or at the
  earliest time found in the data otherwise.

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

- collapse:

  If the range of dates in the data is not a multiple of `period`, the
  final period will be shorter than the others. In that case, should
  this shorter period be collapsed into the penultimate period?

- params:

  A list of optional parameters that can affect the output. The list can
  be produced most easily using the
  [`hotspot_classify_params`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify_params.md)
  helper function.

- quiet:

  if set to `TRUE`, messages reporting the values of any parameters set
  automatically will be suppressed. The default is `FALSE`.

## Value

An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) tibble of
regular grid cells with corresponding hot-spot classifications for each
cell. This can be plotted using
[`autoplot`](https://ggplot2.tidyverse.org/reference/autoplot.html).

Hot-spots are spatial areas that contain more points than would be
expected by chance; cold-spots are areas that contain fewer points than
would be expected. Whether an area is a hot-spot can vary over time.
This function creates a space-time cube, determines whether an area is a
hot-spot for each of several consecutive time periods and uses that to
classify areas according to whether they are persistent, intermittent,
emerging or former hot- or cold-spots.

### Hot and cold spots

Hot- and cold-spots are identified by calculating the Getis-Ord
*G*_(*i*)^(\*) (gi-star) or *G*_(*i*)^(\*) \\Z\\-score statistic for
each cell in a regular grid for each time period. Cells are classified
as follows, using the parameters provided in the `params` argument:

- *Persistent hot-/cold-spots* are cells that have been hot-/cold-spots
  consistently over time. Formally: if the *p*-value is less than
  `critical_p` for at least `persistent_prop` proportion of time
  periods.

- *Emerging hot-/cold-spots* are cells that have become hot-/cold-spots
  recently but were not previously. Formally: if the *p*-value is less
  than `critical_p` for at least `hotspot_prop` of time periods defined
  as recent by `recent_prop` but the *p*-value was *not* less than
  `critical_p` for at least `hotspot_prop` of time periods defined as
  non-recent by `1 - recent_prop`.

- *Former hot-/cold-spots* are cells that used to be hot-/cold-spots but
  have not been more recently. Formally: if the *p*-value was less than
  `critical_p` for at least `hotspot_prop` of time periods defined as
  non-recent by `1 - recent_prop` but the *p*-value was *not* less than
  `critical_p` for for at least `hotspot_prop` of time periods defined
  as recent by `recent_prop`.

- *Intermittent hot-/cold-spots* are cells that have been
  hot-/cold-spots, but not as frequently as persistent hotspots and not
  only during recent/non-recent periods. Formally: if the *p*-value is
  less than `critical_p` for at least `hotspot_prop` of time periods but
  the cell is not an emerging or former hotspot.

- *No pattern* if none of the above categories apply.

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

Chainey, S. (2020). *Understanding Crime: Analyzing the Geography of
Crime*. Redlands, CA: ESRI.
