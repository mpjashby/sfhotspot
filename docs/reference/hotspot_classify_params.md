# Control the parameters used to classify hotspots

This function allows specification of parameters that affect the output
from
[`hotspot_classify`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify.md).

## Usage

``` r
hotspot_classify_params(
  hotspot_prop = 0.1,
  persistent_prop = 0.8,
  recent_prop = 0.2,
  critical_p = 0.05,
  nb_dist = NULL,
  include_self = TRUE,
  p_adjust_method = NULL
)
```

## Arguments

- hotspot_prop:

  A single numeric value specifying the minimum proportion of periods
  for which a cell must contain significant clusters of points before
  the cell can be classified as a hot or cold spot of any type.

- persistent_prop:

  A single numeric value specifying the minimum proportion of periods
  for which a cell must contain significant clusters of points before
  the cell can be classified as a persistent hot or cold spot.

- recent_prop:

  A single numeric value specifying the proportion of periods that
  should be treated as being recent in the classification of emerging
  and former hotspots.

- critical_p:

  A threshold *p*-value below which values should be treated as being
  statistically significant.

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

## Value

A list that can be used as the input to the `params` argument to
[`hotspot_classify`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify.md).
