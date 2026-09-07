# Package index

## Analysing hotspots

Tools for analysing hotspots.

- [`hotspot_count()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_count.md)
  : Count points in cells in a two-dimensional grid
- [`hotspot_change()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_change.md)
  : Identify change in hotspots over time
- [`hotspot_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_kde.md)
  : Estimate two-dimensional kernel density of points
- [`hotspot_dual_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dual_kde.md)
  : Estimate the relationship between the kernel density of two layers
  of points
- [`hotspot_gistar()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_gistar.md)
  : Identify significant spatial clusters of points
- [`hotspot_dbscan()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dbscan.md)
  : Identify hotspots using DBSCAN
- [`hotspot_classify()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify.md)
  : Classify hot-spots
- [`hotspot_classify_params()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify_params.md)
  : Control the parameters used to classify hotspots

## Data wrangling

Tools for working with data that is used to identify hotspots. Most data
wrangling is done automatically by the analysis functions listed above,
but you can use the functions below to control in more detail how this
is done.

- [`hotspot_clip()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_clip.md)
  : Extract spatial features inside a polygon
- [`hotspot_grid()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_grid.md)
  : Create either a rectangular or hexagonal two-dimensional grid
- [`st_transform_auto()`](https://pkgs.lesscrime.info/sfhotspot/reference/st_transform_auto.md)
  : Toggle between lon/lat and UTM co-ordinates

## Plotting results

These methods for the
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
function automatically produce charts that are tailored to displaying
the results produced by one of the `hotspot_*()` family of functions.
[`hotspot_isoband()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_isoband.md)
generalises the polygons produced by the hotspot-analysis functions into
polygons, which can sometimes be preferable for plotting.

- [`hotspot_isoband()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_isoband.md)
  : Convert a hotspot grid to isobands
- [`autoplot(`*`<hspt_c>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_c.md)
  [`autolayer(`*`<hspt_c>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_c.md)
  : Plot map of hotspot classifications
- [`autoplot(`*`<hspt_d>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_d.md)
  [`autolayer(`*`<hspt_d>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_d.md)
  : Plot map of changes in grid counts
- [`autoplot(`*`<hspt_dk>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_dk.md)
  [`autolayer(`*`<hspt_dk>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_dk.md)
  : Plot map of dual kernel-density values
- [`autoplot(`*`<hspt_g>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_g.md)
  [`autolayer(`*`<hspt_g>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_g.md)
  : Plot map of Getis-Ord Gi\* results
- [`autoplot(`*`<hspt_ib>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_ib.md)
  [`autolayer(`*`<hspt_ib>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_ib.md)
  : Plot isobands
- [`autoplot(`*`<hspt_k>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_k.md)
  [`autolayer(`*`<hspt_k>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_k.md)
  : Plot map of kernel-density values
- [`autoplot(`*`<hspt_n>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_n.md)
  [`autolayer(`*`<hspt_n>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_n.md)
  : Plot map of grid counts
- [`autoplot(`*`<hspt_s>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_s.md)
  [`autolayer(`*`<hspt_s>`*`)`](https://pkgs.lesscrime.info/sfhotspot/reference/autoplot.hspt_s.md)
  : Plot DBSCAN hotspot clusters

## Sample data

Sample data that can be used with the functions in this package.

- [`memphis_population`](https://pkgs.lesscrime.info/sfhotspot/reference/memphis_population.md)
  : Populations of census blocks in Memphis in 2020
- [`memphis_precincts`](https://pkgs.lesscrime.info/sfhotspot/reference/memphis_precincts.md)
  : Memphis Police Department Precincts
- [`memphis_robberies`](https://pkgs.lesscrime.info/sfhotspot/reference/memphis_robberies.md)
  : Personal robberies in Memphis in 2019
- [`memphis_robberies_jan`](https://pkgs.lesscrime.info/sfhotspot/reference/memphis_robberies_jan.md)
  : Personal robberies in Memphis in January 2019
