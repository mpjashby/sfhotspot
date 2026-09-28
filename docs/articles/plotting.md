# Plotting results returned by functions in sfhotspot

The `hotspot_*()` functions return simple-feature (SF) objects with
specialised classes.
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
recognises those classes and chooses a variable, colour scale, and
legend suited to the result. Some choices also depend on the columns or
attributes present in the result.

| Result | Default variable shown on map |
|----|----|
| [`hotspot_count()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_count.md) (`hspt_n`) | `sum` when weighted counts are present; otherwise `n` |
| [`hotspot_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_kde.md) (`hspt_k`) | `kde` |
| [`hotspot_dual_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dual_kde.md) (`hspt_dk`) | Scale selected from the `method` attribute |
| [`hotspot_change()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_change.md) (`hspt_d`) | Diverging scale of `change`, centred on zero |
| [`hotspot_classify()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify.md) (`hspt_c`) | Categorical hotspot and coldspot colours |
| [`hotspot_gistar()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_gistar.md) (`hspt_g`) with `kde` | Density in statistically significant cells |
| [`hotspot_gistar()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_gistar.md) (`hspt_g`) without `kde` | Diverging scale of `gistar`, centred on zero |
| [`hotspot_dbscan()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dbscan.md) (`hspt_db`) | `n` |

The maps below show examples of the default maps produced for each
`hotspot_*()` function, based on the `memphis_robberies`,
`memphis_precincts` and `memphis_population` datasets included in the
package. To get started, we create a grid covering the city of Memphis
and transform the dataset to use an appropriate projected coordinate
reference system (CRS), since that makes specifying cell size, etc.,
easier.

``` r

library(sf)
```

    Linking to GEOS 3.13.0, GDAL 3.8.5, PROJ 9.5.1; sf_use_s2() is TRUE

``` r

library(sfhotspot)
library(ggplot2)

# Transform objects to use a local co-ordinate reference system
memphis_robberies_proj <- st_transform_auto(memphis_robberies)  
```

    Data transformed to "WGS 84 / UTM zone 16N" co-ordinate system.
    ℹ CRS code: "EPSG:32616".
    ℹ Unit of measurement: metre.

``` r

memphis_precincts_proj <- st_transform_auto(memphis_precincts, quiet = TRUE)
memphis_population_proj <- st_transform_auto(memphis_population, quiet = TRUE)

# Set up consistent grids of cells
memphis_grid <- hotspot_grid(memphis_precincts_proj, cell_size = 500)
```

> **Grid size**
>
> The 500-metre grid cells used in these examples may be too coarse for
> many analytical purposes, but are used here because of constraints on
> rendering time for R package vignettes. In practice, the cell size
> should be chosen to suit the spatial scale of the data and the
> research question.

In each case the
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
method loads a map with no base map by default. For each of the above
functions a corresponding
[`hotspot_map()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_map.md)
method exists that by-default also includes a base map with suitable
attribution, etc.
[`hotspot_map()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_map.md)
can also be used for mapping ordinary `sf` objects.

## `hotspot_count()`

For an unweighted point count,
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
maps the `n` column.

``` r

memphis_robberies_count <- hotspot_count(
  memphis_robberies_proj, 
  grid = memphis_grid
)

autoplot(memphis_robberies_count)
```

![Grid map of Memphis robberies. Darker blue cells contain more
robberies.](plotting_files/figure-html/plot-unweighted-count-1.png)

If
[`hotspot_count()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_count.md)
is given weights, its result also contains a `sum` column. The plotting
methods detect that column and map the weighted count instead.

[`hotspot_layer()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_layer.md)
applies the same variable mapping as
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
but returns a layer so that it can be combined with other ggplot2
layers. It is a student-friendly wrapper around
[`ggplot2::autolayer()`](https://ggplot2.tidyverse.org/reference/autolayer.html).
For example, a count layer can be drawn with precinct boundaries.

``` r

ggplot() +
  hotspot_layer(memphis_robberies_count) +
  geom_sf(data = memphis_precincts_proj, fill = NA, colour = "grey70") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  labs(fill = "Robbery\ncount") +
  theme_void()
```

![Map of Memphis robbery counts drawn with outlines of police precincts.
Areas of greater robbery counts are darker
blue.](plotting_files/figure-html/plot-count-hotspot-layer-1.png)

[`hotspot_layer()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_layer.md)
is particularly useful because it allows control of where the layer
derived from the result produced by a `hotspot_*()` function appears in
a stack of other spatial layers, so that (for example) it can be placed
on top of a base map.

## `hotspot_kde()`

When provided with an object produced by
[`hotspot_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_kde.md),
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
maps the `kde` column. The default colour scale is sequential, with
darker blue indicating greater density. Note that since
[`hotspot_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_kde.md)
produces density estimates for the convex hull of the data, the
resulting map may extend beyond the precinct boundaries.
[`hotspot_clip()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_clip.md)
can be used to restrict the KDE to the specific area covered by the
point dataset to avoid displaying spurious density estimates for areas
not covered by the data.

``` r

memphis_robberies_proj |> 
  hotspot_kde(bandwidth_adjust = 0.25, grid = memphis_grid) |> 
  hotspot_clip(memphis_precincts_proj) |> 
  autoplot()
```

    Bandwidth set automatically based on rule of thumb.
    ℹ Adjusted bandwidth = 1,397 metres (0.25 * 5,588 metres).
    Removed 179 rows (5.0% of original rows) from `data`

![Map of the density of robberies in Memphis. Darker blue areas have a
higher density of robberies.](plotting_files/figure-html/plot-kde-1.png)

## `hotspot_dual_kde()`

[`hotspot_dual_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dual_kde.md)
can compare the density of two spatial datasets in four ways. The method
is recorded in the `method` attribute of the result, which
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
and
[`hotspot_layer()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_layer.md)
use to select the appropriate colour scale. The following examples use
the same robbery and population data, bandwidth and grid so that the
effect of changing only `method` is visible.

The default, `method = "ratio"`, divides the density of the first
dataset by the density of the second. This is analogous to mapping a
rate. The map uses a sequential scale applied directly to the ratio
values, without treating one as a change point or transforming the
scale.

``` r

hotspot_dual_kde(
  memphis_robberies_proj, 
  memphis_population_proj, 
  method = "ratio",
  bandwidth_adjust = 0.25,
  grid = memphis_grid
) |>
  autoplot()
```

    Bandwidth set automatically based on rule of thumb.
    ℹ Adjusted bandwidth for `x` and `y` = 1,397 metres (0.25 * 5,588 metres).

![Map of the density of robberies relative to the population density in
Memphis. Darker blue areas have a higher density of robberies relative
to the
population.](plotting_files/figure-html/plot-dual-kde-ratio-1.png)

`method = "log"` calculates the natural logarithm of the density ratio.
Zero then represents equal densities, negative values indicate a lower
robbery density relative to population density, and positive values
indicate a higher relative robbery density. Although zero denotes
equality, it is not treated as a colour-scale boundary: a continuous
sequential scale represents progression from lower to higher logged
ratios.

``` r

hotspot_dual_kde(
  memphis_robberies_proj,
  memphis_population_proj,
  method = "log",
  bandwidth_adjust = 0.25,
  grid = memphis_grid
) |>
  autoplot()
```

    Bandwidth set automatically based on rule of thumb.
    ℹ Adjusted bandwidth for `x` and `y` = 1,397 metres (0.25 * 5,588 metres).

![Sequential map of the logged ratio of robbery density to population
density in Memphis. Lighter and darker areas have lower and higher
logged density
ratios.](plotting_files/figure-html/plot-dual-kde-log-1.png)

`method = "diff"` subtracts the density of the second dataset from that
of the first. This is the only method for which
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
uses a diverging scale: negative and positive differences are shown on
opposite sides of zero.

``` r

hotspot_dual_kde(
  memphis_robberies_proj,
  memphis_population_proj,
  method = "diff",
  bandwidth_adjust = 0.25,
  grid = memphis_grid
) |>
  autoplot()
```

    Bandwidth set automatically based on rule of thumb.
    ℹ Adjusted bandwidth for `x` and `y` = 1,397 metres (0.25 * 5,588 metres).

![Diverging map of robbery density minus population density in Memphis.
Negative and positive density differences appear on opposite sides of
the colour
scale.](plotting_files/figure-html/plot-dual-kde-difference-1.png)

Finally, `method = "sum"` adds the two density surfaces. Since sums are
non-negative, they are shown with a sequential scale: darker cells have
a higher combined density.

``` r

hotspot_dual_kde(
  memphis_robberies_proj,
  memphis_population_proj,
  method = "sum",
  bandwidth_adjust = 0.25,
  grid = memphis_grid
) |>
  autoplot()
```

    Bandwidth set automatically based on rule of thumb.
    ℹ Adjusted bandwidth for `x` and `y` = 1,397 metres (0.25 * 5,588 metres).

![Sequential map of the combined robbery and population density in
Memphis. Darker blue cells have a higher combined
density.](plotting_files/figure-html/plot-dual-kde-sum-1.png)

Each legend uses `low` at its lower end and `high` at its upper end.
These labels describe position within the values produced by the
selected method; they do not imply that the four methods produce
directly comparable numeric values.

## `hotspot_change()`

[`hotspot_change()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_change.md)
returns the differences between the number of points in each area in two
time periods. Its scale is symmetric around zero so increases and
decreases of the same size receive equal visual emphasis.

``` r

memphis_robberies_proj |>
  hotspot_change(
    boundary = as.POSIXct("2019-07-01", tz = "UTC"),
    grid = memphis_grid
  ) |>
  autoplot()
```

![Diverging grid map of changes in robbery counts between the first and
second halves of 2019. Decreases and increases use opposite
colours.](plotting_files/figure-html/plot-change-1.png)

## `hotspot_classify()`

[`hotspot_classify()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify.md)
categorises grid cells based on their hotspot and coldspot status over
time. The default palette uses warm hues for hotspot categories, cool
hues for coldspot categories, purple for mixed results, and grey for no
pattern.

``` r

memphis_robberies_proj |>
  hotspot_classify(period = "1 month", grid = memphis_grid) |>
  autoplot()
```

    Date range data is not a multiple of chosen period.
    ℹ Final period contains 0.6 month.
    ℹ Set `collapse = TRUE` to collapse that period into penultimate period.

![Categorical map classifying Memphis grid cells as persistent,
emerging, intermittent or former hotspots and coldspots, mixed areas, or
no pattern.](plotting_files/figure-html/plot-classification-1.png)

## `hotspot_gistar()`

By default,
[`hotspot_gistar()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_gistar.md)
returns density estimates and p-values for each cell alongside the
Getis-Ord Gi\* statistic. The plotting methods use the p-values to
select which cells are visible, then map the density of points in each
visible cell. The `critical_p` and `sign` arguments to
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)/[`hotspot_layer()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_layer.md)
can control this behaviour. Cells with `pvalue >= critical_p` are
transparent. The default is `critical_p = 0.05`. The `sign` argument can
be used to restrict the visible cells to those with significant positive
or negative Gi\*/Gi values. When both signs are shown, a diverging scale
distinguishes cold spots from hot spots; cold-spot densities are negated
only for plotting. When only hot spots or only cold spots are shown, a
medium-to-dark blue scale ensures that every significant cell remains
visible.

``` r

memphis_robberies_proj |>
  hotspot_gistar(grid = memphis_grid, bandwidth_adjust = 0.25) |>
  autoplot()
```

    Bandwidth set automatically based on rule of thumb.
    ℹ Adjusted bandwidth = 1,397 metres (0.25 * 5,588 metres).

![Density map showing only areas with significantly more or fewer
robberies than expected by chance. Other grid cells are
transparent.](plotting_files/figure-html/plot-gistar-both-1.png)

If `kde = FALSE`, the result has no `kde` column. The plotting methods
instead map the `gistar` column using a diverging scale centred on zero.
In this case, the `critical_p` and `sign` arguments to
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)/[`hotspot_layer()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_layer.md)
are ignored.

``` r

memphis_robberies_proj |>
  hotspot_gistar(grid = memphis_grid, bandwidth_adjust = 0.25, kde = FALSE) |>
  autoplot()
```

![Diverging grid map of Getis-Ord Gi-star statistics for Memphis
robberies, centred on zero. Positive and negative values use opposite
colours.](plotting_files/figure-html/plot-gistar-without-kde-1.png)

## `hotspot_dbscan()`

[`hotspot_dbscan()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dbscan.md)
returns polygons covering clusters of points identified by the DBSCAN
algorithm. By default, the `fill` aesthetic is mapped to the `n` column,
which counts the number of points in each cluster. This can be changed
using the `col_fill` argument to
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html) or
[`hotspot_layer()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_layer.md).
Polygons can also be labelled using the `col_label` argument to
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html) or
[`hotspot_layer()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_layer.md).
The default colour scale is sequential, with darker blue indicating
clusters with more points.

``` r

memphis_robberies_proj |>
  hotspot_dbscan(density_adjust = 3) |>
  autoplot()
```

    Minimum points set automatically from the number of point coordinates.
    ℹ `min_pts` = 48.
    Neighbourhood distance set automatically from nearest-neighbour distances.
    ℹ `eps` = 863.2 metres; `density_adjust` = 3.

![Map of clusters of robberies in Memphis identified by the DBSCAN
algorithm.](plotting_files/figure-html/plot-dbscan-1.png)

## Isobands

The numeric grid outputs from
[`hotspot_count()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_count.md),
[`hotspot_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_kde.md),
[`hotspot_dual_kde()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_dual_kde.md),
[`hotspot_change()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_change.md)
and
[`hotspot_gistar()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_gistar.md)
can be generalised into isobands using
[`hotspot_isoband()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_isoband.md).
The categorical output from
[`hotspot_classify()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_classify.md)
cannot be converted because isobands require numeric values on a regular
square grid.

Isobands retain information about the original `hotspot_*()` result and
can themselves be plotted using
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html) or
[`hotspot_layer()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_layer.md).
The default palette, legend and treatment of meaningful values such as
zero therefore depend on the function that produced the original grid.
For example, KDE isobands use a sequential scale:

``` r

memphis_robberies_proj |>
  hotspot_kde(bandwidth_adjust = 0.25, grid = memphis_grid) |>
  hotspot_isoband() |>
  autoplot()
```

    Bandwidth set automatically based on rule of thumb.
    ℹ Adjusted bandwidth = 1,397 metres (0.25 * 5,588 metres).

![Isoband map of robbery density in Memphis. Darker blue bands represent
areas with higher estimated robbery
density.](plotting_files/figure-html/plot-kde-isobands-1.png)

## Customising plots

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
returns an ordinary ggplot object, so scales, labels, themes, and other
layers can be added or replaced with ggplot2.
[`hotspot_layer()`](https://pkgs.lesscrime.info/sfhotspot/reference/hotspot_layer.md)
is useful when the sfhotspot result is one component of a larger map.
See the individual method help pages for the arguments accepted by each
method.

An optional map-tile background can provide geographic context. This
requires the suggested ggspatial package and an internet connection
unless the tiles are already cached. When a base map is requested, the
hotspot layer is drawn with an alpha of 0.75 so that the background
remains visible.

``` r

autoplot(
  memphis_robberies_count,
  basemap_type = "osm",
  basemap_zoom = 12
)
```

The default remains `basemap_type = "none"`, so ordinary plots never
download tiles. Known map types receive an attribution caption
automatically. For a custom or new map type, supply the provider’s
required statement explicitly using `basemap_attribution`. Use `caption`
to add contextual information such as the map author, date or notes.
This text appears above any explanatory caption and base-map
attribution, which are retained automatically.
