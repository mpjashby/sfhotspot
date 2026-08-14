# Create either a rectangular or hexagonal two-dimensional grid

Create either a rectangular or hexagonal two-dimensional grid

## Usage

``` r
hotspot_grid(data, cell_size = NULL, grid_type = "rect", quiet = FALSE, ...)
```

## Arguments

- data:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data frame.

- cell_size:

  `numeric` value specifying the size of each equally spaced grid cell,
  using the same units (metres, degrees, etc.) as used in the `sf` data
  frame given in the `data` argument. If this argument is `NULL` (the
  default), the cell size will be calculated automatically (see
  Details).

- grid_type:

  `character` specifying whether the grid should be made up of squares
  (`"rect"`, the default) or hexagons (`"hex"`).

- quiet:

  if set to `TRUE`, messages reporting the values of any parameters set
  automatically will be suppressed. The default is `FALSE`.

- ...:

  Further arguments passed to `link[sf]{st_make_grid}`.

## Value

A simple features tibble containing polygons representing grid cells.

The grid will be based on the convex hull of `data`, expanded by a
buffer of `cell_size / 2` to ensure all the points in `data` fall within
the resulting grid.
