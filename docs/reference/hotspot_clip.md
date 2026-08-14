# Extract spatial features inside a polygon

Extract spatial features inside a polygon

## Usage

``` r
hotspot_clip(data, boundary, quiet = FALSE, ...)
```

## Arguments

- data:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data frame
  containing points or polygons.

- boundary:

  [`sf`](https://r-spatial.github.io/sf/reference/sf.html) data frame
  containing polygons.

- quiet:

  if set to `TRUE`, messages reporting the values of any parameters set
  automatically will be suppressed. The default is `FALSE`.

- ...:

  Further arguments passed to
  [`st_intersection`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html).

## Value

an SF data frame containing those spatial features that are covered by
the polygons.

## Details

This function is a wrapper around
[`st_intersection`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html)
that performs some additional checks and reports useful information. If
`data` has a specialised result class produced by this package, that
class is preserved in the clipped result.
