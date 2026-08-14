# Toggle between lon/lat and UTM co-ordinates

It is often useful to convert geographic data to a projected co-ordinate
system, e.g. for specifying a buffer distance in metres.
`st_transform_auto` converts between lon/lat co-ordinates and the UTM or
UPS zone for the centroid of the data.

## Usage

``` r
st_transform_auto(data, check = TRUE, quiet = FALSE)
```

## Arguments

- data:

  An [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object.

- check:

  Should the function check if the chosen UTM zone covers all the data?
  If so then a warning is issued if the centroid of any row in the data
  is more than 1º outside the area covered by the UTM zone.

- quiet:

  if set to `TRUE`, messages reporting the values of any parameters set
  automatically will be suppressed. The default is `FALSE`.

## Value

SF object transformed to new CRS.
