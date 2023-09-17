data_sf <- suppressWarnings(
  sf::st_centroid(sf::read_sf(system.file("shape/nc.shp", package = "sf")))
)
data_sf$wt <- runif(nrow(data_sf), max = 1000)
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))

polygons <- sf::st_set_geometry(
  create_grid(data = data_sf, cell_size = 0.1),
  "random_geom_column_name"
)
result <- count_points_in_polygons(
  points = data_sf,
  polygons = polygons
)



# CHECK INPUTS -----------------------------------------------------------------


## Errors ----

test_that("error if `points` is not an SF object containing points", {
  expect_error(
    count_points_in_polygons(points = data_df, polygons = polygons)
  )
  expect_error(count_points_in_polygons(
    points = sf::st_cast(data_sf, "LINESTRING"),
    polygons = polygons
  ))
})

test_that("error if `polygons` is not an SF object containing polygons", {
  expect_error(count_points_in_polygons(points = data_sf, polygons = data_df))
  expect_error(count_points_in_polygons(
    points = data_sf,
    polygons = sf::st_cast(data_sf, "LINESTRING")
  ))
})

test_that("error if `weights` is not NULL or the name of a colum in `points`", {
  expect_error(count_points_in_polygons(
    points = data_sf,
    polygons = polygons,
    wt = character()
  ))
  expect_error(count_points_in_polygons(
    points = data_sf,
    polygons = polygons,
    wt = "blah"
  ))
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("function produces an SF tibble", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
})

test_that("output object has the required column names", {
  expect_equal(names(result), c("n", "geometry"))
  expect_equal(
    names(count_points_in_polygons(
      points = data_sf,
      polygons = polygons,
      weights = "wt"
    )),
    c("n", "sum", "geometry")
  )
})

test_that("columns in output have the required types", {
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})
