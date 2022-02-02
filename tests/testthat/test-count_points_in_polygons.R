data_sf <- suppressWarnings(sf::st_centroid(sf::read_sf(system.file("shape/nc.shp", package = "sf"))))
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))

polygons <- create_grid(data = data_sf, cell_size = 0.1)
result <- count_points_in_polygons(
  points = data_sf,
  polygons = polygons
)



# CHECK INPUTS -----------------------------------------------------------------


## Errors ----

test_that("function produces an error if `points` is not an SF object containing points", {
  expect_error(count_points_in_polygons(points = data_df, polygons = polygons))
  expect_error(count_points_in_polygons(
    points = sf::st_cast(data_sf, "LINESTRING"),
    polygons = polygons
  ))
})

test_that("function produces an error if `polygons` is not an SF object containing polygons", {
  expect_error(count_points_in_polygons(points = data_sf, polygons = data_df))
  expect_error(count_points_in_polygons(
    points = data_sf,
    polygons = sf::st_cast(data_sf, "LINESTRING")
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
})

test_that("columns in output have the required types", {
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})

# Remember to run `snapshot_accept()` if the value for this test changes
test_that("output has not changed since last time the package was checked", {
  expect_snapshot_value(result, style = "serialize")
})
