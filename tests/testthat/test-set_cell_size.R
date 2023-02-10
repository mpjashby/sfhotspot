data_sf <- sf::st_transform(head(memphis_robberies, 10), "EPSG:2843")
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))
data_missing_crs <- sf::st_sf(
  row = 1:2,
  geometry = sf::st_sfc(sf::st_point(c(1, 1)), sf::st_point(c(2, 2)))
)
small_polygon <- sf::st_sf(
  "geometry" = sf::st_sfc(sf::st_polygon(
    list(matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE))
  )),
  crs = "EPSG:27700"
)


# CHECK INPUTS -----------------------------------------------------------------


## Errors ----

test_that("error if `data` is not an SF object", {
  expect_error(set_cell_size(data = data_df))
})

test_that("error if `round` is not `TRUE` or `FALSE`", {
  expect_error(set_cell_size(data = data_sf, round = character()))
})

test_that("error if `quiet` is not `TRUE` or `FALSE`", {
  expect_error(set_cell_size(data = data_sf, quiet = character()))
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("produces a single numeric value", {
  expect_type(set_cell_size(data_sf), "double")
  expect_length(set_cell_size(data_sf), 1)
})

test_that("output has not changed since last time the package was checked", {
  expect_snapshot_value(
    set_cell_size(data = data_sf, round = TRUE),
    style = "deparse"
  )
  expect_snapshot_value(
    set_cell_size(data = data_sf, round = FALSE),
    style = "deparse"
  )
  expect_snapshot_value(
    set_cell_size(data = data_missing_crs),
    style = "deparse"
  )
})

test_that("small area does not cause zero cell size (#26)", {
  expect_gt(set_cell_size(small_polygon, round = TRUE), 0)
})


## Messages ----

test_that("produces message advising of calculated cell size", {
  expect_message(set_cell_size(data = data_sf, quiet = FALSE))
})
