data_sf <- head(memphis_robberies, 10)
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))

result <- create_grid(data = data_sf, cell_size = 0.1)



# CHECK INPUTS -----------------------------------------------------------------


## Errors ----

test_that("function produces an error if `data` is not an SF object", {
  expect_error(create_grid(data = data_df))
})

test_that("function produces an error if `cell_size` is not `NULL` or or a single positive number", {
  expect_error(create_grid(data = data_sf, cell_size = character()))
  expect_error(create_grid(data = data_sf, cell_size = 1:2))
  expect_error(create_grid(data = data_sf, cell_size = -1))
  expect_error(create_grid(data = data_sf, cell_size = 0))
})

test_that("function produces an error if `grid_type` is not 'rect' or 'hex'", {
  expect_error(create_grid(data = data_sf, grid_type = "triangle"))
})

test_that("function produces an error if `quiet` is not `TRUE` or `FALSE`", {
  expect_error(create_grid(data = data_sf, quiet = character()))
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("function produces an SF tibble", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_s3_class(create_grid(data = data_sf, cell_size = 0.1, grid_type = "hex"), "sf")
})

test_that("output object has the required column names", {
  expect_equal(names(result), c("geometry"))
})

test_that("columns in output have the required types", {
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})

# Remember to run `snapshot_accept()` if the value for this test changes
test_that("output has not changed since last time the package was checked", {
  expect_snapshot_value(result, style = "serialize")
})
