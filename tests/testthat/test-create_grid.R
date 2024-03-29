data_sf <- head(memphis_robberies, 10)
data_sf_m <- sf::st_transform(data_sf, "EPSG:2843")
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))

result <- create_grid(data = data_sf, cell_size = 0.1)



# CHECK INPUTS -----------------------------------------------------------------


## Errors ----

test_that("error if `data` is not an SF object", {
  expect_error(create_grid(data = data_df))
})

test_that("error if `cell_size` is not `NULL` or or a single positive number", {
  expect_error(create_grid(data = data_sf, cell_size = character()))
  expect_error(create_grid(data = data_sf, cell_size = 1:2))
  expect_error(create_grid(data = data_sf, cell_size = -1))
  expect_error(create_grid(data = data_sf, cell_size = 0))
})

test_that("error if `grid_type` is not 'rect' or 'hex'", {
  expect_error(create_grid(data = data_sf, grid_type = "triangle"))
})

test_that("error if `quiet` is not `TRUE` or `FALSE`", {
  expect_error(create_grid(data = data_sf, quiet = character()))
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("function produces an SF tibble", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_s3_class(
    create_grid(data = data_sf, cell_size = 0.1, grid_type = "hex"),
    "sf"
  )
})

test_that("output object has the required column names", {
  expect_equal(names(result), c("geometry"))
})

test_that("columns in output have the required types", {
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})


## Warnings ----

test_that("produces warning if grid has 100,000+ cells", {
  expect_warning(create_grid(data_sf_m, cell_size = 40, quiet = FALSE))
})
