set.seed(123)

# KDE can only be calculated for projected co-ordinates, so first convert data
# to use local state plane CRS
data_sf <- sf::st_transform(head(memphis_robberies, 100), 2843)
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))
data_missing_crs <- sf::st_sf(
  row = 1:2,
  geometry = sf::st_sfc(sf::st_point(c(1, 1)), sf::st_point(c(2, 2)))
)

# To speed up the checking process, run the function with arguments that should
# not produce any errors or warnings
grid <- create_grid(data = data_sf, cell_size = 1000)
result <- kernel_density(data = data_sf, grid = grid, bandwidth = 10000)



# CHECK INPUTS -----------------------------------------------------------------

# Note that only inputs evaluated in `kernel_density()` are tested here; those
# evaluated in helper functions are tested in the test files for those functions


## Errors ----

test_that("error if `data` has lon/lat co-ordinates", {
  expect_error(kernel_density(
    data = sf::st_transform(data_sf, 4326),
    grid = grid,
    bandwidth = 10000
  ))
})

test_that("error if `data` has no CRS", {
  expect_error(kernel_density(
    data = data_missing_crs,
    grid = grid,
    bandwidth = 10000
  ))
})

test_that("error if `grid` is not an SF object containing polygons", {
  expect_error(kernel_density(
    data = data_sf,
    grid = tibble::tibble(x = 1:3),
    bandwidth = 10000
  ))
  expect_error(kernel_density(
    data = data_sf,
    grid = sf::st_centroid(grid),
    bandwidth = 10000
  ))
})

test_that("error if `grid` has lon/lat co-ordinates", {
  expect_error(
    kernel_density(data = data_sf, grid = sf::st_transform(grid, 4326))
  )
})

test_that("error if `bandwidth` is not `NULL` or or a single positive number", {
  expect_error(
    kernel_density(data = data_sf, grid = grid, bandwidth = character())
  )
  expect_error(kernel_density(data = data_sf, grid = grid, bandwidth = 1:2))
  expect_error(kernel_density(data = data_sf, grid = grid, bandwidth = -1))
  expect_error(kernel_density(data = data_sf, grid = grid, bandwidth = 0))
})

test_that("error if `bandwidth_adjust` is not a single positive number", {
  expect_error(
    kernel_density(data = data_sf, grid = grid, bandwidth_adjust = character())
  )
  expect_error(
    kernel_density(data = data_sf, grid = grid, bandwidth_adjust = 1:2)
  )
  expect_error(
    kernel_density(data = data_sf, grid = grid, bandwidth_adjust = -1)
  )
  expect_error(
    kernel_density(data = data_sf, grid = grid, bandwidth_adjust = 0)
  )
})

test_that("error if `quiet` is not `TRUE` or `FALSE`", {
  expect_error(kernel_density(
    data = data_sf,
    grid = grid,
    bandwidth = 10000,
    quiet = character())
  )
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("output is an SF tibble", {
  # Multiple tests are needed here to get 100% coverage for this function
  expect_s3_class(result, "sf")
  expect_s3_class(kernel_density(data = data_sf, grid = grid), "sf")
  expect_s3_class(
    kernel_density(
      data = data_sf,
      grid = grid,
      bandwidth = 10000,
      quiet = FALSE
    ),
    "sf"
  )
  expect_s3_class(result, "tbl_df")
  expect_s3_class(
    kernel_density(data = data_sf, grid = grid, kernel = "triweight"),
    "sf"
  )
})

test_that("output object has the required column names", {
  expect_equal(names(result), c("kde_value", "geometry"))
})

test_that("columns in output have the required types", {
  expect_type(result$kde_value, "double")
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})

test_that("column values are within the specified range", {
  expect_true(all(result$kde_value >= 0))
})
