set.seed(123)

data_sf <- head(memphis_robberies, 10)
grid <- hotspot_grid(data_sf)
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))
grid_df <- as.data.frame(sf::st_drop_geometry(grid))
data_sf_empty <- data_sf
data_sf_empty$geometry[1] <- sf::st_point()
data_sf_zero <- data_sf
data_sf_zero$geometry[1] <- sf::st_point(x = c(0, 0))



# CHECK INPUTS -----------------------------------------------------------------

# Note that only inputs evaluated in `validate_inputs()` are tested here; those
# evaluated in helper functions are tested in the test files for those functions


## Errors ----

test_that("error if `data` or `grid` are not SF objects of the correct type", {
  expect_error(validate_inputs(data = data_df, grid = grid, quiet = FALSE))
  expect_error(validate_inputs(data = data_sf, grid = grid_df, quiet = FALSE))
  expect_error(
    validate_inputs(
      data = sf::st_cast(data_sf, "LINESTRING"),
      grid = grid,
      quiet = FALSE
    )
  )
  expect_error(
    validate_inputs(
      data = data_sf,
      grid = sf::st_cast(grid, "LINESTRING"),
      quiet = FALSE
    )
  )
})

test_that("Error if `data` contains empty geometries", {
  expect_error(
    validate_inputs(data = data_sf_empty, grid = grid, quiet = FALSE)
  )
})

test_that("error if `quiet` is not a single `TRUE` or `FALSE` value", {
  expect_error(
    validate_inputs(data = data_sf, grid = grid, quiet = character())
  )
  expect_error(
    validate_inputs(data = data_sf, grid = grid, quiet = c(TRUE, FALSE))
  )
})

test_that(
  "error if `bandwidth` or `bandwidth_adjust` are of the wrong type or length",
  {
    expect_error(validate_bandwidth(bandwidth = "blah"))
    expect_error(validate_bandwidth(bandwidth = c(2, 3)))
    expect_error(validate_bandwidth(bandwidth = -1))
    expect_error(validate_bandwidth(adjust = "blah"))
    expect_error(validate_bandwidth(adjust = c(2, 3)))
    expect_error(validate_bandwidth(adjust = -1))
  }
)


## Warnings ----

test_that("Warning if `data` contain zero co-ordinates", {
  expect_warning(
    validate_inputs(data = data_sf_zero, grid = grid, quiet = FALSE)
  )
})



# CHECK OUTPUTS


## Correct outputs ----

test_that("Result is an invisible `NULL` value", {
  expect_invisible(validate_inputs(data = data_sf, grid = grid, quiet = FALSE))
  expect_null(validate_inputs(data = data_sf, grid = grid, quiet = FALSE))
  expect_invisible(validate_bandwidth(bandwidth = 1))
  expect_null(validate_bandwidth(bandwidth = 1))
})
