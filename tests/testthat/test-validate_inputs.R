set.seed(123)

data_sf <- head(memphis_robberies, 10)
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))
data_sf_empty <- data_sf
data_sf_empty$geometry[1] <- sf::st_point()
data_sf_zero <- data_sf
data_sf_zero$geometry[1] <- sf::st_point(x = c(0, 0))



# CHECK INPUTS -----------------------------------------------------------------

# Note that only inputs evaluated in `validate_inputs()` are tested here; those
# evaluated in helper functions are tested in the test files for those functions


## Errors ----

test_that("error if `data` is not an SF object containing points", {
  expect_error(validate_inputs(data = data_df, quiet = FALSE))
  expect_error(
    validate_inputs(data = sf::st_cast(data_sf, "LINESTRING"), quiet = FALSE)
  )
})

test_that("Error if `data` contains empty geometries", {
  expect_error(validate_inputs(data = data_sf_empty, quiet = FALSE))
})

test_that("error if `quiet` is not a single `TRUE` or `FALSE` value", {
  expect_error(validate_inputs(data = data_sf, quiet = character()))
  expect_error(validate_inputs(data = data_sf, quiet = c(TRUE, FALSE)))
})


## Warnings ----

test_that("Warning if `data` contain zero co-ordinates", {
  expect_warning(validate_inputs(data = data_sf_zero, quiet = FALSE))
})



# CHECK OUTPUTS


## Correct outputs ----

test_that("Result is an invisible `NULL` value", {
  expect_invisible(validate_inputs(data = data_sf, quiet = FALSE))
  expect_null(validate_inputs(data = data_sf, quiet = FALSE))
})
