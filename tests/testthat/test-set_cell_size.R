data_sf <- sf::st_transform(head(memphis_robberies, 10), 2843)
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))



# CHECK INPUTS -----------------------------------------------------------------


## Errors ----

test_that("function produces an error if `data` is not an SF object", {
  expect_error(set_cell_size(data = data_df))
})

test_that("function produces an error if `round` is not `TRUE` or `FALSE`", {
  expect_error(set_cell_size(data = data_sf, round = character()))
})

test_that("function produces an error if `quiet` is not `TRUE` or `FALSE`", {
  expect_error(set_cell_size(data = data_sf, quiet = character()))
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("function produces a single numeric value", {
  expect_type(set_cell_size(data_sf), "double")
  expect_length(set_cell_size(data_sf), 1)
})

test_that("output has not changed since last time the package was checked", {
  expect_snapshot_value(set_cell_size(data = data_sf, round = TRUE), style = "deparse")
  expect_snapshot_value(set_cell_size(data = data_sf, round = FALSE), style = "deparse")
})


## Messages ----

test_that("function produces a message advising of calculated cell size", {
  expect_message(set_cell_size(data = data_sf, quiet = FALSE))
})
