set.seed(123)

data_sf <- head(memphis_robberies, 10)
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))



# CHECK INPUTS -----------------------------------------------------------------


## Errors ----

test_that("function produces an error if `data` is not an SF object", {
  expect_error(set_bandwidth(data = data_df))
  expect_error(set_bandwidth(data = sf::st_cast(data_sf, "LINESTRING")))
})

test_that("function produces an error if `quiet` is not `TRUE` or `FALSE`", {
  expect_error(set_bandwidth(data = data_sf, quiet = character()))
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("output is a single numeric value", {
  expect_type(set_bandwidth(data = data_sf), "double")
  expect_length(set_bandwidth(data = data_sf), 1)
})


## Messages ----

test_that("message advising of calculated bandwidth", {
  expect_message(
    set_bandwidth(data = data_sf, quiet = FALSE),
    "Bandwidth set automatically based on rule of thumb"
  )
  expect_message(
    set_bandwidth(data = data_sf, label = "for `x`", quiet = FALSE),
    "Bandwidth set automatically based on rule of thumb"
  )
  expect_message(
    set_bandwidth(data = data_sf, adjust = 0.5, quiet = FALSE),
    "Adjusted bandwidth"
  )
})

test_that("adjusted bandwidth correctly reported", {
  expect_message(
    set_bandwidth(
      st_transform_auto(data_sf, quiet = TRUE), 
      adjust = 0.1, 
      quiet = FALSE
    ),
    "1,195 metres"
  )
})



# CHECK HELPER FUNCTIONS -------------------------------------------------------

test_that("function produces a single numeric value", {
  expect_type(bandwidth_nrd_sf(data_sf), "double")
  expect_length(bandwidth_nrd_sf(data_sf), 1)
})

test_that("function produces a single numeric value", {
  expect_type(bandwidth_nrd(1:10), "double")
  expect_length(bandwidth_nrd(1:10), 1)
})
