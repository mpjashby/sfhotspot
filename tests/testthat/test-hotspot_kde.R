# KDE can only be calculated for projected co-ordinates, so first convert data
# to use local state plane CRS
data_sf <- sf::st_transform(head(memphis_robberies, 100), 2843)
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))

# To speed up the checking process, run the function with arguments that should
# not produce any errors or warnings
result <- hotspot_kde(data = data_sf, cell_size = 1000, bandwidth = 10000, quiet = TRUE)



# CHECK INPUTS -----------------------------------------------------------------

# Note that only inputs evaluated in `hotspot_kde()` are tested here; those
# evaluated in helper functions are tested in the test files for those functions


## Errors ----

test_that("function produces an error if `data` is not an SF object containing points", {
  expect_error(hotspot_kde(data = data_df))
  expect_error(hotspot_kde(data = sf::st_cast(data_sf, "LINESTRING")))
})

test_that("function produces an error if `quiet` is not `TRUE` or `FALSE`", {
  expect_error(hotspot_kde(data = data_sf, quiet = character()))
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("function produces an SF tibble", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
})

test_that("output object has the required column names", {
  expect_equal(names(result), c("n", "kde", "geometry"))
})

test_that("columns in output have the required types", {
  expect_type(result$n, "double")
  expect_type(result$kde, "double")
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})
