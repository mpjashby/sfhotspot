set.seed(123)

data_sf <- head(memphis_robberies, 1000)
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))

# To speed up the checking process, run the function with arguments that should
# not produce any errors or warnings
result <- hotspot_count(data = data_sf)



# CHECK INPUTS -----------------------------------------------------------------

# Note that only inputs evaluated in `hotspot_count()` are tested here; those
# evaluated in helper functions are tested in the test files for those functions


## Errors ----

test_that("error if `data` is not an SF object containing points", {
  expect_error(hotspot_count(data = data_df))
  expect_error(hotspot_count(data = sf::st_cast(data_sf, "LINESTRING")))
})

test_that("error if `quiet` is not `TRUE` or `FALSE`", {
  expect_error(hotspot_count(data = data_sf, quiet = character()))
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("output is an SF tibble", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
})

test_that("output object has the required column names", {
  expect_equal(names(result), c("n", "geometry"))
})

test_that("columns in output have the required types", {
  expect_type(result$n, "double")
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})
