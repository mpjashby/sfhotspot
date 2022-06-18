set.seed(123)

data_sf <- head(memphis_robberies, 1000)
data_sf$wt <- runif(nrow(data_sf), max = 1000)
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))

# To speed up the checking process, run the function with arguments that should
# not produce any errors or warnings
result <- hotspot_count(data = data_sf)



# CHECK INPUTS -----------------------------------------------------------------

# Note that common inputs are tested in `validate_inputs()` and tested in the
# corresponding test file



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("output is an SF tibble with class hspt_n", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "hspt_n")
})

test_that("output object has the required column names", {
  expect_equal(names(result), c("n", "geometry"))
  expect_equal(
    names(hotspot_count(data = data_sf, weights = wt)),
    c("n", "sum", "geometry")
  )
})

test_that("columns in output have the required types", {
  expect_type(result$n, "double")
  expect_type(hotspot_count(data = data_sf, weights = wt)$sum, "double")
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})
