data_sf <- sf::st_transform(head(memphis_robberies, 1000), 2843)

# To speed up the checking process, run the function with arguments that should
# not produce any errors or warnings
result <- hotspot_gistar(data_sf, quiet = TRUE)



# CHECK INPUTS -----------------------------------------------------------------

# Note that only inputs evaluated in `hotspot_gistar()` are tested here; those
# evaluated in helper functions are tested in the test files for those functions


## Errors ----

test_that("error if `data` is not an SF points object in a projected CRS", {
  expect_error(hotspot_gistar(data = sf::st_drop_geometry(data_sf)))
  expect_error(hotspot_gistar(data = sf::st_cast(data_sf, "LINESTRING")))
  expect_error(hotspot_gistar(data = sf::st_transform(data_sf, 4326)))
})

test_that("error if inputs don't have correct types", {
  expect_error(hotspot_gistar(data = data_sf, quiet = character()))
})

test_that("error if inputs aren't of length 1", {
  expect_error(hotspot_gistar(data = data_sf, quiet = c(TRUE, FALSE)))
})


## Messages ----

test_that("message if `data` use a geographic CRS", {
  expect_message(
    hotspot_gistar(data = sf::st_transform(data_sf, 4326), kde = FALSE)
  )
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("function produces an SF tibble", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
})

test_that("output object has the required column names", {
  expect_equal(
    names(hotspot_gistar(data_sf, kde = FALSE)),
    c("n", "gistar", "pvalue", "geometry")
  )
  expect_equal(names(result), c("n", "kde", "gistar", "pvalue", "geometry"))
})

test_that("columns in output have the required types", {
  expect_type(result$n, "double")
  expect_type(result$kde, "double")
  expect_type(result$gistar, "double")
  expect_type(result$pvalue, "double")
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})

test_that("column values are within the specified range", {
  expect_true(all(result$n >= 0))
  expect_true(all(result$kde >= 0))
  expect_true(all(result$pvalue >= 0))
  expect_true(all(result$pvalue <= 1))
})

test_that("output has not changed since last time the package was checked", {
  expect_snapshot_value(result, style = "serialize")
})
