data_sf <- sf::st_transform(head(memphis_robberies, 1000), 2843)
data_sf$wt <- runif(nrow(data_sf), max = 1000)

# To speed up the checking process, run the function with arguments that should
# not produce any errors or warnings
result <- hotspot_gistar(data_sf, quiet = TRUE)
result_wt <- hotspot_gistar(data_sf, weights = wt, quiet = TRUE)



# CHECK INPUTS -----------------------------------------------------------------

# Note that common inputs are tested in `validate_inputs()` and tested in the
# corresponding test file


## Errors ----

test_that("error if `data` doesn't use a projected CRS", {
  expect_error(hotspot_gistar(data = sf::st_transform(data_sf, 4326)))
})



## Messages ----

test_that("message if `data` uses a geographic CRS and KDE not performed", {
  expect_message(
    hotspot_gistar(data = sf::st_transform(data_sf, 4326), kde = FALSE)
  )
})

test_that("no message reporting cell size when grid is provided", {
  expect_no_message(
    hotspot_gistar(data_sf, grid = hotspot_grid(data_sf, cell_size = 1000)),
    message = "Cell size set"
  )
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("function produces an SF tibble", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
})

test_that("output object has the required column names", {
  expect_equal(names(result), c("n", "kde", "gistar", "pvalue", "geometry"))
  expect_equal(
    names(result_wt),
    c("n", "sum", "kde", "gistar", "pvalue", "geometry")
  )
  expect_equal(
    names(hotspot_gistar(data_sf, weights = wt, kde = FALSE)),
    c("n", "sum", "gistar", "pvalue", "geometry")
  )
  expect_equal(
    names(hotspot_gistar(data_sf, kde = FALSE)),
    c("n", "gistar", "pvalue", "geometry")
  )
})

test_that("columns in output have the required types", {
  expect_type(result$n, "double")
  expect_type(result_wt$sum, "double")
  expect_type(result$kde, "double")
  expect_type(result_wt$kde, "double")
  expect_type(result$gistar, "double")
  expect_type(result$pvalue, "double")
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})

test_that("column values are within the specified range", {
  expect_true(all(result$n >= 0))
  expect_true(all(result$kde >= 0))
  expect_true(all(result_wt$kde >= 0))
  expect_true(all(result$pvalue >= 0))
  expect_true(all(result$pvalue <= 1))
})
