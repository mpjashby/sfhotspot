# KDE can only be calculated for projected co-ordinates, so first convert data
# to use local state plane CRS
data_sf <- sf::st_transform(head(memphis_robberies, 100), 2843)
data_sf$wt <- runif(nrow(data_sf), max = 1000)

# To speed up the checking process, run the function with arguments that should
# not produce any errors or warnings
result <- hotspot_dual_kde(
  x = data_sf,
  y = data_sf,
  cell_size = 1000,
  bandwidth = 10000,
  quiet = TRUE
)
result_wt <- hotspot_dual_kde(
  x = data_sf,
  y = data_sf,
  cell_size = 1000,
  bandwidth = 10000,
  weights = c(wt, wt),
  quiet = TRUE
)
result_dual_bdwth <- hotspot_dual_kde(
  x = data_sf,
  y = data_sf,
  cell_size = 1000,
  bandwidth = list(9000, 10000),
  quiet = TRUE
)
result_dual_adj <- hotspot_dual_kde(
  x = data_sf,
  y = data_sf,
  cell_size = 1000,
  bandwidth_adjust = list(0.5, 1),
  quiet = TRUE
)



# CHECK INPUTS -----------------------------------------------------------------

# Note that common inputs are tested in `validate_inputs()` and
# `validate_bandwidth()` then tested in the corresponding test file

test_that("error if input values have the wrong type", {
  expect_error(hotspot_dual_kde(x = data_sf, y = data_sf, method = TRUE))
  expect_error(hotspot_dual_kde(x = data_sf, y = data_sf, weights = TRUE))
})

test_that("error if input values have the wrong length", {
  expect_error(
    hotspot_dual_kde(x = data_sf, y = data_sf, method = character(0))
  )
  expect_error(
    hotspot_dual_kde(x = data_sf, y = data_sf, method = c("ratio", "log"))
  )
  expect_error(hotspot_dual_kde(x = data_sf, y = data_sf, weights = wt))
  expect_error(
    hotspot_dual_kde(x = data_sf, y = data_sf, weights = c(wt, wt, wt))
  )
})

test_that("error if input values have invalid", {
  expect_error(hotspot_dual_kde(x = data_sf, y = data_sf, method = "blah"))
  expect_error(
    hotspot_dual_kde(x = data_sf, y = data_sf, weights = c(blah, blah))
  )
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("output is an SF tibble with class hspt_k", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "hspt_k")
  expect_s3_class(result_wt, "sf")
  expect_s3_class(result_wt, "tbl_df")
  expect_s3_class(result_wt, "hspt_k")
  expect_s3_class(result_dual_bdwth, "sf")
  expect_s3_class(result_dual_bdwth, "tbl_df")
  expect_s3_class(result_dual_bdwth, "hspt_k")
  expect_s3_class(result_dual_adj, "sf")
  expect_s3_class(result_dual_adj, "tbl_df")
  expect_s3_class(result_dual_adj, "hspt_k")
})

test_that("output object has the required column names", {
  expect_equal(names(result), c("n", "kde", "geometry"))
  expect_equal(names(result_wt), c("n", "sum", "kde", "geometry"))
})

test_that("columns in output have the required types", {
  expect_type(result$n, "double")
  expect_type(result_wt$sum, "double")
  expect_type(result$kde, "double")
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})

test_that("no issues when grid provided", {
  expect_no_condition(
    hotspot_dual_kde(
      x = data_sf,
      y = data_sf,
      bandwidth = 10000,
      grid = hotspot_grid(data_sf, cell_size = 1000)
    )
  )
})

test_that("no issues with different methods", {
  expect_no_condition(
    hotspot_dual_kde(
      data_sf,
      data_sf,
      cell_size = 1000,
      bandwidth = 10000,
      method = "log"
    )
  )
  expect_no_condition(
    hotspot_dual_kde(
      data_sf,
      data_sf,
      cell_size = 1000,
      bandwidth = 10000,
      method = "diff"
    )
  )
  expect_no_condition(
    hotspot_dual_kde(
      data_sf,
      data_sf,
      cell_size = 1000,
      bandwidth = 10000,
      method = "sum"
    )
  )
})


## Messages ----

test_that("message when cell size set automatically", {
  expect_message(
    hotspot_dual_kde(data_sf, data_sf, bandwidth = 10000),
    "Cell size set to"
  )
})

test_that("message when bandwidth set automatically", {
  expect_message(
    hotspot_dual_kde(data_sf, data_sf, cell_size = 1000),
    "Bandwidth set automatically based on rule of thumb"
  )
  expect_message(
    hotspot_dual_kde(
      data_sf,
      data_sf,
      cell_size = 1000,
      bandwidth = list(NULL, 10000)
    ),
    "Bandwidth set automatically based on rule of thumb"
  )
})
