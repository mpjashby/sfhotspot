# KDE can only be calculated for projected co-ordinates, so first convert data
# to use local state plane CRS
data_sf <- sf::st_transform(head(memphis_robberies, 100), 2843)
data_sf$wt <- runif(nrow(data_sf), max = 1000)
data_lonlat <- sf::st_transform(data_sf, 4326)
data_lonlat_y <- data_lonlat[seq(1, nrow(data_lonlat), by = 2), ]
grid_lonlat <- hotspot_grid(data_lonlat, cell_size = 0.03, quiet = TRUE)

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
  expect_error(hotspot_dual_kde(x = data_sf, y = data_sf, transform = "yes"))
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

test_that("error if `y` and the analysis grid use different CRSs", {
  expect_error(
    hotspot_dual_kde(
      x = data_lonlat,
      y = sf::st_transform(data_lonlat_y, 32616),
      grid = grid_lonlat,
      bandwidth = 10000,
      quiet = TRUE
    ),
    "must use the same co-ordinate reference system"
  )
})

test_that("error if `y` does not overlap the analysis grid", {
  data_lonlat_far <- data_lonlat_y
  sf::st_geometry(data_lonlat_far) <-
    sf::st_geometry(data_lonlat_far) + c(10, 0)
  sf::st_crs(data_lonlat_far) <- sf::st_crs(data_lonlat_y)

  expect_error(
    hotspot_dual_kde(
      x = data_lonlat,
      y = data_lonlat_far,
      cell_size = 0.03,
      bandwidth = 10000,
      quiet = TRUE
    ),
    "must overlap"
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

test_that("lon/lat data use valid projected bandwidths and retain their CRS", {
  data_projected <- st_transform_auto(data_lonlat, quiet = TRUE)
  data_y_projected <- sf::st_transform(
    data_lonlat_y,
    sf::st_crs(data_projected)
  )
  bandwidth_x <- set_bandwidth(data_projected)
  bandwidth_y <- set_bandwidth(data_y_projected)

  result_default <- hotspot_dual_kde(
    data_lonlat,
    data_lonlat_y,
    grid = grid_lonlat,
    quiet = TRUE
  )
  result_shared <- hotspot_dual_kde(
    data_lonlat,
    data_lonlat_y,
    grid = grid_lonlat,
    bandwidth = bandwidth_x,
    quiet = TRUE
  )
  result_separate <- hotspot_dual_kde(
    data_lonlat,
    data_lonlat_y,
    grid = grid_lonlat,
    bandwidth = list(NULL, NULL),
    quiet = TRUE
  )
  result_separate_expected <- hotspot_dual_kde(
    data_lonlat,
    data_lonlat_y,
    grid = grid_lonlat,
    bandwidth = list(bandwidth_x, bandwidth_y),
    quiet = TRUE
  )

  expect_false(isTRUE(all.equal(bandwidth_x, bandwidth_y)))
  expect_equal(result_default$kde, result_shared$kde)
  expect_equal(result_separate$kde, result_separate_expected$kde)
  expect_true(all(is.finite(result_default$kde)))
  expect_equal(sf::st_crs(result_default), sf::st_crs(data_lonlat))
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

test_that("message when data were transformed", {
  expect_message(
    hotspot_dual_kde(
      x = data_lonlat,
      y = data_lonlat,
      cell_size = 0.005,
      bandwidth = 10000
    ),
    "Data transformed to "
  )
  expect_no_message(
    hotspot_dual_kde(
      x = data_lonlat,
      y = data_lonlat,
      cell_size = 0.005,
      bandwidth = 10000,
      quiet = TRUE
    )
  )
})
