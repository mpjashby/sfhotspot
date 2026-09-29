# A coarse toy grid is sufficient for testing the high-level Gi* orchestration.
# The standalone `gistar()` tests cover the statistic itself in greater detail.
data_sf <- sf::st_transform(head(memphis_robberies, 100), 2843)
data_sf$wt <- seq_len(nrow(data_sf))
cell_size <- 5000
grid <- hotspot_grid(data_sf, cell_size = cell_size, quiet = TRUE)
data_lonlat <- sf::st_transform(head(data_sf, 30), 4326)
grid_lonlat <- sf::st_transform(grid, 4326)

# To speed up the checking process, run the function with arguments that should
# not produce any errors or warnings
result <- hotspot_gistar(data_sf, grid = grid, quiet = TRUE)
result_wt <- hotspot_gistar(data_sf, grid = grid, weights = wt, quiet = TRUE)
result_no_kde <- hotspot_gistar(data_sf, grid = grid, kde = FALSE, quiet = TRUE)
result_wt_no_kde <- hotspot_gistar(
  data_sf,
  grid = grid,
  weights = wt,
  kde = FALSE,
  quiet = TRUE
)



# CHECK INPUTS -----------------------------------------------------------------

# Note that common inputs are tested in `validate_inputs()` and tested in the
# corresponding test file


## Errors ----

test_that("error for lon/lat `data` if `transform = FALSE`", {
  expect_error(
    hotspot_gistar(data_lonlat, transform = FALSE, quiet = TRUE),
    "Cannot calculate KDE values for lon/lat data"
  )
})



## Messages ----

test_that("message if `data` uses a geographic CRS and KDE not performed", {
  expect_message(
    hotspot_gistar(data_lonlat, grid = grid_lonlat, kde = FALSE)
  )
})

test_that("cell size is extracted silently from a supplied grid", {
  # This test concerns supplied-grid precedence, so the shared coarse cell size
  # avoids repeated fine-resolution Gi* calculations.
  supplied_grid <- hotspot_grid(data_sf, cell_size = cell_size, quiet = TRUE)

  expect_no_message(
    grid_result <- hotspot_gistar(data_sf, grid = supplied_grid, kde = FALSE)
  )
  expect_equal(
    grid_result,
    hotspot_gistar(
      data_sf,
      grid = supplied_grid,
      cell_size = cell_size / 2,
      kde = FALSE,
      quiet = TRUE
    )
  )
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("every return branch produces an hspt_g SF tibble (#82)", {
  for (output in list(result, result_wt, result_no_kde, result_wt_no_kde)) {
    expect_identical(class(output)[[1]], "hspt_g")
    expect_s3_class(output, "hspt_g")
    expect_s3_class(output, "sf")
    expect_s3_class(output, "tbl_df")
  }
})

test_that("standard SF printing and subsetting are preserved (#82)", {
  expect_output(print(result), "Simple feature collection")

  subset <- result[1:2, c("gistar", "geometry")]
  expect_s3_class(subset, "hspt_g")
  expect_s3_class(subset, "sf")
  expect_equal(names(subset), c("gistar", "geometry"))
  expect_equal(nrow(subset), 2)
})

test_that("function calculates KDE values for lon/lat data", {
  # One small geographic end-to-end case retains transformation and KDE
  # coverage without generating an automatically fine grid.
  result_lonlat <- hotspot_gistar(
    data_lonlat,
    grid = grid_lonlat,
    quiet = TRUE
  )

  expect_s3_class(result_lonlat, "sf")
  expect_type(result_lonlat$kde, "double")
  expect_equal(sf::st_crs(result_lonlat), sf::st_crs(data_lonlat))
})

test_that("output object has the required column names", {
  expect_equal(names(result), c("n", "kde", "gistar", "pvalue", "geometry"))
  expect_equal(
    names(result_wt),
    c("n", "sum", "kde", "gistar", "pvalue", "geometry")
  )
  expect_equal(
    names(result_wt_no_kde),
    c("n", "sum", "gistar", "pvalue", "geometry")
  )
  expect_equal(
    names(result_no_kde),
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

test_that("NULL uses Holm adjustment once (#94)", {
  holm_result <- hotspot_gistar(
    data_sf,
    grid = grid,
    kde = FALSE,
    p_adjust_method = "holm",
    quiet = TRUE
  )
  unadjusted_result <- hotspot_gistar(
    data_sf,
    grid = grid,
    kde = FALSE,
    p_adjust_method = "none",
    quiet = TRUE
  )

  expect_equal(result_no_kde$pvalue, holm_result$pvalue)
  expect_false(isTRUE(all.equal(
    result_no_kde$pvalue,
    unadjusted_result$pvalue
  )))
})

test_that("weights affect Gi* statistics and p-values (#86)", {
  expect_false(isTRUE(all.equal(result$gistar, result_wt$gistar)))
  expect_false(isTRUE(all.equal(result$pvalue, result_wt$pvalue)))
})
