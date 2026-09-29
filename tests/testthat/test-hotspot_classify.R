# The classification tests need several time periods and occupied cells, but not
# the full example dataset. Evenly spaced rows retain the complete date range in
# a 200-point fixture rather than taking 200 records from only the earliest dates.
data_sf <- memphis_robberies[unique(round(seq(
  1,
  nrow(memphis_robberies),
  length.out = 200
))), ]
cell_size <- 0.03

result <- hotspot_classify(data_sf, cell_size = cell_size, quiet = TRUE)

# CHECK INPUTS -----------------------------------------------------------------

# Note that common inputs are tested in `validate_inputs()` and tested in the
# corresponding test file

test_that("error if no Date/POSIX columns present in the data", {
  expect_error(
    hotspot_classify(data_sf[, c("uid", "offense_type", "geometry")])
  )
})

test_that("error if multiple Date/POSIX columns present and none specified", {
  data_sf2 <- data_sf
  data_sf2$date2 <- data_sf$date
  expect_error(hotspot_classify(data_sf2))
})

test_that("error if specified `time` column is not Date/POSIX", {
  expect_error(hotspot_classify(data_sf, time = "offense_type"))
})

test_that("error if specified `time` column is not present in the data", {
  expect_error(hotspot_classify(data_sf, time = "some_column"))
})

test_that("error if inputs don't have correct types", {
  expect_error(hotspot_classify(data_sf, period = 1))
  expect_error(hotspot_classify(data_sf, period = "foo"))
  expect_error(hotspot_classify(data_sf, start = "foo"))
  expect_error(hotspot_classify(data_sf, collapse = "foo"))
  expect_error(hotspot_classify(data_sf, params = "foo"))
})

test_that("error if inputs aren't of correct length", {
  expect_error(hotspot_classify(data_sf, period = c("1 month", "1 week")))
  expect_error(
    hotspot_classify(data_sf, start = as.Date(c("2022-02-12", "2022-02-13")))
  )
  expect_error(hotspot_classify(data_sf, collapse = "foo"))
  expect_error(
    hotspot_classify(data_sf, params = hotspot_classify_params()[1:2])
  )
})

test_that("error if values are of the correct type/length but are invalid", {
  expect_error(hotspot_classify(data_sf, start = Sys.Date()))
})



# CHECK OUTPUTS ----------------------------------------------------------------

## Correct outputs ----

test_that("function produces an SF tibble with the class hspt_c", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "hspt_c")
})

test_that("output object has the required column names", {
  expect_equal(names(result), c("hotspot_category", "geometry"))
})

test_that("columns in output have the required types", {
  expect_type(result$hotspot_category, "character")
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})

test_that("automatic period and cell size are reported", {
  # Automatic cell-size selection is tested directly elsewhere. Mock its value
  # here so this orchestration branch remains covered without recreating the
  # fine grid that made the original classification tests slow.
  local_mocked_bindings(
    set_cell_size = function(...) cell_size,
    .package = "sfhotspot"
  )

  messages <- capture_messages(automatic <- hotspot_classify(data_sf))

  expect_true(any(grepl("period.*set to.*automatically", messages)))
  expect_s3_class(automatic, "hspt_c")
})

test_that("p-values are not adjusted again across periods (#94)", {
  testthat::local_mocked_bindings(
    gistar = function(counts, ...) {
      counts$gistar <- 1
      counts$pvalue <- c(0.01, rep(1, nrow(counts) - 1))
      counts
    },
    .package = "sfhotspot"
  )

  classified <- hotspot_classify(
    data_sf,
    period = "1 month",
    cell_size = cell_size,
    quiet = TRUE
  )

  expect_identical(classified$hotspot_category[[1]], "persistent hotspot")
})

test_that("cell size is extracted silently from a supplied grid (#87)", {
  data_projected <- sf::st_transform(data_sf, 2843)
  # The regression is about extracting a supplied cell size, so use a coarse
  # grid that keeps the repeated classifications small.
  grid <- hotspot_grid(data_projected, cell_size = 5000, quiet = TRUE)

  # Regression test for https://github.com/mpjashby/sfhotspot/issues/87
  messages <- testthat::capture_messages(
    grid_result <- hotspot_classify(
      data_projected,
      period = "1 month",
      grid = grid
    )
  )
  expected <- hotspot_classify(
    data_projected,
    period = "1 month",
    grid = grid,
    params = hotspot_classify_params(
      nb_dist = get_cell_size(grid) * sqrt(2)
    ),
    quiet = TRUE
  )

  expect_false(any(grepl("Cell size set", messages, fixed = TRUE)))
  expect_equal(grid_result$hotspot_category, expected$hotspot_category)
})
