set.seed(123)

# Counting behaviour is independent of the full example-data scale. This small
# fixture includes enough occupied and empty cells for output and weighting tests.
data_sf <- head(memphis_robberies, 100)
data_sf$wt <- runif(nrow(data_sf), max = 1000)
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))
cell_size <- 0.03

# To speed up the checking process, run the function with arguments that should
# not produce any errors or warnings
result <- hotspot_count(data = data_sf, cell_size = cell_size, quiet = TRUE)



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
    names(hotspot_count(
      data = data_sf,
      cell_size = cell_size,
      weights = wt,
      quiet = TRUE
    )),
    c("n", "sum", "geometry")
  )
})

test_that("columns in output have the required types", {
  expect_type(result$n, "double")
  expect_type(
    hotspot_count(
      data = data_sf,
      cell_size = cell_size,
      weights = wt,
      quiet = TRUE
    )$sum,
    "double"
  )
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})

test_that("cell size is ignored silently when grid is provided", {
  grid <- hotspot_grid(data_sf, cell_size = cell_size, quiet = TRUE)

  expect_no_message(grid_result <- hotspot_count(data_sf, grid = grid))
  expect_equal(
    grid_result,
    hotspot_count(
      data_sf,
      grid = grid,
      cell_size = cell_size / 2,
      quiet = TRUE
    )
  )
})
