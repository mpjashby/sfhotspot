set.seed(123)

# KDE can only be calculated for projected co-ordinates, so first convert data
# to use local state plane CRS
data_sf <- sf::st_transform(head(memphis_robberies, 100), 2843)
data_sf$wt <- runif(nrow(data_sf), max = 1000)
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))
data_missing_crs <- sf::st_sf(
  row = 1:2,
  geometry = sf::st_sfc(sf::st_point(c(1, 1)), sf::st_point(c(2, 2)))
)

# To speed up the checking process, run the function with arguments that should
# not produce any errors or warnings
grid <- sf::st_set_geometry(
  create_grid(data = data_sf, cell_size = 1000),
  "random_geom_column_name"
)
result <- kernel_density(data = data_sf, grid = grid, bandwidth = 10000)
result_wt <- kernel_density(
  data = data_sf,
  grid = grid,
  bandwidth = 10000,
  weights = "wt"
)



# CHECK INPUTS -----------------------------------------------------------------

# Note that only inputs evaluated in `kernel_density()` are tested here; those
# evaluated in helper functions are tested in the test files for those functions


## Errors ----

test_that("error if `data` has lon/lat co-ordinates and `transform = FALSE`", {
  expect_error(
    kernel_density(
      data = sf::st_transform(data_sf, "EPSG:4326"),
      grid = grid,
      bandwidth = 10000,
      transform = FALSE
    ),
    "Cannot calculate KDE values for lon/lat data"
  )
  expect_no_error(
    kernel_density(
      data = sf::st_transform(data_sf, "EPSG:4326"),
      grid = sf::st_transform(grid, "EPSG:4326"),
      bandwidth = 10000,
      transform = TRUE
    )
  )
})

test_that("error if `data` has no CRS", {
  expect_error(
    kernel_density(
      data = data_missing_crs,
      grid = grid,
      bandwidth = 10000
    ),
    "is missing"
  )
})

test_that("error if `grid` is not an SF object containing polygons", {
  expect_error(kernel_density(
    data = data_sf,
    grid = tibble::tibble(x = 1:3),
    bandwidth = 10000
  ))
  expect_error(kernel_density(
    data = data_sf,
    grid = sf::st_centroid(grid),
    bandwidth = 10000
  ))
})

test_that("error if `grid` has lon/lat co-ordinates and `transform = FALSE`", {
  expect_error(
    kernel_density(data = data_sf, grid = sf::st_transform(grid, 4326))
  )
  expect_no_error(
    kernel_density(
      data = sf::st_transform(data_sf, 4326), 
      grid = sf::st_transform(grid, 4326), 
      transform = TRUE
    )
  )
})

test_that("error if `bandwidth` is not `NULL` or a single positive number", {
  expect_error(
    kernel_density(data = data_sf, grid = grid, bandwidth = character())
  )
  expect_error(kernel_density(data = data_sf, grid = grid, bandwidth = 1:2))
  expect_error(kernel_density(data = data_sf, grid = grid, bandwidth = -1))
  expect_error(kernel_density(data = data_sf, grid = grid, bandwidth = 0))
})

test_that("error if `bandwidth_adjust` is not a single positive number", {
  expect_error(
    kernel_density(data = data_sf, grid = grid, bandwidth_adjust = character())
  )
  expect_error(
    kernel_density(data = data_sf, grid = grid, bandwidth_adjust = 1:2)
  )
  expect_error(
    kernel_density(data = data_sf, grid = grid, bandwidth_adjust = -1)
  )
  expect_error(
    kernel_density(data = data_sf, grid = grid, bandwidth_adjust = 0)
  )
})

test_that("error if `weights` is not the name of a column in the data", {
  expect_error(
    kernel_density(data = data_sf, grid = grid, weights = "blah"),
    "`weights` must be NULL or the name of a single column"
  )
})

test_that("error if `weights` is not numeric", {
  expect_error(
    kernel_density(data = data_sf, grid = grid, weights = "date"),
    "name of a column of numeric values"
  )
})

test_that("error if `transform` is not `TRUE`/`FALSE`", {
  expect_error(
    kernel_density(
      data = data_sf,
      grid = grid,
      bandwidth = 10000,
      transform = character()
    )
  )
})

test_that("error if `quiet` is not `TRUE`/`FALSE`", {
  expect_error(
    kernel_density(
      data = data_sf,
      grid = grid,
      bandwidth = 10000,
      quiet = character()
    )
  )
})

## Messages ----

test_that("message if `data` has lon/lat co-ordinates", {
  expect_message(
    kernel_density(
      data = sf::st_transform(data_sf, "EPSG:4326"),
      grid = sf::st_transform(grid, "EPSG:4326"),
      bandwidth = 10000,
      transform = TRUE,
      quiet = FALSE
    ),
    "Data transformed to"
  )
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("output is an SF tibble", {
  # Multiple tests are needed here to get 100% coverage for this function
  expect_s3_class(result, "sf")
  expect_s3_class(result_wt, "sf")
  expect_s3_class(kernel_density(data = data_sf, grid = grid), "sf")
  expect_s3_class(
    kernel_density(
      data = data_sf,
      grid = grid,
      bandwidth = 10000,
      quiet = FALSE
    ),
    "sf"
  )
  expect_s3_class(
    kernel_density(
      data = data_sf,
      grid = grid,
      bandwidth = 10000,
      weights = "wt",
      quiet = FALSE
    ),
    "sf"
  )
  expect_s3_class(result, "tbl_df")
  expect_s3_class(
    kernel_density(data = data_sf, grid = grid, kernel = "triweight"),
    "sf"
  )
})

test_that("output object has the required column names", {
  expect_equal(names(result), c("kde_value", "geometry"))
})

test_that("columns in output have the required types", {
  expect_type(result$kde_value, "double")
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})

test_that("column values are within the specified range", {
  expect_true(all(result$kde_value >= 0))
})

test_that("automatic transformation restores the exact geographic CRS (#89)", {
  data_etrs89 <- sf::st_transform(data_sf, 4258)
  grid_etrs89 <- sf::st_transform(grid, 4258)

  result_etrs89 <- kernel_density(
    data_etrs89,
    grid_etrs89,
    bandwidth = 10000,
    transform = TRUE,
    quiet = TRUE
  )

  expect_equal(sf::st_crs(result_etrs89), sf::st_crs(data_etrs89))
})

test_that("data and grid use one automatically selected analysis CRS (#90)", {
  boundary_data <- sf::st_as_sf(
    data.frame(
      x = c(-89.9995, -89.9985, -89.9975),
      y = c(35, 35.001, 35.002)
    ),
    coords = c("x", "y"),
    crs = 4326
  )
  ring <- matrix(
    c(
      -90.010, 34.990,
      -89.995, 34.990,
      -89.995, 35.010,
      -90.010, 35.010,
      -90.010, 34.990
    ),
    ncol = 2,
    byrow = TRUE
  )
  boundary_grid <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
  )
  analysis_crs <- sf::st_crs(
    st_transform_auto(boundary_data, check = FALSE, quiet = TRUE)
  )

  result_auto <- kernel_density(
    boundary_data,
    boundary_grid,
    bandwidth = 1000,
    transform = TRUE,
    quiet = TRUE
  )
  result_explicit <- kernel_density(
    sf::st_transform(boundary_data, analysis_crs),
    sf::st_transform(boundary_grid, analysis_crs),
    bandwidth = 1000,
    transform = FALSE,
    quiet = TRUE
  )

  expect_equal(result_auto$kde_value, result_explicit$kde_value)
  expect_equal(sf::st_crs(result_auto), sf::st_crs(boundary_data))
})
