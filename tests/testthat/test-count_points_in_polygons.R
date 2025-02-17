data_sf <- suppressWarnings(
  sf::st_centroid(sf::read_sf(system.file("shape/nc.shp", package = "sf")))
)
data_sf$wt <- runif(nrow(data_sf), max = 1000)
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))

# Create polygons object with extra column (to check if it's kept) and unusual
# geometry column name
polygons <- create_grid(data = data_sf, cell_size = 0.1)
polygons$extra_col <- TRUE
polygons <- sf::st_set_geometry(polygons, "random_geom_column_name")

# Move one point outside the area covered by the polygons
points_outside <- data_sf
points_outside$geometry[[1]] <- sf::st_point(c(0, 0))

result <- count_points_in_polygons(
  points = data_sf,
  polygons = polygons
)



# CHECK INPUTS -----------------------------------------------------------------


## Errors ----

test_that("error if `points` is not an SF object containing points", {
  expect_error(
    count_points_in_polygons(points = data_df, polygons = polygons)
  )
  expect_error(count_points_in_polygons(
    points = sf::st_cast(data_sf, "LINESTRING"),
    polygons = polygons
  ))
})

test_that("error if `polygons` is not an SF object containing polygons", {
  expect_error(count_points_in_polygons(points = data_sf, polygons = data_df))
  expect_error(count_points_in_polygons(
    points = data_sf,
    polygons = sf::st_cast(data_sf, "LINESTRING")
  ))
})

test_that("error if `weights` is not NULL or the name of a colum in `points`", {
  expect_error(
    count_points_in_polygons(data_sf, polygons, weights = "blah"),
    "`weights` must be `NULL` or the name of a single column"
  )
  expect_error(
    count_points_in_polygons(data_sf, polygons, weights = "NAME"),
    "`weights` must be `NULL` or the name of a column of numeric values"
  )
})

## Warnings ----

test_that("warning if some points outside polygons", {
  expect_warning(
    count_points_in_polygons(points_outside, polygons = polygons),
    "outside the area covered by the supplied polygons"
  )
})

test_that("warning if `polygons` includes column names used internally", {
  polygons_warn <- polygons
  expect_warning(
    {
      polygons_warn$n <- 1
      count_points_in_polygons(points = data_sf, polygons = polygons_warn)
      polygons_warn$n <- NULL
    },
    regexp = "Existing column `n` will be overwritten."
  )
  expect_warning(
    {
      polygons_warn$`.polygon_id` <- 1
      count_points_in_polygons(points = data_sf, polygons = polygons_warn)
      polygons_warn$`.polygon_id` <- NULL
    },
    regexp = "Existing column `.polygon_id` will be removed."
  )
  expect_warning(
    {
      polygons_warn$sum <- 1
      count_points_in_polygons(points = data_sf, polygons = polygons_warn)
      polygons_warn$sum <- NULL
    },
    regexp = "Existing column `sum` will be removed."
  )
  expect_warning(
    {
      polygons_warn$sum <- 1
      count_points_in_polygons(
        points = data_sf,
        polygons = polygons_warn,
        weights = "wt"
      )
      polygons_warn$sum <- NULL
    },
    regexp = "Existing column `sum` will be overwritten."
  )
  expect_warning(
    {
      polygons_warn$x <- 1
      count_points_in_polygons(points = data_sf, polygons = polygons_warn)
      polygons_warn$x <- NULL
    },
    regexp = "Existing column `x` will be removed."
  )
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("function produces an SF tibble", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
})

test_that("output object has the required column names", {
  expect_contains(names(result), c("n", "geometry"))
  expect_contains(
    names(count_points_in_polygons(
      points = data_sf,
      polygons = polygons,
      weights = "wt"
    )),
    c("n", "sum", "geometry")
  )
})

test_that("output object does not have columns used internally", {
  expect_false(".polygon_id" %in% names(result))
  expect_false("x" %in% names(result))
})

test_that("columns in output have the required types", {
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})
