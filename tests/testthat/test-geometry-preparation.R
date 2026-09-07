local_crs <- sf::st_crs(
  'LOCAL_CS["arbitrary",LOCAL_DATUM["unknown",0],UNIT["unknown",1]]'
)

square_points <- sf::st_as_sf(
  data.frame(id = 1:4, x = c(1, 1, 3, 3), y = c(1, 3, 1, 3)),
  coords = c("x", "y"),
  crs = 3857
)

test_that("unknown CRS is rejected by hotspot functions", {
  no_crs <- sf::st_set_crs(square_points, NA)
  expect_error(hotspot_count(no_crs, cell_size = 1), "reference system.*missing")
})

test_that("unknown units are required only for distance operations", {
  unknown_units <- square_points
  suppressWarnings(sf::st_crs(unknown_units) <- local_crs)
  expect_error(hotspot_count(unknown_units), "Unit metadata.*missing")

  grid <- sf::st_sf(
    geometry = sf::st_sfc(sf::st_polygon(list(matrix(
      c(0, 0, 4, 0, 4, 4, 0, 4, 0, 0),
      ncol = 2,
      byrow = TRUE
    ))), crs = local_crs)
  )
  expect_no_error(hotspot_count(unknown_units, grid = grid, quiet = TRUE))
  expect_no_error(hotspot_clip(grid, grid, quiet = TRUE))
})

test_that("MULTIPOINT is cast unless attributes are used", {
  multipoint <- sf::st_sf(
    weight = 2,
    geometry = sf::st_sfc(sf::st_multipoint(matrix(
      c(0, 0, 2, 2), ncol = 2, byrow = TRUE
    )), crs = 3857)
  )

  result <- hotspot_count(multipoint, cell_size = 1, quiet = TRUE)
  expect_equal(sum(result$n), 2)
  expect_error(
    hotspot_count(multipoint, cell_size = 1, weights = weight, quiet = TRUE),
    "cannot contain MULTIPOINT.*attribute data"
  )
})

test_that("empty geometries are removed and reported", {
  with_empty <- rbind(
    square_points,
    sf::st_sf(id = 5, geometry = sf::st_sfc(sf::st_point(), crs = 3857))
  )
  expect_message(
    hotspot_count(with_empty, cell_size = 1),
    "Removed 1 row.*empty geometry"
  )
  expect_no_message(hotspot_count(with_empty, cell_size = 1, quiet = TRUE))

  all_empty <- with_empty[5, ]
  expect_error(hotspot_count(all_empty, cell_size = 1), "No rows with non-empty geometry")
})

test_that("Z and M dimensions are removed before processing", {
  xym <- sf::st_sf(
    id = 1:4,
    geometry = sf::st_sfc(lapply(seq_len(4), function(i) {
      sf::st_point(c(i %% 2, i %/% 2, i), dim = "XYM")
    }), crs = 3857)
  )
  expect_no_error(hotspot_count(xym, cell_size = 1, quiet = TRUE))
})

test_that("new_hotspot_results validates and orders result classes", {
  result <- hotspot_grid(square_points, cell_size = 1, quiet = TRUE)
  classed <- new_hotspot_results(result, class = "hspt_test")
  expect_identical(class(classed)[[1]], "hspt_test")

  expect_error(
    new_hotspot_results(result[0, ], class = "hspt_test"),
    "zero output rows"
  )

  empty_result <- result[1, ]
  sf::st_geometry(empty_result) <- sf::st_sfc(sf::st_polygon(), crs = 3857)
  expect_error(
    new_hotspot_results(empty_result, class = "hspt_test"),
    "empty output geometry"
  )
})
