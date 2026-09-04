data_sf <- memphis_robberies_jan
boundary_sf <- memphis_precincts[1, ]
result <- hotspot_clip(data_sf, boundary_sf, quiet = TRUE)
polygon_data_sf <- hotspot_count(data_sf, quiet = TRUE)
polygon_result <- hotspot_clip(
  polygon_data_sf,
  memphis_precincts,
  quiet = TRUE
)


# CHECK INPUTS -----------------------------------------------------------------

# Note that common inputs are tested in `validate_inputs()` and tested in the
# corresponding test file

test_that("boundary must contain polygon geometry", {
  linestring_boundary <- suppressWarnings(
    sf::st_cast(boundary_sf, "LINESTRING")
  )
  expect_error(
    hotspot_clip(
      data_sf,
      linestring_boundary,
      quiet = TRUE
    ),
    "boundary.*must be an SF object with POLYGON or MULTIPOLYGON geometry"
  )
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("output is an SF tibble", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
})

test_that("output object has same column names as input", {
  expect_equal(names(data_sf), names(result))
})

test_that("output has correct number of rows", {
  expect_equal(nrow(result), 21)
})

test_that("polygon data can be clipped (#65)", {
  expect_s3_class(polygon_result, "sf")
  expect_equal(names(polygon_result), names(polygon_data_sf))
  expect_true(nrow(polygon_result) < nrow(polygon_data_sf))
  expect_true(all(sf::st_is(polygon_result, c("POLYGON", "MULTIPOLYGON"))))
})

test_that("data can contain any geometry type (#78)", {
  geometries <- list(
    sf::st_point(c(0.5, 0.5)),
    sf::st_multipoint(matrix(c(0.25, 0.25, 0.75, 0.75), ncol = 2, byrow = TRUE)),
    sf::st_linestring(matrix(c(0.25, 0.25, 0.75, 0.75), ncol = 2, byrow = TRUE)),
    sf::st_multilinestring(list(
      matrix(c(0.25, 0.25, 0.75, 0.75), ncol = 2, byrow = TRUE)
    )),
    sf::st_polygon(list(matrix(
      c(0.25, 0.25, 0.75, 0.25, 0.75, 0.75, 0.25, 0.25),
      ncol = 2,
      byrow = TRUE
    ))),
    sf::st_multipolygon(list(list(matrix(
      c(0.25, 0.25, 0.75, 0.25, 0.75, 0.75, 0.25, 0.25),
      ncol = 2,
      byrow = TRUE
    )))),
    sf::st_geometrycollection(list(
      sf::st_point(c(0.5, 0.5)),
      sf::st_linestring(matrix(c(0.25, 0.25, 0.75, 0.75), ncol = 2, byrow = TRUE))
    ))
  )
  boundary <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))))

  for (geometry in geometries) {
    data <- sf::st_as_sf(sf::st_sfc(geometry))
    expect_no_error(hotspot_clip(data, boundary, quiet = TRUE))
  }
})

test_that("package-specific result classes are preserved (#71)", {
  result_classes <- c("hspt_n", "hspt_k", "hspt_c", "hspt_d", "hspt_g")
  base_classes <- setdiff(class(polygon_data_sf), result_classes)

  for (result_class in result_classes) {
    classed_data <- structure(
      polygon_data_sf,
      class = c(result_class, base_classes)
    )
    classed_result <- hotspot_clip(
      classed_data,
      memphis_precincts,
      quiet = TRUE
    )
    expect_s3_class(classed_result, result_class)
  }

  expect_s3_class(autoplot(polygon_result), "ggplot")
})

test_that("unrelated classes are not preserved (#71)", {
  unclassed_data <- structure(
    polygon_data_sf,
    class = c("unrelated_class", setdiff(class(polygon_data_sf), "hspt_n"))
  )
  unclassed_result <- hotspot_clip(
    unclassed_data,
    memphis_precincts,
    quiet = TRUE
  )

  expect_false(inherits(unclassed_result, "unrelated_class"))
})

## Messages ----

test_that("function produces message summarising rows removed", {
  expect_message(hotspot_clip(data_sf, boundary_sf), "^Removed 185 rows")
})

test_that("function produces no message if no rows are removed (#66)", {
  expect_no_message(hotspot_clip(data_sf, memphis_precincts))
})


## Warnings ----

test_that("expected st_intersection warning is suppressed (#79)", {
  expect_no_warning(hotspot_clip(data_sf, boundary_sf, quiet = TRUE))
})

test_that("other st_intersection warnings are not suppressed (#79)", {
  st_intersection <- sf::st_intersection
  local_mocked_bindings(
    st_intersection = function(...) {
      warning("A relevant warning from st_intersection()")
      st_intersection(...)
    },
    .package = "sf"
  )

  expect_warning(
    hotspot_clip(data_sf, boundary_sf, quiet = TRUE),
    "A relevant warning from st_intersection\\(\\)"
  )
})

test_that("lower-dimensional output produces a warning (#78)", {
  data <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))))
  boundary <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(matrix(
    c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0),
    ncol = 2,
    byrow = TRUE
  )))))

  expect_warning(
    result <- hotspot_clip(data, boundary, quiet = TRUE),
    "reduced the geometry dimension of 1 output feature"
  )
  expect_true(sf::st_is(result, "LINESTRING"))
})

test_that("single/multi type changes do not produce a warning (#78)", {
  polygon <- sf::st_polygon(list(matrix(
    c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
    ncol = 2,
    byrow = TRUE
  )))
  data <- sf::st_as_sf(sf::st_sfc(sf::st_multipolygon(list(polygon))))
  boundary <- sf::st_as_sf(sf::st_sfc(polygon))

  expect_no_warning(hotspot_clip(data, boundary, quiet = TRUE))
})
