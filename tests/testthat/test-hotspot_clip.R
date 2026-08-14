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

test_that("unsupported geometry produces a useful error", {
  expect_error(
    hotspot_clip(
      sf::st_cast(data_sf, "LINESTRING"),
      boundary_sf,
      quiet = TRUE
    ),
    "must be an SF object with POINT, POLYGON, or MULTIPOLYGON geometry"
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

test_that("package-specific result classes are preserved (#71)", {
  result_classes <- c("hspt_n", "hspt_k", "hspt_c", "hspt_d")
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
