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

## Messages ----

test_that("function produces message summarising rows removed", {
  expect_message(hotspot_clip(data_sf, boundary_sf), "^Removed 185 rows")
})
