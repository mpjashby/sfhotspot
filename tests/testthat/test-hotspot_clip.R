data_sf <- memphis_robberies_jan
boundary_sf <- memphis_precincts[1, ]
result <- hotspot_clip(data_sf, boundary_sf, quiet = TRUE)


# CHECK INPUTS -----------------------------------------------------------------

# Note that common inputs are tested in `validate_inputs()` and tested in the
# corresponding test file



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


## Messages ----

test_that("function produces message summarising rows removed", {
  expect_message(hotspot_clip(data_sf, boundary_sf), "^Removed 185 rows")
})
