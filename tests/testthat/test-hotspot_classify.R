data_sf <- head(memphis_robberies, 1000)

result <- hotspot_classify(data_sf)

# CHECK INPUTS -----------------------------------------------------------------

test_that("error if `data` is not an SF points", {
  expect_error(hotspot_classify(data = sf::st_drop_geometry(data_sf)))
  expect_error(hotspot_classify(data = sf::st_cast(data_sf, "LINESTRING")))
})

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
  expect_error(hotspot_classify(data_sf, quiet = "foo"))
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
  expect_error(hotspot_classify(data_sf, quiet = "foo"))
})

test_that("error if values are of the correct type/length but are invalid", {
  expect_error(hotspot_classify(data_sf, period = "10 years"))
  expect_error(hotspot_classify(data_sf, start = Sys.Date()))
})



# CHECK OUTPUTS ----------------------------------------------------------------

## Correct outputs ----

test_that("function produces an SF tibble", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
})

test_that("output object has the required column names", {
  expect_equal(names(result), c("hotspot_category", "geometry"))
})

test_that("columns in output have the required types", {
  expect_type(result$hotspot_category, "character")
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})
