data_sf <- head(memphis_robberies, 1000)
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))
data_sf$no_groups <- NA_real_
data_sf$bad_groups <- data_sf$date < min(data_sf$date)
data_sf$multi_groups <- cut(data_sf$date, breaks = 5)
data_sf$good_groups <- as.character(cut(data_sf$date, breaks = 2))

# To speed up the checking process, run the function with arguments that should
# not produce any errors or warnings
result <- hotspot_change(data = data_sf, quiet = TRUE)



# CHECK INPUTS -----------------------------------------------------------------

# Note that common inputs are tested in `validate_inputs()` and tested in the
# corresponding test file

test_that("error if inputs have wrong type", {
  expect_error(hotspot_change(data_df))
  expect_error(hotspot_change(data_sf, time = 1L))
  expect_error(hotspot_change(data_sf, boundary = "blah"))
  expect_error(hotspot_change(data_sf, groups = 1L))
})

test_that("error if inputs have the wrong length", {
  expect_error(hotspot_change(data_sf, boundary = c(Sys.Date(), Sys.time())))
})

test_that("error if specified time/groups column not present", {
  expect_error(
    hotspot_change(data_sf, time = blah),
    "NULL or the name of a column"
  )
  expect_error(
    hotspot_change(data_sf, groups = blah),
    "NULL or the name of a column"
  )
})

test_that("error if specified time column has wrong type", {
  expect_error(
    hotspot_change(data_sf, time = offense_type),
    "name of a column of type"
  )
})

test_that("error if there are no date/time columns", {
  expect_error(
    hotspot_change(data_sf[, "geometry"]),
    "No columns in `data` contain <Date/POSIXt> values"
  )
})

test_that("error if there are multiple date/time columns", {
  data_sf_multi <- data_sf
  data_sf_multi$date2 <- data_sf_multi$date
  expect_error(hotspot_change(data_sf_multi))
})

test_that("error if `groups` does not have two values", {
  expect_error(hotspot_change(data_sf, groups = no_groups))
  expect_error(hotspot_change(data_sf, groups = bad_groups))
  expect_error(hotspot_change(data_sf, groups = multi_groups))
})

test_that("error if `boundary` is not within range of data", {
  expect_error(hotspot_change(data_sf, boundary = min(data_sf$date) - 60))
})

test_that("error if no data in one/both groups", {
  expect_error(hotspot_change(data_sf, boundary = min(data_sf$date)))
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("output is an SF tibble with class hspt_n", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "hspt_d")
})

test_that("output object has the required column names", {
  expect_equal(names(result), c("n_before", "n_after", "change", "geometry"))
})

test_that("columns in output have the required types", {
  expect_type(result$n_before, "double")
  expect_type(result$n_after, "double")
  expect_type(result$change, "double")
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})


## Messages ----

test_that("boundary point is reported if not specified", {
  expect_message(hotspot_change(data_sf), "Boundary point set as")
})

test_that("summary message if groups specified and not factor", {
  expect_message(
    hotspot_change(data_sf, groups = good_groups),
    "Comparing periods based on values"
  )
})
