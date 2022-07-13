set.seed(123)

data_sf <- memphis_robberies
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))
result_count <- hotspot_count(data_sf, cell_size = 0.01, quiet = TRUE)
result_kde <- hotspot_kde(
  sf::st_transform(data_sf, 32616),
  cell_size = 1000,
  quiet = TRUE
)
result_classify <- hotspot_classify(
  memphis_robberies,
  cell_size = 0.01,
  quiet = TRUE
)
result_change <- hotspot_change(data_sf, quiet = TRUE)



# TEST INPUTS ------------------------------------------------------------------


# Errors ----

test_that("error if `object` is not an SF object", {
  expect_error(autoplot(sf::st_drop_geometry(result_count)))
  expect_error(autolayer(sf::st_drop_geometry(result_count)))
  expect_error(autoplot(sf::st_drop_geometry(result_kde)))
  expect_error(autolayer(sf::st_drop_geometry(result_kde)))
  expect_error(autoplot(sf::st_drop_geometry(result_classify)))
  expect_error(autolayer(sf::st_drop_geometry(result_classify)))
  expect_error(autoplot(sf::st_drop_geometry(result_change)))
  expect_error(autolayer(sf::st_drop_geometry(result_change)))
})

test_that("error if `object` does not contain the required columns", {
  expect_error(autoplot(result_count[, "geometry"]))
  expect_error(autolayer(result_count[, "geometry"]))
  expect_error(autoplot(result_kde[, "geometry"]))
  expect_error(autolayer(result_kde[, "geometry"]))
  expect_error(autoplot(result_classify[, "geometry"]))
  expect_error(autolayer(result_classify[, "geometry"]))
  expect_error(autoplot(result_change[, "geometry"]))
  expect_error(autolayer(result_change[, "geometry"]))
})

test_that("error if required column does not have correct type", {
  result_count$n <- as.character(result_count$n)
  expect_error(autoplot(result_count))
  expect_error(autolayer(result_count))
  result_kde$kde <- as.character(result_kde$kde)
  expect_error(autoplot(result_kde))
  expect_error(autolayer(result_kde))
  result_change$change <- as.character(result_change$change)
  expect_error(autoplot(result_change))
  expect_error(autolayer(result_change))
})



# TEST OUTPUTS -----------------------------------------------------------------

test_that("output has correct class", {
  expect_s3_class(autoplot(result_count), "ggplot")
  expect_s3_class(autoplot(result_kde), "ggplot")
  expect_s3_class(autoplot(result_classify), "ggplot")
  expect_s3_class(autoplot(result_change), "ggplot")
})
