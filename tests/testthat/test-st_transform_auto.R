# # Create input dataset that covers a large area
# large_area <- sf::st_sf(
#   geometry = sf::st_sfc(list(sf::st_point(c(-1, 0)), sf::st_point(c(50, 0)))),
#   crs = "EPSG:4326",
#   sf_column_name = "geometry"
# )
#
# # Create input dataset with missing CRS
# robbery_nocrs <- memphis_robberies_jan
# sf::st_crs(robbery_nocrs) <- NA
#
#
#
# # CHECK INPUTS -----------------------------------------------------------------
#
# # Note checking of some inputs is tested in the test file for `validate_sf()`
#
# test_that("error if CRS is missing", {
#   expect_error(st_transform_auto(robbery_nocrs), "must have a specified")
# })
#
# test_that("error if `check` is not TRUE/FALSE", {
#   expect_error(
#     st_transform_auto(memphis_robberies_jan, check = "blah"),
#     "must be either"
#   )
#   expect_error(
#     st_transform_auto(memphis_robberies_jan, check = 1L),
#     "must be either"
#   )
#   expect_error(
#     st_transform_auto(memphis_robberies_jan, check = 1:2),
#     "must be either"
#   )
# })
#
# test_that("error if data to be checked are not lon/lat", {
#   expect_error(
#     check_utm_data(sf::st_transform(memphis_robberies_jan, "EPSG:27700")),
#     "must be lon/lat pairs"
#   )
# })
#
# test_that("error if `code` to be checked isn't valid", {
#   expect_error(
#     check_utm_data(memphis_robberies_jan, code = 27700),
#     "CRS code for a UTM or UPS zone"
#   )
# })
#
# test_that("warning if data covers multiple UTM zones", {
#   expect_warning(
#     st_transform_auto(large_area),
#     "centroids more than 1 degree outside"
#   )
# })
#
#
# # CHECK OUTPUTS ----------------------------------------------------------------
#
# test_that("function performs expected transformation", {
#   expect_equal(
#     sf::st_crs(st_transform_auto(memphis_robberies_jan, quiet = TRUE))$epsg,
#     32616
#   )
# })
#
# test_that("function produces expected messages", {
#   expect_message(
#     st_transform_auto(memphis_robberies_jan),
#     'Data transformed to "WGS 84 / UTM zone 16N" co-ordinate system'
#   )
# })
