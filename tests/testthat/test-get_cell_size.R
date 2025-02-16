data_sf <- sf::st_transform(head(memphis_robberies, 100), "EPSG:2843")
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))



# CHECK INPUTS -----------------------------------------------------------------


## Errors ----

test_that("error if `grid` is not an SF object containing polygons", {
  expect_error(get_cell_size(data_df), "must be an SF object")
  expect_error(get_cell_size(data_sf), "SF object containing polygons")
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("correct value returned", {
  expect_equal(get_cell_size(create_grid(data_sf, cell_size = 1000)), 1000)
})
