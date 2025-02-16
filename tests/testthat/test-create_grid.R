data_sf <- head(memphis_robberies, 10)
data_sf_m <- sf::st_transform(data_sf, "EPSG:2843")
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))

result_pts <- create_grid(data = data_sf, cell_size = 0.1)
result_ply <- create_grid(
  data = sf::st_transform(memphis_precincts, "EPSG:2843")
)

nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))



# CHECK INPUTS -----------------------------------------------------------------


## Errors ----

test_that("error if `data` is not an SF object", {
  expect_error(create_grid(data = data_df))
})

test_that("error if `cell_size` is not `NULL` or a single positive number", {
  expect_error(create_grid(data = data_sf, cell_size = character()))
  expect_error(create_grid(data = data_sf, cell_size = 1:2))
  expect_error(create_grid(data = data_sf, cell_size = -1))
  expect_error(create_grid(data = data_sf, cell_size = 0))
})

test_that("error if `grid_type` is not 'rect' or 'hex'", {
  expect_error(create_grid(data = data_sf, grid_type = "triangle"))
})

test_that("error if `quiet` is not `TRUE` or `FALSE`", {
  expect_error(create_grid(data = data_sf, quiet = character()))
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("function produces an SF tibble", {
  expect_s3_class(result_pts, "sf")
  expect_s3_class(
    create_grid(data = data_sf, cell_size = 0.1, grid_type = "hex"),
    "sf"
  )
  expect_s3_class(result_ply, "sf")
  expect_s3_class(result_pts, "tbl_df")
  expect_s3_class(
    create_grid(data = data_sf, cell_size = 0.1, grid_type = "hex"),
    "tbl_df"
  )
  expect_s3_class(result_ply, "tbl_df")
})

test_that("output object has the required column names", {
  expect_equal(names(result_pts), c("geometry"))
  expect_equal(names(result_ply), c("geometry"))
})

test_that("columns in output have the required types", {
  expect_true(sf::st_is(result_pts$geometry[[1]], "POLYGON"))
  expect_true(sf::st_is(result_ply$geometry[[1]], "POLYGON"))
})

test_that("output grid covers input geometry", {
  expect_true(
    sf::st_covers(
      sf::st_union(result_pts),
      sf::st_convex_hull(sf::st_union(data_sf)),
      sparse = FALSE
    )
  )
  expect_true(
    sf::st_covers(
      sf::st_union(result_ply),
      sf::st_union(sf::st_transform(memphis_precincts, "EPSG:2843")),
      sparse = FALSE
    )
  )
})

test_that("no error if non-multipolygon geometry provided (#46)", {
  # Polygon input geometry
  expect_no_error(hotspot_grid(memphis_precincts[1, ], quiet = TRUE))
  # Multipolygon input geometry
  expect_no_error(hotspot_grid(memphis_precincts[2, ], quiet = TRUE))
})


## Messages ----

test_that("produces warning if grid has 100,000+ cells", {
  expect_message(create_grid(data_sf_m, cell_size = 40, quiet = FALSE))
})
