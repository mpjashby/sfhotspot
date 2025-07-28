# Create input dataset that covers a large area
large_area <- sf::st_sf(
  geometry = sf::st_sfc(list(sf::st_point(c(-1, 0)), sf::st_point(c(50, 0)))),
  crs = "EPSG:4326",
  sf_column_name = "geometry"
)

# Create input dataset with missing CRS
robbery_nocrs <- memphis_robberies_jan
sf::st_crs(robbery_nocrs) <- NA



# CHECK INPUTS -----------------------------------------------------------------

# Note checking of some inputs is tested in the test file for `validate_sf()`

test_that("error if CRS is missing", {
  expect_error(st_transform_auto(robbery_nocrs), "must have a specified")
})

test_that("error if `check` is not TRUE/FALSE", {
  expect_error(
    st_transform_auto(memphis_robberies_jan, check = "blah"),
    "must be either"
  )
  expect_error(
    st_transform_auto(memphis_robberies_jan, check = 1L),
    "must be either"
  )
  expect_error(
    st_transform_auto(memphis_robberies_jan, check = 1:2),
    "must be either"
  )
})

test_that("error if data to be checked are not lon/lat", {
  expect_error(
    check_utm_data(sf::st_transform(memphis_robberies_jan, "EPSG:27700")),
    "must be lon/lat pairs"
  )
})

test_that("error if `code` to be checked isn't valid", {
  expect_error(
    check_utm_data(memphis_robberies_jan, code = 27700),
    "CRS code for a UTM or UPS zone"
  )
})

test_that("warning if data covers multiple UTM zones", {
  expect_warning(
    st_transform_auto(large_area),
    "centroids more than 1 degree outside"
  )
})


# CHECK OUTPUTS ----------------------------------------------------------------

test_that("function performs expected transformation", {

  # From lat/lon
  expect_equal(
    sf::st_crs(st_transform_auto(memphis_robberies_jan, quiet = TRUE))$epsg,
    32616
  )

  # To lat/lon
  expect_equal(
    sf::st_crs(
      st_transform_auto(
        sf::st_transform(memphis_robberies_jan, "EPSG:32616"), 
        quiet = TRUE
      )
    )$epsg,
    4326
  )

  # Zones in the far north of Europe where normal rules are amended
  expect_equal(
    sf::st_crs(
      st_transform_auto(
        sf::st_sf(
          geometry = sf::st_sfc(
            list(sf::st_point(c(2, 75)), sf::st_point(c(8, 75)))
          ), 
          crs = "EPSG:4326", 
          sf_column_name = "geometry"
        ), 
        quiet = TRUE
      )
    )$epsg,
    32631
  )
  expect_equal(
    sf::st_crs(
      st_transform_auto(
        sf::st_sf(
          geometry = sf::st_sfc(
            list(sf::st_point(c(10, 75)), sf::st_point(c(20, 75)))
          ), 
          crs = "EPSG:4326", 
          sf_column_name = "geometry"
        ), 
        quiet = TRUE
      )
    )$epsg,
    32633
  )
  expect_equal(
    sf::st_crs(
      st_transform_auto(
        sf::st_sf(
          geometry = sf::st_sfc(
            list(sf::st_point(c(22, 75)), sf::st_point(c(32, 75)))
          ), 
          crs = "EPSG:4326", 
          sf_column_name = "geometry"
        ), 
        quiet = TRUE
      )
    )$epsg,
    32635
  )
  expect_equal(
    sf::st_crs(
      st_transform_auto(
        sf::st_sf(
          geometry = sf::st_sfc(
            list(sf::st_point(c(34, 75)), sf::st_point(c(41, 75)))
          ), 
          crs = "EPSG:4326", 
          sf_column_name = "geometry"
        ), 
        quiet = TRUE
      )
    )$epsg,
    32637
  )

  # Western Norway
  expect_equal(
    sf::st_crs(
      st_transform_auto(
        sf::st_sf(
          geometry = sf::st_sfc(
            list(sf::st_point(c(4, 57)), sf::st_point(c(11, 63)))
          ), 
          crs = "EPSG:4326", 
          sf_column_name = "geometry"
        ), 
        quiet = TRUE
      )
    )$epsg,
    32632
  )

})

test_that("no unexpected warnings", {

  # North Pole
  expect_no_warning(
    st_transform_auto(
      sf::st_sf(
        geometry = sf::st_sfc(
          list(sf::st_point(c(0, 90)), sf::st_point(c(20, 88)))
        ), 
        crs = "EPSG:4326", 
        sf_column_name = "geometry"
      )
    )
  )

  # Non-polar regions
  expect_no_warning(
    st_transform_auto(
      sf::st_sf(
        geometry = sf::st_sfc(
          list(sf::st_point(c(0, 1)), sf::st_point(c(1, 0)))
        ), 
        crs = "EPSG:4326", 
        sf_column_name = "geometry"
      )
    )
  )
  expect_no_warning(
    st_transform_auto(
      sf::st_sf(
        geometry = sf::st_sfc(
          list(sf::st_point(c(0, -1)), sf::st_point(c(-1, 0)))
        ), 
        crs = "EPSG:4326", 
        sf_column_name = "geometry"
      )
    )
  )

  # South Pole
  expect_no_warning(
    st_transform_auto(
      sf::st_sf(
        geometry = sf::st_sfc(
          list(sf::st_point(c(0, -90)), sf::st_point(c(20, -88)))
        ), 
        crs = "EPSG:4326", 
        sf_column_name = "geometry"
      )
    )
  )

})

test_that("function produces expected messages", {

  # North Pole
  expect_message(
    st_transform_auto(
      sf::st_sf(
        geometry = sf::st_sfc(
          list(sf::st_point(c(0, 90)), sf::st_point(c(20, 88)))
        ), 
        crs = "EPSG:4326", 
        sf_column_name = "geometry"
      )
    ),
    "UPS North"
  )

  # Non-polar regions
  expect_message(
    st_transform_auto(
      sf::st_sf(
        geometry = sf::st_sfc(
          list(sf::st_point(c(0, 1)), sf::st_point(c(1, 0)))
        ), 
        crs = "EPSG:4326", 
        sf_column_name = "geometry"
      )
    ),
    "UTM zone 31N"
  )

  # South Pole
  expect_message(
    st_transform_auto(
      sf::st_sf(
        geometry = sf::st_sfc(
          list(sf::st_point(c(0, -90)), sf::st_point(c(20, -88)))
        ), 
        crs = "EPSG:4326", 
        sf_column_name = "geometry"
      )
    ),
    "UPS South"
  )

})

test_that("function produces expected warnings", {
  expect_warning(
    st_transform_auto(
      sf::st_sf(
        geometry = sf::st_sfc(
          list(sf::st_point(c(0, 90)), sf::st_point(c(10, 50)))
        ), 
        crs = "EPSG:4326", 
        sf_column_name = "geometry"
      )
    ),
    "have centroids more than 1 degree"
  )
})
