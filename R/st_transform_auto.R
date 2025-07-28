#' Toggle between lon/lat and UTM co-ordinates
#'
#' It is often useful to convert geographic data to a projected co-ordinate
#' system, e.g. for specifying a buffer distance in metres.
#' \code{st_transform_auto} converts between lon/lat co-ordinates and the UTM
#' or UPS zone for the centroid of the data.
#'
#' @param data An \code{\link[sf]{sf}} object.
#' @param check Should the function check if the chosen UTM zone covers all the
#'   data? If so then a warning is issued if the centroid of any row in the data
#'   is more than 1º outside the area covered by the UTM zone.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{FALSE}.
#'
#' @return SF object transformed to new CRS.
#'
#' @export

st_transform_auto <- function(data, check = TRUE, quiet = FALSE) {

  # Check inputs
  validate_sf(data, quiet = quiet)
  if (rlang::is_empty(sf::st_crs(data, parameters = TRUE))) {
    cli::cli_abort(c(
      "{.var data} must have a specified co-ordinate reference system.",
      "i" = "Check or set CRS with {.fn st_crs}."
    ))
  }
  if (!rlang::is_logical(check, n = 1))
    cli::cli_abort("{.arg check} must be either {.q TRUE} or {.q FALSE}.")

  # Transform data
  if (sf::st_is_longlat(data)) {

    # Extract centroid for dataset
    centroid <- as.data.frame(
      sf::st_coordinates(st_centroid_quietly(sf::st_union(data)))
    )

    # Identify EPSG code for centroid of data
    code <- latlon_to_utm(centroid$X, centroid$Y)

  } else {

    code <- 4326

  }

  # Transform data
  result <- sf::st_transform(data, code)

  # Report result and any issues
  if (!rlang::is_true(quiet)) {
    crs_name <- sf::st_crs(result)$Name
    crs_code <- sf::st_crs(result)$epsg
    crs_unit <- sf::st_crs(result)$units_gdal
    crs_info <- sf::st_crs(result)
    if (is.numeric(crs_code)) crs_code <- paste0("EPSG:", crs_code)
    cli::cli_inform(c(
      "Data transformed to {.val {crs_name}} co-ordinate system.",
      "i" = "CRS code: {.val {crs_code}}.",
      "i" = "Unit of measurement: {crs_unit}."
    ))
  }

  if (!sf::st_is_longlat(result) & check)
    check_utm_data(data, code, quiet = quiet)

  # Return result
  result

}


# Function to find appropriate UTM zone for a single lon/lat location
latlon_to_utm <- function(x, y) {

  # Some of this code adapted from https://gis.stackexchange.com/a/375285
  if (y >= 84) {
    # North Pole
    zone <- 32661
  } else if (y <= -80) {
    # South Pole
    zone <- 32761
  } else if (y > 72) {
    # Zones in northern Europe where the normal rules are amended
    if (x >= 0 & x < 9) {
      zone <- 32631
    } else if (x >= 9 & x < 21) {
      zone <- 32633
    } else if (x >= 21 & x < 33) {
      zone <- 32635
    } else if (x >= 33 & x < 42) {
      zone <- 32637
    }
  } else if (x >= 3 & x <= 12 & y >= 56 & y < 64) {
    # Extension of zone 32N to cover western Norway
    zone <- 32632
  } else {
    # Everywhere else
    # * + 180 because UTM zone 1 starts at the international date line
    # * / 6 because UTM zones are 6 degrees wide
    # * floor() to find the left-most point in a UTM zone
    # * + 1 because there is no UTM zone 0
    # * + 100 if y < 0 because UTM codes for the Southern Hemisphere are in a
    #   series of numbers 100 greater than those for the Northern Hemisphere
    zone <- 32600 + floor((x + 180) / 6) + 1 + ifelse(y < 0, 100, 0)
  }

  zone

}


# Function to give a warning if any rows in the data have co-ordinates more than
# one degree outside a specified UTM zone
check_utm_data <- function(data, code, quiet = TRUE) {

  # Set buffer size in decimal degrees. This represents how far outside a UTM
  # zone points can be before they are considered problematic.
  buffer_size <- 1

  # Check inputs
  validate_sf(
    data,
    allow_null = TRUE,
    call = rlang::caller_env(),
    # `quiet` should be TRUE because the relevant warnings will already have
    # been issued in the function calling this one
    quiet = TRUE
  )
  if (!sf::st_is_longlat(data))
    cli::cli_abort("Co-ordinates in {.var data} must be lon/lat pairs.")
  if (!code %in% c(32600:32661, 32700:32761))
    cli::cli_abort(
      "{.arg code} must be a valid CRS code for a UTM or UPS zone."
    )

  bbox <- sf::st_bbox(data)

  if (code == 32661) {

    # North Pole
    data_lat <- sf::st_coordinates(st_centroid_quietly(data))[, 2]
    prop_outside <- sum(data_lat < 84 - buffer_size) / nrow(data)

  } else if (code == 32761) {

    # South Pole
    data_lat <- sf::st_coordinates(st_centroid_quietly(data))[, 2]
    prop_outside <- sum(data_lat > -80 + buffer_size) / nrow(data)

  } else {

    if (code > 32700 & code <= 32760) {
      # Southern hemisphere
      xmin <- ((code - 32700 - 1) * 6) - 180
      ymax <- -80
    } else if (code > 32600 & code <= 32660) {
      # Northern hemisphere
      xmin <- ((code - 32600 - 1) * 6) - 180
      ymax <- 84
    }

    zone_area <- sf::st_buffer(
      sf::st_sfc(
        sf::st_polygon(list(rbind(
          c(xmin, 0), # bottom left
          c(xmin + 6, 0), # bottom right
          c(xmin + 6, ymax), # top right
          c(xmin, ymax), # top left
          c(xmin, 0) # bottom left again
        ))),
        crs = 4326
      ),
      dist = buffer_size)

    withCallingHandlers({
      data_in_zone <- sf::st_intersection(st_centroid_quietly(data), zone_area)
    }, warning = function(w) {
      if (grepl("attribute variables are assumed", conditionMessage(w)))
        invokeRestart("muffleWarning")
    })

    prop_outside <- (nrow(data) - nrow(data_in_zone)) / nrow(data)

  }

  # If any problems have been identified, report them here
  if (prop_outside > 0 & rlang::is_false(quiet)) {

    if (prop_outside < 0.001) {
      msg <- "A small proportion (<0.1%)"
    } else if (prop_outside > 0.1) {
      msg <- sprintf("%1.0f%%", prop_outside * 100)
    } else {
      msg <- sprintf("%1.1f%%", prop_outside * 100)
    }

    crs_name <- sf::st_crs(code)$Name
    cli::cli_warn(c(
      "Some transformed data may be distorted.",
      "i" = paste0(
        "{msg} of rows in {.var data} have centroids more than {buffer_size} ",
        "degree{?s} outside area for which the {.val {crs_name}} CRS was ",
        "designed."
      ),
      "i" = "Consider transforming manually to a different CRS."
    ))

  }

  invisible(NULL)

}

# Function to allow st_centroid() to run without always producing the same
# warning
st_centroid_quietly <- function(...) {
  withCallingHandlers({
    sf::st_centroid(...)
  }, warning = function(w) {
    if (grepl("st_centroid assumes", conditionMessage(w)))
      invokeRestart("muffleWarning")
  })
}
