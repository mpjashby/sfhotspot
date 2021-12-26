#' Estimate two-dimensional kernel density of points on a regular grid
#'
#' @param data \code{\link[sf]{sf}} data frame containing points.
#' @param cell_size \code{numeric} value specifying the size of each equally
#'   spaced grid cell, using the same units (metres, degrees, etc.) as used in
#'   the \code{sf} data frame given in the \code{data} argument. If this
#'   argument is \code{NULL} (the default), the cell size will be calculated
#'   automatically (see Details).
#' @param grid_type \code{character} specifying whether the grid should be made
#'   up of squares (\code{"rect"}, the default) or hexagons (\code{"hex"}).
#' @param bandwidth \code{numeric} value specifying the bandwidth to be used in
#'   calculating the kernel density estimates. If this argument is \code{NULL}
#'   (the default), the bandwidth will be specified automatically using the mean
#'   result of \code{\link[MASS]{bandwidth.nrd}} called on the \code{x} and
#'   \code{y} co-ordinates separately.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{FALSE}.
#' @param ... Further arguments passed to \code{\link[SpatialKDE]{kde}}.
#' @return An \code{\link[sf]{sf}} tibble of regular grid cells with
#'   corresponding point counts and kernel density estimates for each cell.
#'
#'   The output from this function can be plotted in the same way as for other
#'   SF objects, for which see \code{vignette("sf5", package = "sf")}.
#'
#' @details
#'
#' This function uses functions from the \code{\link{SpatialKDE}} package to
#' create a regular two-dimensional grid of cells and then calculate the density
#' of points in each cell. The count of points in each cell is also returned.
#'
#' ## Coverage of the output data
#'
#' The grid produced by this function covers the convex hull of the input data
#' layer. This means the result may include KDE values for cells that are
#' outside the area for which data were provided, which could be misleading. To
#' handle this, consider cropping the output layer to the area for which data
#' are available. For example, if you only have crime data for a particular
#' district, crop the output dataset to the district boundary using
#' \code{\link[sf]{st_intersection}}.
#'
#' ## Automatic cell-size selection
#'
#' If no cell size is given then the cell size will be set so that there are 50
#' cells on the shorter side of the grid. If the `data` SF object is projected
#' in metres or feet, the number of cells will be adjusted upwards so that the
#' cell size is a multiple of 100.
#'
#' @examples
#' library(sf)
#'
#' # Transform data to UTM zone 15N so that cell_size and bandwidth can be set
#' # in metres
#' memphis_robberies_utm <- st_transform(memphis_robberies, 32615)
#'
#' # Automatically set grid-cell size, bandwidth and neighbour distance
#' hotspot_kde(memphis_robberies_utm)
#'
#' # Manually set grid-cell size and bandwidth in metres, since the
#' # `memphis_robberies_utm` dataset uses a co-ordinate reference system (UTM
#' # zone 15 north) that is specified in metres
#' hotspot_kde(memphis_robberies_utm, cell_size = 200, bandwidth = 1000)
#'
#' @export

hotspot_kde <- function (
  data,
  cell_size = NULL,
  grid_type = "rect",
  bandwidth = NULL,
  quiet = FALSE,
  ...
) {

  # Check inputs
  if (!inherits(data, "sf"))
    rlang::abort("`data` must be an SF object")
  if (any(!sf::st_is(data, "POINT")))
    rlang::abort("`data` must be an SF object containing points")
  if (!rlang::is_null(cell_size) & !rlang::is_double(cell_size, n = 1))
    rlang::abort("`cell_size` must be `NULL` or a single numeric value")
  if (!rlang::is_null(cell_size)) {
    if (cell_size <= 0) rlang::abort("`cell_size` must be greater than zero")
  }
  if (!rlang::is_null(bandwidth) & !rlang::is_double(bandwidth, n = 1))
    rlang::abort("`bandwidth` must be NULL or a single numeric value")
  if (!rlang::is_null(bandwidth)) {
    if (bandwidth <= 0) rlang::abort("`bandwidth` must be greater than zero")
  }
  grid_type <- rlang::arg_match(grid_type, c("rect", "hex"))
  if (!rlang::is_logical(quiet))
    rlang::abort("`quiet` must be one of `TRUE` or `FALSE`")
  if (sf::st_is_longlat(data)) {
    rlang::abort(c(
      "KDE values cannot be calculated for lon/lat data",
      "i" = "Transform `data` to use a projected CRS"
    ))
  }

  # Find spatial unit
  unit <- sf::st_crs(data, parameters = TRUE)$units_gdal
  unit_pl <- ifelse(
    unit %in% c("metre", "meter"),
    "metres",
    ifelse(
      unit %in% c("foot", "US survey foot"),
      "feet",
      ifelse(unit == "degree", "degrees", paste("(unit =", unit))
    )
  )

  # Set cell size if not specified
  if (rlang::is_null(cell_size)) {

    bbox <- sf::st_bbox(data)
    side_length <- min(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin)

    if (unit %in% c("metre", "meter", "foot", "US survey foot")) {

      # If the units are metres or feet, round the cell size so it is a round
      # number of 100 metres/feet
      cell_size <- floor((side_length / 50) / 100) * 100

    } else {

      # Otherwise, just set the cell size so there are 50 cells on the shortest
      # size
      cell_size <- side_length / 50

    }

    if (rlang::is_false(quiet)) {
      rlang::inform(c("i" = paste(
        "Cell size set to", format(cell_size, big.mark = ","), unit_pl,
        "automatically"
      )))
    }

  }

  # Set bandwidth if not specified
  if (rlang::is_null(bandwidth)) {
    bandwidth <- bandwidth_nrd_sf(data)
    if (rlang::is_false(quiet)) {
      rlang::inform(c("i" = paste(
        "Bandwidth set to", format(bandwidth, big.mark = ","), unit_pl,
        "automatically based on rule of thumb"
      )))
    }
  }

  # Create grid
  if (grid_type == "hex") {
    grid <- SpatialKDE::create_grid_hexagonal(data, cell_size = cell_size)
  } else {
    grid <- SpatialKDE::create_grid_rectangular(data, cell_size = cell_size)
  }
  grid$id <- 1:nrow(grid)

  # Count points
  # Join the grid cell IDs to the points layer and, count how many points have
  # the unique ID of each grid cell, join the counts back to the grid and
  # replace missing values (the consequence of cells not matched in the second
  # join) with zeros
  ids <- sf::st_drop_geometry(sf::st_join(data, grid))
  counts <- stats::aggregate(ids$offense_type, list("id" = ids$id), FUN = length)
  counts <- merge(counts, grid, by = "id", all.y = TRUE)
  counts$n <- ifelse(is.na(counts$x), 0, counts$x)
  counts <- sf::st_as_sf(counts)

  # Calculate KDE
  if (rlang::is_true(quiet)) {
    kde_val <- suppressMessages(
      SpatialKDE::kde(data, band_width = bandwidth, grid = grid)
    )
  } else {
    kde_val <- SpatialKDE::kde(data, band_width = bandwidth, grid = grid)
  }

  # Join results
  result <- counts
  result$kde <- kde_val$kde_value

  # Return result
  sf::st_as_sf(tibble::as_tibble(result[, c("id", "n", "kde", "geometry")]))

}
