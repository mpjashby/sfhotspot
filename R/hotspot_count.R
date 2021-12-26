#' Count points in cells in a two-dimensional grid
#'
#' @param data \code{\link[sf]{sf}} data frame containing points.
#' @param cell_size \code{numeric} value specifying the size of each equally
#'   spaced grid cell, using the same units (metres, degrees, etc.) as used in
#'   the \code{sf} data frame given in the \code{data} argument. If this
#'   argument is \code{NULL} (the default), the cell size will be calculated
#'   automatically (see Details).
#' @param grid_type \code{character} specifying whether the grid should be made
#'   up of squares (\code{"rect"}, the default) or hexagons (\code{"hex"}).
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{FALSE}.
#' @return An \code{\link[sf]{sf}} tibble of regular grid cells with
#'   corresponding point counts for each cell.
#'
#'   The output from this function can be plotted in the same way as for other
#'   SF objects, for which see \code{vignette("sf5", package = "sf")}.
#'
#' @details
#'
#' This function counts the number of points in each cell in a regular grid.
#'
#' ## Automatic cell-size selection
#'
#' If no cell size is given then the cell size will be set so that there are 50
#' cells on the shorter side of the grid. If the `data` SF object is projected
#' in metres or feet, the number of cells will be adjusted upwards so that the
#' cell size is a multiple of 100.
#'
#' @examples
#'
#' # Set cell size automatically
#' hotspot_count(memphis_robberies)
#'
#' # Transform data to UTM zone 15N so that cell_size and bandwidth can be set
#' # in metres
#' library(sf)
#' memphis_robberies_utm <- st_transform(memphis_robberies, 32615)
#'
#' # Manually set grid-cell size in metres, since the `memphis_robberies_utm`
#' # dataset uses a co-ordinate reference system (UTM zone 15 north) that is
#' # specified in metres
#' hotspot_count(memphis_robberies_utm, cell_size = 200)
#'
#' @export

hotspot_count <- function (
  data,
  cell_size = NULL,
  grid_type = "rect",
  quiet = FALSE
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
  grid_type <- rlang::arg_match(grid_type, c("rect", "hex"))
  if (!rlang::is_logical(quiet))
    rlang::abort("`quiet` must be one of `TRUE` or `FALSE`")

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

  # Return result
  sf::st_as_sf(tibble::as_tibble(counts[, c("id", "n", "geometry")]))

}
