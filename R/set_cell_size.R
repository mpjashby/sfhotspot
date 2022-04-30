#' Automatically determine a suitable cell size for a two-dimensional grid
#'
#' This function suggests a cell size for a two-dimensional regular grid of
#' cells to cover the convex hull of one or more point locations.
#'
#' @param data \code{\link[sf]{sf}} data frame.
#' @param round If the \code{data} SF object is projected in metres or feet,
#'   should the number of cells will be adjusted upwards so that the cell size
#'   is a multiple of 100? The default is \code{TRUE}.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{TRUE}.
#'
#' @return A single numeric value representing the recommended cell size, using
#'   the same spatial units as specified in the co-ordinate reference system of
#'   the supplied \code{data} object.
#'
#' @noRd
#'
#' @details
#'
#' The recommended cell size will be 1/50th of the length of the shorter side of
#' the grid, except if \code{round = TRUE} and the \code{data} SF object is
#' projected in metres or feet. In that case the number of cells will be
#' adjusted upwards so that the cell size is a multiple of 100.

set_cell_size <- function(data, round = TRUE, quiet = TRUE) {

  # Check inputs
  if (!inherits(data, "sf"))
    rlang::abort("`data` must be an SF object")
  if (!rlang::is_logical(round))
    rlang::abort("`round` must be `TRUE` or `FALSE`")
  if (!rlang::is_logical(quiet))
    rlang::abort("`quiet` must be `TRUE` or `FALSE`")

  # Find spatial unit
  unit <- sf::st_crs(data, parameters = TRUE)$units_gdal

  # Replace empty unit to prevent the error described at
  # https://github.com/mpjashby/sfhotspot/issues/9
  if (rlang::is_empty(unit)) unit <- "unknown"

  # Find plural form of unit
  unit_pl <- ifelse(
    unit %in% c("metre", "meter"),
    "metres",
    ifelse(
      unit %in% c("foot", "US survey foot"),
      "feet",
      ifelse(unit == "degree", "degrees", paste0("(unit = ", unit, ")"))
    )
  )

  # Calculate cell size
  bbox <- sf::st_bbox(data)
  side_length <- min(bbox$xmax - bbox$xmin, bbox$ymax - bbox$ymin)

  if (
    unit %in% c("metre", "meter", "foot", "US survey foot") &
    rlang::is_true(round)
  ) {

    # If the units are metres or feet, round the cell size so it is a round
    # number of 100 metres/feet
    cell_size <- floor((side_length / 50) / 100) * 100

  } else {

    # Otherwise, just set the cell size so there are 50 cells on the shortest
    # size
    cell_size <- side_length / 50

  }

  if (rlang::is_false(quiet)) {
    rlang::inform(c(paste(
      "Cell size set to", format(cell_size, big.mark = ","), unit_pl,
      "automatically"
    )))
  }

  cell_size

}
