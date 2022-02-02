#' Create either a rectangular or hexagonal two-dimensional grid
#'
#' @param data \code{\link[sf]{sf}} data frame.
#' @param cell_size \code{numeric} value specifying the size of each equally
#'   spaced grid cell, using the same units (metres, degrees, etc.) as used in
#'   the \code{sf} data frame given in the \code{data} argument. If this
#'   argument is \code{NULL} (the default), the cell size will be calculated
#'   automatically (see Details).
#' @param grid_type \code{character} specifying whether the grid should be made
#'   up of squares (\code{"rect"}, the default) or hexagons (\code{"hex"}).
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{TRUE}.
#' @param ... Further arguments passed to \code{link[sf]{st_make_grid}}.
#'
#' @return a simple features tibble containing polygons representing grid cells.
#'
#' @noRd

create_grid <- function (
  data,
  cell_size = NULL,
  grid_type = "rect",
  quiet = TRUE,
  ...
) {

  # Check inputs
  if (!inherits(data, "sf"))
    rlang::abort("`data` must be an SF object")
  if (!rlang::is_null(cell_size) & !rlang::is_double(cell_size, n = 1))
    rlang::abort("`cell_size` must be `NULL` or a single numeric value")
  if (!rlang::is_null(cell_size)) {
    if (cell_size <= 0) rlang::abort("`cell_size` must be greater than zero")
  }
  grid_type <- rlang::arg_match(grid_type, c("rect", "hex"))

  # Set cell size if not specified
  if (rlang::is_null(cell_size))
    cell_size <- set_cell_size(data, round = TRUE, quiet = quiet)

  # Create grid
  grid <- sf::st_make_grid(
    data,
    cellsize = cell_size,
    square = grid_type == "rect",
    ...
  )

  # Return result
  sf::st_as_sf(
    tibble::tibble(geometry = grid),
    crs = sf::st_crs(data),
    sf_column_name = "geometry"
  )

}
