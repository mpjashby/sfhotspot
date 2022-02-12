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
#' \donttest{
#' hotspot_count(memphis_robberies_jan)
#' }
#'
#' # Transform data to UTM zone 15N so that cell_size and bandwidth can be set
#' # in metres
#' library(sf)
#' memphis_robberies_utm <- st_transform(memphis_robberies_jan, 32615)
#'
#' # Manually set grid-cell size in metres, since the `memphis_robberies_utm`
#' # dataset uses a co-ordinate reference system (UTM zone 15 north) that is
#' # specified in metres
#' \donttest{
#' hotspot_count(memphis_robberies_utm, cell_size = 200)
#' }
#'
#' @export

hotspot_count <- function(
  data,
  cell_size = NULL,
  grid_type = "rect",
  quiet = FALSE
) {

  # Check inputs that are not checked in a helper function
  if (!inherits(data, "sf"))
    rlang::abort("`data` must be an SF object")
  if (any(!sf::st_is(data, "POINT")))
    rlang::abort("`data` must be an SF object containing points")
  if (!rlang::is_logical(quiet))
    rlang::abort("`quiet` must be one of `TRUE` or `FALSE`")

  # Create grid
  grid <- create_grid(
    data,
    cell_size = cell_size,
    grid_type = grid_type,
    quiet = quiet
  )

  # Count points
  counts <- count_points_in_polygons(data, grid)

  # Return result
  counts

}
