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
#' This function uses functions from the \code{SpatialKDE} package to create a
#' regular two-dimensional grid of cells and then calculate the density of
#' points in each cell. The count of points in each cell is also returned.
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

  # Check inputs that are not checked in a helper function
  if (!inherits(data, "sf"))
    rlang::abort("`data` must be an SF object")
  if (any(!sf::st_is(data, "POINT")))
    rlang::abort("`data` must be an SF object containing points")
  if (!rlang::is_logical(quiet))
    rlang::abort("`quiet` must be one of `TRUE` or `FALSE`")

  # Create grid
  grid <- create_grid(data, cell_size = cell_size, grid_type = grid_type)

  # Count points
  counts <- count_points_in_polygons(data, grid)

  # Calculate KDE
  kde_val <- kernel_density(data, grid, bandwidth = bandwidth, quiet = quiet)
  counts$kde <- kde_val$kde_value

  # Return result
  sf::st_as_sf(tibble::as_tibble(counts[, c("n", "kde", "geometry")]))

}
