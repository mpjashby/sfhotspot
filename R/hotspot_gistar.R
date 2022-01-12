#' Identify significant spatial clusters of points
#'
#' Identify hotspot and coldspot locations, that is cells in a regular grid in
#' which there are more/fewer points than would be expected if the points were
#' distributed randomly.
#'
#' @param data \code{\link[sf]{sf}} data frame containing points.
#' @param cell_size \code{numeric} value specifying the size of each equally
#'   spaced grid cell, using the same units (metres, degrees, etc.) as used in
#'   the \code{sf} data frame given in the \code{data} argument. If this
#'   argument is \code{NULL} (the default), the cell size will be calculated
#'   automatically (see Details).
#' @param grid_type \code{character} specifying whether the grid should be made
#'   up of squares (\code{"rect"}, the default) or hexagons (\code{"hex"}).
#' @param kde \code{TRUE} (the default) or \code{FALSE} indicating whether
#'   kernel density estimates (KDE) should be produced for each grid cell.
#' @param bandwidth \code{numeric} value specifying the bandwidth to be used in
#'   calculating the kernel density estimates. If this argument is \code{NULL}
#'   (the default), the bandwidth will be specified automatically using the mean
#'   result of \code{\link[MASS]{bandwidth.nrd}} called on the \code{x} and
#'   \code{y} co-ordinates separately.
#' @param nb_dist The distance around a cell that contains the neighbours of
#'   that cell, which are used in calculating the statistic. If this argument is
#'   \code{NULL} (the default), \code{nb_dist} is set as \code{cell_size *
#'   sqrt(2)} so that only the cells immediately adjacent to each cell are
#'   treated as being its neighbours.
#' @param include_self Should points in a given cell be counted as well as
#'   counts in neighbouring cells when calculating the values of
#'   \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G^*_i}}
#'   (if \code{include_self = TRUE}, the default) or
#'   \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G_i}}
#'   (if \code{include_self = FALSE}) values? You are unlikely to want to change
#'   the default value.
#' @param p_adjust_method The method to be used to adjust \emph{p}-values for
#'   multiple comparisons. \code{NULL} (the default) uses the default method
#'   used by \code{\link[stats]{p.adjust}}, but any of the character values in
#'   \code{stats::p.adjust.methods} may be specified.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{FALSE}.
#' @param ... Further arguments passed to \code{\link[SpatialKDE]{kde}} or
#'   ignored if \code{kde = FALSE}.
#' @return An \code{\link[sf]{sf}} tibble of regular grid cells with
#'   corresponding point counts,
#'   \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G_i}} or
#'   \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G^*_i}}
#'   values and (optionally) kernel density estimates for each cell. Values
#'   greater than zero indicate more points than would be expected for randomly
#'   distributed points and values less than zero indicate fewer points.
#'   Critical values of
#'   \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G_i}} and
#'   \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G^*_i}}
#'   are given in the manual page for \code{\link[spdep]{localG}}.
#'
#'   The output from this function can be plotted in the same way as for other
#'   SF objects, for which see \code{vignette("sf5", package = "sf")}.
#'
#' @details
#'
#' This function calculates the Getis-Ord
#' \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G^*_i}}
#' (gi-star) or
#' \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G_i}}
#' \eqn{Z}-score statistic for identifying clusters of point locations. The
#' underlying implementation uses the \code{\link[spdep]{localG}} function to
#' calculate the \eqn{Z} scores and then \code{\link[spdep]{p.adjustSP}}
#' function to adjust the corresponding \eqn{p}-values for multiple comparison.
#' The function also returns counts of points ineach cell and (by default but
#' optionally) kernel density estimates using the \code{\link[SpatialKDE]{kde}}
#' function.
#'
#' ## Coverage of the output data
#'
#' The grid produced by this function covers the convex hull of the input data
#' layer. This means the result may include
#' \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G^*_i}} or
#' \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G_i}}
#' values for cells that are outside the area for which data were provided,
#' which could be misleading. To handle this, consider cropping the output layer
#' to the area for which data are available. For example, if you only have crime
#' data for a particular district, crop the output dataset to the district
#' boundary using \code{\link[sf]{st_intersection}}.
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
#' hotspot_gistar(memphis_robberies_utm)
#'
#' # Manually set grid-cell size in metres, since the `memphis_robberies`
#' # dataset uses a co-ordinate reference system (UTM zone 15 north) that is
#' # specified in metres
#' hotspot_gistar(memphis_robberies_utm, cell_size = 200)
#'
#' # Automatically set grid-cell size and bandwidth for lon/lat data, since it
#' # is not intuitive to set these values manually in decimal degrees. To do
#' # this it is necessary to not calculate KDEs due to a limitation in the
#' # underlying function.
#' hotspot_gistar(memphis_robberies, kde = FALSE)
#'
#' @export

hotspot_gistar <- function (
  data,
  cell_size = NULL,
  grid_type = "rect",
  kde = TRUE,
  bandwidth = NULL,
  nb_dist = NULL,
  include_self = TRUE,
  p_adjust_method = NULL,
  quiet = FALSE,
  ...
) {

  # Check inputs that are not checked in a helper function
  if (!inherits(data, "sf"))
    rlang::abort("`data` must be an SF object")
  if (any(!sf::st_is(data, "POINT")))
    rlang::abort("`data` must be an SF object containing points")
  if (!rlang::is_logical(quiet, n = 1))
    rlang::abort("`quiet` must be one of `TRUE` or `FALSE`")
  if (sf::st_is_longlat(data)) {
    if (rlang::is_true(kde)) {
      # `kernel_density()` will throw an error in this case as well, but it is
      # useful to catch it in `hotspot_gistar()` because in `hotspot_gistar()`
      # we can solve the problem by setting `kde = FALSE` whereas the
      # recommendation in the error produced by `kernel_density()` is to
      # transform the data, which may not be necessary
      rlang::abort(c(
        "KDE values cannot be calculated for lon/lat data",
        "i" = "Transform `data` to use a projected CRS or set `kde = FALSE`"
      ))
    } else if (rlang::is_false(quiet)) {
      rlang::inform(c(
        "The co-ordinates in `data` are latitudes and longitudes",
        "i" = "`cell_size` and `bandwidth` will be in decimal degrees",
        "i" = "Transform `data` to use a projected CRS or set `kde = FALSE`"
      ))
    }
  }

  # Set cell size if not specified (do this here because it is needed by both
  # `create_grid()` and `gistar()`)
  if (rlang::is_null(cell_size))
    cell_size <- set_cell_size(data, round = TRUE, quiet = quiet)

  # Create grid
  grid <- create_grid(data, cell_size = cell_size, grid_type = grid_type, quiet = quiet)

  # Count points
  counts <- count_points_in_polygons(data, grid)

  # Calculate KDE
  if (rlang::is_true(kde))
    kde_val <- kernel_density(data, grid, bandwidth = bandwidth, quiet = quiet)

  # Calculate Gi*
  result <- gistar(
    counts,
    n = "n",
    nb_dist = nb_dist,
    cell_size = cell_size,
    include_self = include_self,
    p_adjust_method = p_adjust_method,
    quiet = quiet
  )

  # Join results
  if (rlang::is_true(kde)) result$kde <- kde_val$kde_value

  # Return result
  if (rlang::is_true(kde)) {
    sf::st_as_sf(tibble::as_tibble(
      result[, c("n", "kde", "gistar", "pvalue", "geometry")]
    ))
  } else {
    sf::st_as_sf(tibble::as_tibble(
      result[, c("n", "gistar", "pvalue", "geometry")]
    ))
  }

}
