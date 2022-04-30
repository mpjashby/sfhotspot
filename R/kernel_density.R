#' Estimate two-dimensional kernel density of points on a regular grid
#'
#' @param data \code{\link[sf]{sf}} data frame containing points.
#' @param grid \code{\link[sf]{sf}} data frame containing polygon grid cells,
#'   e.g. as produced by \code{\link{create_grid}}.
#' @param bandwidth \code{numeric} value specifying the bandwidth to be used in
#'   calculating the kernel density estimates. If this argument is \code{NULL}
#'   (the default), the bandwidth will be specified automatically using the mean
#'   result of \code{\link[MASS]{bandwidth.nrd}} called on the \code{x} and
#'   \code{y} co-ordinates separately.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{TRUE}.
#' @return An SF object based on \code{grid} with a column indicating the
#'   density estimate for each cell.
#' @noRd

kernel_density <- function(data, grid, bandwidth = NULL, quiet = TRUE) {

  # Check inputs
  if (!inherits(data, "sf"))
    rlang::abort("`data` must be an SF object")
  if (any(!sf::st_is(data, "POINT")))
    rlang::abort("`data` must be an SF object containing points")
  if (
    sf::st_is_longlat(data) |
    rlang::is_empty(sf::st_crs(data, parameters = TRUE))
  ) {
    rlang::abort(c(
      "KDE values cannot be calculated for lon/lat data or data without a co-ordinate reference system",
      "i" = "Check projection of `data` using st_crs()",
      "i" = "Transform `data` to use a projected CRS"
    ))
  }
  if (!inherits(grid, "sf"))
    rlang::abort("`grid` must be an SF object")
  if (any(!sf::st_is(grid, "POLYGON")))
    rlang::abort("`grid` must be an SF object containing polygons")
  if (sf::st_is_longlat(grid)) {
    rlang::abort(c(
      "KDE values cannot be calculated for lon/lat data",
      "i" = "Transform `grid` to use a projected CRS"
    ))
  }
  if (!rlang::is_null(bandwidth) & !rlang::is_double(bandwidth, n = 1))
    rlang::abort("`bandwidth` must be NULL or a single numeric value")
  if (!rlang::is_null(bandwidth)) {
    if (bandwidth <= 0) rlang::abort("`bandwidth` must be greater than zero")
  }
  if (!rlang::is_logical(quiet))
    rlang::abort("`quiet` must be one of `TRUE` or `FALSE`")

  # Set bandwidth if not specified
  if (rlang::is_null(bandwidth)) bandwidth <- set_bandwidth(data, quiet = quiet)

  # Calculate KDE
  # Code for suppressing specific messages is from
  # https://stackoverflow.com/a/38605924/8222654
  if (rlang::is_true(quiet)) {
    kde_val <- suppressMessages(
      SpatialKDE::kde(data, band_width = bandwidth, grid = grid)
    )
  } else {
    withCallingHandlers({
      kde_val <- SpatialKDE::kde(data, band_width = bandwidth, grid = grid)
    }, message = function(m) {
      if (startsWith(conditionMessage(m), "Using centroids instead"))
        invokeRestart("muffleMessage")
    })
  }

  # Return result
  kde_val[, c("kde_value", "geometry")]

}
