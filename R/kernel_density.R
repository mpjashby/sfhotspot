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
#' @param bandwidth_adjust single positive \code{numeric} value by which the
#'   value of \code{bandwidth} is multiplied. Useful for setting the bandwidth
#'   relative to the default.
#' @param weights character vector giving the name of a numeric column in
#'   \code{data} representing weights for weighted KDE values.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{TRUE}.
#' @param ... Further arguments passed to \code{\link[SpatialKDE]{kde}}.
#' @return An SF object based on \code{grid} with a column indicating the
#'   density estimate for each cell.
#' @noRd

kernel_density <- function(
  data,
  grid,
  bandwidth = NULL,
  bandwidth_adjust = 1,
  weights = NULL,
  cell_size = NULL,
  quiet = TRUE,
  ...
) {

  # Check inputs
  if (
    sf::st_is_longlat(data) |
    rlang::is_empty(sf::st_crs(data, parameters = TRUE))
  ) {
    rlang::abort(c(
      paste(
        "KDE values cannot be calculated for lon/lat data or data without a",
        "co-ordinate reference system"
      ),
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
  validate_bandwidth(
    bandwidth = bandwidth,
    adjust = bandwidth_adjust,
    cell_size = cell_size
  )
  if (!rlang::is_null(weights)) {
    if (!weights %in% names(data) | !rlang::is_character(weights, n = 1))
      rlang::abort("`weights` must be `NULL` or the name of a single column.")
    if (!rlang::is_bare_numeric(data[[weights]])) {
      rlang::abort(
        "`weights` must be `NULL` or the name of a column of numeric values."
      )
    }
  }
  if (!rlang::is_logical(quiet, n = 1))
    rlang::abort("`quiet` must be one of `TRUE` or `FALSE`")

  # Replace name of geometry column in SF objects if necessary
  grid <- set_geometry_name(grid)

  # Set bandwidth if not specified
  if (rlang::is_null(bandwidth)) {
    bandwidth <- set_bandwidth(data, quiet = quiet, adjust = bandwidth_adjust)
  }

  # Calculate KDE
  # Code for suppressing specific messages is from
  # https://stackoverflow.com/a/38605924/8222654
  if (rlang::is_true(quiet)) {
    if (rlang::is_null(weights)) {
      kde_val <- suppressMessages(
        SpatialKDE::kde(
          data,
          band_width = bandwidth * bandwidth_adjust,
          grid = grid,
          quiet = quiet,
          ...
        )
      )
    } else {
      kde_val <- suppressMessages(
        SpatialKDE::kde(
          data,
          band_width = bandwidth * bandwidth_adjust,
          weights = data[[weights]],
          grid = grid,
          quiet = quiet,
          ...
        )
      )
    }
  } else {
    withCallingHandlers({
      if (rlang::is_null(weights)) {
        kde_val <- SpatialKDE::kde(
          data,
          band_width = bandwidth * bandwidth_adjust,
          grid = grid,
          ...
        )
      } else {
        kde_val <- SpatialKDE::kde(
          data,
          band_width = bandwidth * bandwidth_adjust,
          weights = data[[weights]],
          grid = grid,
          ...
        )
      }
    }, message = function(m) {
      if (startsWith(conditionMessage(m), "Using centroids instead"))
        invokeRestart("muffleMessage")
    })
  }

  # Return result
  kde_val[, c("kde_value", "geometry")]

}
