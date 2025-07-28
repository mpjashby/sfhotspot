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
#' @param transform the underlying SpatialKDE package cannot calculate kernel
#'   density for lon/lat data, so this must be transformed to use a projected
#'   co-ordinate reference system. If this argument is \code{TRUE} (the 
#'   default) and \code{sf::st_is_longlat(data)} is \code{TRUE}, \code{data} 
#'   (and \code{grid} if provided) will be transformed automatically using 
#'   \code{link{st_transform_auto}} before the kernel density is estimated and
#'   transformed back afterwards. Set this argument to \code{FALSE} to suppress 
#'   automatic transformation of the data.
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
  transform = FALSE,
  quiet = TRUE,
  ...
) {

  # Check inputs
  if (rlang::is_empty(sf::st_crs(data, parameters = TRUE))) {
    cli::cli_abort(c(
      "Co-ordinate reference system for {.var data} is missing.",
      "i" = "Cannot calculate KDE values from datasets with a missing CRS.",
      "i" = "Check CRS of {.var data} using {.fn st_crs()}."
    ))
  }
  validate_sf(
    grid,
    label = "grid",
    type = c("POLYGON", "MULTIPOLYGON"),
    quiet = quiet
  )
  validate_bandwidth(
    bandwidth = bandwidth,
    adjust = bandwidth_adjust,
    cell_size = cell_size
  )
  if (!rlang::is_null(weights)) {
    if (!weights %in% names(data) | !rlang::is_character(weights, n = 1))
      cli::cli_abort(
        "{.arg weights} must be NULL or the name of a single column."
      )
    if (!rlang::is_bare_numeric(data[[weights]])) {
      cli::cli_abort(
        "{.arg weights} must be NULL or the name of a column of numeric values."
      )
    }
  }
  if (!rlang::is_logical(transform, n = 1)) {
    cli::cli_abort("{.arg transform} must be one of {.q TRUE} or {.q FALSE}.")
  }
  if (!rlang::is_logical(quiet, n = 1)) {
    cli::cli_abort("{.arg quiet} must be one of {.q TRUE} or {.q FALSE}.")
  }

  # Transform CRS if required
  is_transformed <- FALSE
  if (sf::st_is_longlat(data)) {
    if (rlang::is_true(transform)) {
      data <- st_transform_auto(data, quiet = quiet)
      grid <- st_transform_auto(grid, quiet = TRUE)
      is_transformed <- TRUE
    } else {
      cli::cli_abort(c(
        "Cannot calculate KDE values for lon/lat data. You can:",
        "*" = paste0(
          "set {.arg transform} to {.q TRUE} to allow auto-transformation or"
        ),
        "*" = "transform {.var data} manually to use a projected CRS."
      ))  
    }
  }

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
  if (is_transformed) {
    st_transform_auto(kde_val[, c("kde_value", "geometry")], quiet = TRUE)
  } else {
    kde_val[, c("kde_value", "geometry")]
  }

}
