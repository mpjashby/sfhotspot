#' Estimate the relationship between the kernel density of two layers of points
#'
#' @param x,y \code{\link[sf]{sf}} data frames containing points.
#' @param cell_size \code{numeric} value specifying the size of each equally
#'   spaced grid cell, using the same units (metres, degrees, etc.) as used in
#'   the \code{sf} data frame given in the \code{x} argument. Ignored if
#'   \code{grid} is not \code{NULL}. If this argument and \code{grid} are
#'   \code{NULL} (the default), the cell size will be calculated automatically
#'   (see Details).
#' @param grid_type \code{character} specifying whether the grid should be made
#'   up of squares (\code{"rect"}, the default) or hexagons (\code{"hex"}).
#'   Ignored if \code{grid} is not \code{NULL}.
#' @param bandwidth either a single \code{numeric} value specifying the
#'   bandwidth to be used in calculating the kernel density estimates, or a list
#'   of exactly 2 such values. If this argument is \code{NULL} (the default),
#'   the bandwidth for both \code{x} and \code{y} will be determined
#'   automatically using the result of \code{\link[MASS]{bandwidth.nrd}} called
#'   on the co-ordinates of the points in \code{x}. If this argument is
#'   \code{list(NULL, NULL)}, separate bandwidths will be determined
#'   automatically for \code{x} and \code{y} based on each layer.
#' @param bandwidth_adjust single positive \code{numeric} value by which the
#'   value of \code{bandwidth} for both \code{x} and \code{y} will be
#'   multiplied, or a list of two such values. Useful for setting the bandwidth
#'   relative to the default.
#' @param method \code{character} specifying the method by which the densities,
#'   \code{d()}, of \code{x} and \code{y} will be related:
#'   \describe{
#'     \item{\code{ratio}}{(the default) calculates the density of \code{x}
#'     divided by the density of \code{y}, i.e. \code{d(x) / d(y)}.}
#'     \item{\code{log}}{calculates the natural logarithm of the density of
#'     \code{x} divided by the density of \code{y}, i.e.
#'     \code{log(d(x) / d(y))}.}
#'     \item{\code{diff}}{calculates the difference between the density of
#'     \code{x} and the density of \code{y}, i.e. \code{d(x) - d(y)}.}
#'     \item{\code{sum}}{calculates the sum of the density of \code{x} and the
#'     density of \code{y}, i.e. \code{d(x) + d(y)}.}
#'   }
#'   The result of this calculation will be returned in the \code{kde} column of
#'   the return value.
#' @param grid \code{\link[sf]{sf}} data frame containing polygons, which will
#'   be used as the grid for which densities are estimated.
#' @param weights \code{NULL} (the default) or a vector of length two giving
#'   either \code{NULL} or the name of a column in each of \code{x} and
#'   \code{y} to be used as weights for weighted counts and KDE values.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{FALSE}.
#' @param ... Further arguments passed to \code{\link[SpatialKDE]{kde}}.
#' @return An \code{\link[sf]{sf}} tibble of grid cells with corresponding point
#'   counts and dual kernel density estimates for each cell. This can be plotted
#'   using \code{\link{autoplot}}.
#'
#' This function creates a regular two-dimensional grid of cells (unless a
#' custom grid is specified with \code{grid}), calculates the density of points
#' in each cell for each of \code{x} and \code{y} using functions from the
#' \code{SpatialKDE} package, then produces a value representing a relation
#' between the two densities. The count of points in each cell is also returned.
#'
#' Dual kernel density values can be useful for understanding the relationship
#' between the distributions of two sets of point locations. For example:
#'
#'   * The ratio between two densities representing the locations of burglaries
#'     and the locations of houses can show the distribution of the risk
#'     (incidence rate) of burglaries. The logged ratio may be useful to show
#'     relationships where one set of points has an extremely skewed
#'     distribution.
#'   * The difference between two densities can show the change in distributions
#'     between two points in time.
#'   * The sum of two densities can be used to estimate the total density of two
#'     types of point, e.g. the locations of occurrences of two diseases.
#'
#' ## Coverage of the output data
#'
#' The grid produced by this function covers the convex hull of the points in
#' \code{x}. This means the result may include KDE values for cells that are
#' outside the area for which data were provided, which could be misleading. To
#' handle this, consider cropping the output layer to the area for which data
#' are available. For example, if you only have crime data for a particular
#' district, crop the output dataset to the district boundary using
#' \code{\link[sf]{st_intersection}}.
#'
#' ## Automatic cell-size selection
#'
#' If no cell size is given then the cell size will be set so that there are 50
#' cells on the shorter side of the grid. If the `x` SF object is projected
#' in metres or feet, the number of cells will be adjusted upwards so that the
#' cell size is a multiple of 100.
#'
#' @references
#' Yin, P. (2020). Kernels and Density Estimation. \emph{The Geographic
#' Information Science & Technology Body of Knowledge} (1st Quarter 2020
#' Edition), John P. Wilson (ed.).
#' doi:\doi{10.22224/gistbok/2020.1.12}
#'
#' @examples
#' # See also the examples for `hotspot_kde()` for examples of how to specify
#' # `cell_size`, `bandwidth`, etc.
#'
#' library(sf)
#'
#' # Transform data to UTM zone 15N so that cell_size and bandwidth can be set
#' # in metres
#' memphis_robberies_utm <- st_transform(memphis_robberies, 32615)
#' memphis_population_utm <- st_transform(memphis_population, 32615)
#'
#' # Calculate burglary risk based on residential population. `weights` is set
#' # to `c(NULL, population)` so that the robberies layer is not weighted and
#' # the population layer is weighted according to the number of residents in
#' # each census block.
#' \donttest{
#' hotspot_dual_kde(
#'   memphis_robberies_utm,
#'   memphis_population_utm,
#'   bandwidth = list(NULL, NULL),
#'   weights = c(NULL, population)
#' )
#' }
#'
#' @export

hotspot_dual_kde <- function(
  x,
  y,
  cell_size = NULL,
  grid_type = "rect",
  bandwidth = NULL,
  bandwidth_adjust = 1,
  method = "ratio",
  grid = NULL,
  weights = NULL,
  quiet = FALSE,
  ...
) {

  # Process arguments that are column names
  weights_name_error <- FALSE
  if (rlang::quo_is_null(rlang::enquo(weights))) {
    weights_y <- weights_x <- NA_character_
  } else {
    weights_names <- rlang::quo_get_expr(rlang::enquo(weights))
    if (length(weights_names) == 3) {
      weights_x <- ifelse(
        rlang::is_null(weights_names[[2]]),
        NA_character_,
        rlang::as_name(weights_names[[2]])
      )
      weights_y <- ifelse(
        rlang::is_null(weights_names[[3]]),
        NA_character_,
        rlang::as_name(weights_names[[3]])
      )
    } else {
      weights_name_error <- TRUE
    }
  }
  if (exists("weights_x") & exists("weights_y")) {
    if (!rlang::is_chr_na(weights_x)) {
      if (!weights_x %in% names(x)) weights_name_error <- TRUE
    }
    if (!rlang::is_chr_na(weights_y)) {
      if (!weights_y %in% names(y)) weights_name_error <- TRUE
    }
  } else {
    weights_name_error <- TRUE
  }
  if (weights_name_error) {
    rlang::abort(paste(
      "`weights` must be `NULL` or a vector of two names, the first a column",
      "in `x` and the second a column in `y`"
    ))
  }

  # Check inputs that are not checked in a helper function
  # `validate_inputs()` is called twice to validate both datasets
  validate_inputs(data = x, grid = grid, quiet = quiet, name_data = "x")
  validate_inputs(data = y, grid = NULL, quiet = quiet, name_data = "y")
  # `arg_match()` throws an uninformative error if `method` has length 0, so
  # first test if `method` is a character vector of length 1
  if (!rlang::is_character(method, n = 1))
    rlang::abort('`method` must be one of "ratio", "log", "diff", or "sum"')
  rlang::arg_match(method, c("ratio", "log", "diff", "sum"), multiple = FALSE)

  # Create grid
  if (rlang::is_null(grid)) {
    grid <- create_grid(
      x,
      cell_size = cell_size,
      grid_type = grid_type,
      quiet = quiet
    )
  }

  # Extract bandwidth values for each layer
  # These need to be validated here to prevent duplicate messages
  if (rlang::is_bare_list(bandwidth, n = 2)) {
    validate_bandwidth(bandwidth = bandwidth[[1]], list = TRUE)
    validate_bandwidth(bandwidth = bandwidth[[2]], list = TRUE)
    bandwidth_x <- ifelse(
      rlang::is_null(bandwidth[[1]]),
      set_bandwidth(x, quiet = quiet, label = "for `x`"),
      bandwidth[[1]]
    )
    bandwidth_y <- ifelse(
      rlang::is_null(bandwidth[[2]]),
      set_bandwidth(y, quiet = quiet, label = "for `y`"),
      bandwidth[[2]]
    )
  } else {
    validate_bandwidth(bandwidth = bandwidth)
    if (rlang::is_null(bandwidth)) {
      bandwidth_y <- bandwidth_x <- set_bandwidth(
        x,
        quiet = quiet,
        label = "for `x` and `y`"
      )
    } else {
      bandwidth_y <- bandwidth_x <- bandwidth
    }
  }
  if (rlang::is_bare_list(bandwidth_adjust, n = 2)) {
    validate_bandwidth(adjust = bandwidth_adjust[[1]], list = TRUE)
    validate_bandwidth(adjust = bandwidth_adjust[[2]], list = TRUE)
    bandwidth_adjust_x <- bandwidth_adjust[[1]]
    bandwidth_adjust_y <- bandwidth_adjust[[2]]
  } else {
    validate_bandwidth(adjust = bandwidth_adjust)
    bandwidth_adjust_x <- bandwidth_adjust_y <- bandwidth_adjust
  }

  # Count points
  if (rlang::is_chr_na(weights_x)) {
    counts <- count_points_in_polygons(x, grid)
  } else {
    counts <- count_points_in_polygons(x, grid, weights = weights_x)
  }

  # Check if any points in `y` were not counted in polygons because the polygons
  # (which are based on the bounding box of `x`) do not cover all the points

  # Calculate KDE for `x`
  if (rlang::is_chr_na(weights_x)) {
    kde_x <- kernel_density(
      x,
      grid,
      bandwidth = bandwidth_x,
      bandwidth_adjust = bandwidth_adjust_x,
      quiet = quiet,
      ...
    )
  } else {
    kde_x <- kernel_density(
      x,
      grid,
      bandwidth = bandwidth_x,
      bandwidth_adjust = bandwidth_adjust_x,
      weights = weights_x,
      quiet = quiet,
      ...
    )
  }

  # Calculate KDE for `y`
  if (rlang::is_chr_na(weights_y)) {
    kde_y <- kernel_density(
      y,
      grid,
      bandwidth = bandwidth_y,
      bandwidth_adjust = bandwidth_adjust_y,
      quiet = quiet,
      ...
    )
  } else {
    kde_y <- kernel_density(
      y,
      grid,
      bandwidth = bandwidth_y,
      bandwidth_adjust = bandwidth_adjust_y,
      weights = weights_y,
      quiet = quiet,
      ...
    )
  }

  # Combine layers
  kde <- kde_x[, "geometry"]
  kde$kde_x <- kde_x$kde_value
  kde$kde_y <- kde_y$kde_value

  # Compare KDE layers
  if (method == "log") {
    counts$kde <- log(kde$kde_x / kde$kde_y)
  } else if (method == "diff") {
    counts$kde <- kde$kde_x - kde$kde_y
  } else if (method == "sum") {
    counts$kde <- kde$kde_x + kde$kde_y
  } else {
    counts$kde <- kde$kde_x / kde$kde_y
  }

  # Return result
  if ("sum" %in% names(counts)) {
    result <- sf::st_as_sf(
      tibble::as_tibble(counts[, c("n", "sum", "kde", "geometry")])
    )
  } else {
    result <- sf::st_as_sf(
      tibble::as_tibble(counts[, c("n", "kde", "geometry")])
    )
  }
  structure(result, class = c("hspt_k", class(result)))

}
