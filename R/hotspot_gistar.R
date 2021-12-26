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
  if (!rlang::is_null(bandwidth) & !rlang::is_double(bandwidth, n = 1))
    rlang::abort("`bandwidth` must be NULL or a single numeric value")
  if (!rlang::is_null(bandwidth)) {
    if (bandwidth <= 0) rlang::abort("`bandwidth` must be greater than zero")
  }
  grid_type <- rlang::arg_match(grid_type, c("rect", "hex"))
  if (!rlang::is_null(nb_dist) & !rlang::is_double(nb_dist, n = 1))
    rlang::abort("`nb_dist` must be `NULL` or a single numeric value")
  if (!rlang::is_null(nb_dist)) {
    if (nb_dist <= 0) rlang::abort("`nb_dist` must be greater than zero")
  }
  if (!rlang::is_null(p_adjust_method)) {
    if (!p_adjust_method %in% stats::p.adjust.methods)
      rlang::abort(paste0(
        "`p_adjust_method` must be either `NULL` or one of \"",
        paste(stats::p.adjust.methods, collapse = "\", \""), "\""
      ))
  }
  if (!rlang::is_logical(quiet))
    rlang::abort("`quiet` must be one of `TRUE` or `FALSE`")
  if (sf::st_is_longlat(data)) {
    if (rlang::is_true(kde)) {
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

  # Set bandwidth if not specified
  if (rlang::is_null(bandwidth)) {
    bandwidth <- bandwidth_nrd_sf(data)
    if (rlang::is_false(quiet)) {
      rlang::inform(c("i" = paste(
        "Bandwidth set to", format(bandwidth, big.mark = ","), unit_pl,
        "automatically based on rule of thumb"
      )))
    }
  }

  # Set neighbour distance if not specified
  # No message is created in this case because users will almost always want to
  # leave the value as the default, and because the default value is fixed so is
  # given in the manual page
  if (rlang::is_null(nb_dist)) nb_dist <- cell_size * sqrt(2)

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

  # Calculate KDE
  if (kde == TRUE) {
    if (rlang::is_true(quiet)) {
      kde_val <- suppressMessages(
        SpatialKDE::kde(data, band_width = bandwidth, grid = grid)
      )
    } else {
      kde_val <- SpatialKDE::kde(data, band_width = bandwidth, grid = grid)
    }
  }

  # Find neighbours
  centroids <- suppressWarnings(sf::st_centroid(grid))
  nb <- spdep::dnearneigh(sf::st_coordinates(centroids), 0, nb_dist)

  # Determine if each cell should be treated as a neighbour of itself
  if (include_self == TRUE) {
    nb <- spdep::include.self(nb)
  }

  # Calculate gi* statistic
  gi <- spdep::localG(counts$n, listw = spdep::nb2listw(nb, style = "B"))

  # Join results
  result <- counts
  if (rlang::is_true(kde)) result$kde <- kde_val$kde_value
  result$gistar <- as.numeric(gi)
  result$pvalue <- spdep::p.adjustSP(
    2 * stats::pnorm(-abs(as.numeric(result$gistar))),
    nb,
    method = ifelse(rlang::is_null(p_adjust_method), "none", p_adjust_method)
  )

  # Return result
  if (rlang::is_true(kde)) {
    sf::st_as_sf(tibble::as_tibble(
      result[, c("id", "n", "kde", "gistar", "pvalue", "geometry")]
    ))
  } else {
    sf::st_as_sf(tibble::as_tibble(
      result[, c("id", "n", "gistar", "pvalue", "geometry")]
    ))
  }

}
