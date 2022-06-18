#' Count points in cells in a two-dimensional grid
#'
#' @param data \code{\link[sf]{sf}} data frame containing points.
#' @param cell_size \code{numeric} value specifying the size of each equally
#'   spaced grid cell, using the same units (metres, degrees, etc.) as used in
#'   the \code{sf} data frame given in the \code{data} argument. Ignored if
#'   \code{grid} is not \code{NULL}. If this argument and \code{grid} are
#'   \code{NULL} (the default), the cell size will be calculated automatically
#'   (see Details).
#' @param grid_type \code{character} specifying whether the grid should be made
#'   up of squares (\code{"rect"}, the default) or hexagons (\code{"hex"}).
#'   Ignored if \code{grid} is not \code{NULL}.
#' @param grid \code{\link[sf]{sf}} data frame containing polygons, which will
#'   be used as the grid for which counts are made.
#' @param weights \code{NULL} or the name of a column in \code{data} to be used
#'   as weights for weighted counts.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{FALSE}.
#' @return An \code{\link[sf]{sf}} tibble of regular grid cells with
#'   corresponding point counts for each cell. This can be plotted using
#'   \code{\link{autoplot}}.
#'
#' @details
#'
#' This function counts the number of points in each cell in a regular grid. If
#' a column name in \code{data} is supplied with the \code{weights} argument,
#' weighted counts will also be produced.
#'
#' ## Automatic cell-size selection
#'
#' If `grid` is `NULL` and no cell size is given, the cell size will be set so
#' that there are 50 cells on the shorter side of the grid. If the `data` SF
#' object is projected in metres or feet, the number of cells will be adjusted
#' upwards so that the cell size is a multiple of 100.
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
  grid = NULL,
  weights = NULL,
  quiet = FALSE
) {

  # Process arguments that are column names
  weights <- ifelse(
    rlang::quo_is_null(rlang::enquo(weights)),
    NA_character_,
    rlang::as_name(rlang::enquo(weights))
  )

  # Check inputs that are not checked in a helper function
  validate_inputs(data = data, grid = grid, quiet = quiet)

  # Create grid
  if (rlang::is_null(grid)) {
    grid <- create_grid(
      data,
      cell_size = cell_size,
      grid_type = grid_type,
      quiet = quiet
    )
  }

  # Count points
  if (rlang::is_chr_na(weights)) {
    counts <- count_points_in_polygons(data, grid)
  } else {
    counts <- count_points_in_polygons(data, grid, weights = weights)
  }

  # Return result
  structure(counts, class = c("hspt_n", class(counts)))

}
