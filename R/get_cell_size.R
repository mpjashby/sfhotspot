#' Extract cell size from grid
#'
#' @param grid SF object containing polygons.
#'
#' @return Numeric cell size.
#'
#' @noRd

get_cell_size <- function(grid) {

  # Validate inputs
  if (!inherits(grid, "sf"))
    rlang::abort("`grid` must be an SF object")
  if (any(!sf::st_is(grid, "POLYGON")))
    rlang::abort("`grid` must be an SF object containing polygons")

  # Extract cell size from grid
  centroids <- suppressWarnings(sf::st_centroid(grid))
  cell_size <- as.numeric(mean(sf::st_distance(
    centroids,
    centroids[sf::st_nearest_feature(centroids), ],
    by_element = TRUE
  )))

  cell_size

}
