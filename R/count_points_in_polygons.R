#' Count points in cells in polygons
#'
#' @param points \code{\link[sf]{sf}} data frame containing points.
#' @param polygons \code{\link[sf]{sf}} data frame containing polygon grid
#'   cells, e.g. as produced by \code{\link{create_grid}}.
#' @return An SF tibble containing counts for each polygon.
#'
#' @noRd

count_points_in_polygons <- function(points, polygons) {

  # Check inputs
  if (!inherits(points, "sf"))
    rlang::abort("`points` must be an SF object")
  if (any(!sf::st_is(points, "POINT")))
    rlang::abort("`points` must be an SF object containing points")
  if (!inherits(polygons, "sf"))
    rlang::abort("`polygons` must be an SF object")
  if (any(!sf::st_is(polygons, "POLYGON")))
    rlang::abort("`polygons` must be an SF object containing polygons")

  # Create a unique ID for each polygon
  polygons$`.polygon_id` <- seq_len(nrow(polygons))

  # Join the unique polygon IDs to each point
  ids <- sf::st_drop_geometry(sf::st_join(points, polygons))

  # Count the number of points with each polygon ID
  counts <- stats::aggregate(
    ids$`.polygon_id`,
    list(".polygon_id" = ids$`.polygon_id`),
    FUN = length
  )

  # Join the counts to the polygons
  counts <- merge(counts, polygons, by = ".polygon_id", all.y = TRUE)

  # Replace NAs produced by zero counts with zeros
  counts$n <- ifelse(is.na(counts$x), 0, counts$x)

  # Remove working columns and convert to SF object
  counts <- sf::st_as_sf(
    tibble::as_tibble(counts[, c("n", "geometry")]),
    sf_column_name = "geometry"
  )

  counts

}
