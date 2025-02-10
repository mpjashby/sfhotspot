#' Count points in cells in polygons
#'
#' @param points \code{\link[sf]{sf}} data frame containing points.
#' @param polygons \code{\link[sf]{sf}} data frame containing polygon grid
#'   cells, e.g. as produced by \code{\link{create_grid}}.
#' @param weights character vector giving the name of a numeric column in
#'   \code{points} representing weights for weighted counts.
#' @return An SF tibble containing counts for each polygon.
#'
#' @noRd

count_points_in_polygons <- function(points, polygons, weights = NULL) {

  # Check inputs
  if (!inherits(points, "sf"))
    rlang::abort("`points` must be an SF object.")
  if (any(!sf::st_is(points, "POINT")))
    rlang::abort("`points` must be an SF object containing points.")
  if (!inherits(polygons, "sf"))
    rlang::abort("`polygons` must be an SF object.")
  if (any(!sf::st_is(polygons, "POLYGON")))
    rlang::abort("`polygons` must be an SF object containing polygons.")
  if (!rlang::is_null(weights)) {
    if (!weights %in% names(points))
      rlang::abort("`weights` must be `NULL` or the name of a single column.")
    if (!rlang::is_bare_numeric(points[[weights]])) {
      rlang::abort(
        "`weights` must be `NULL` or the name of a column of numeric values."
      )
    }
  }

  # Warn if polygons object contains column names used internally
  if ("n" %in% names(polygons)) {
    rlang::warn(c(
      "Existing column 'n' will be overwritten.",
      "i" = "Consider renaming the existing column first."
    ))
  }
  if (".polygon_id" %in% names(polygons)) {
    rlang::warn(c(
      "Existing column '.polygon_id' will be removed.",
      "i" = "Consider renaming the existing column first."
    ))
  }
  if ("x" %in% names(polygons)) {
    rlang::warn(c(
      "Existing column 'x' will be removed.",
      "i" = "Consider renaming the existing column first."
    ))
  }
  if (!rlang::is_null(weights) & "sum" %in% names(polygons)) {
    rlang::warn(c(
      "Existing column 'sum' will be overwritten.",
      "i" = "Consider renaming the existing column first."
    ))
  } else if ("sum" %in% names(polygons)) {
    rlang::warn(c(
      "Existing column 'sum' will be removed.",
      "i" = "Consider renaming the existing column first."
    ))
  }
  polygons$n <- polygons$x <- polygons$sum <- polygons$`.polygon_id` <- NULL

  # Replace name of geometry column in SF objects if necessary
  polygons <- set_geometry_name(polygons)

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

  # If weights are provided, sum those and join them
  if (!rlang::is_null(weights)) {
    sums <- stats::aggregate(
      ids[[weights]],
      list(".polygon_id" = ids$`.polygon_id`),
      FUN = sum
    )
    sums$sum <- sums$x
    sums$x <- NULL
    sums_merged <- merge(sums, polygons, by = ".polygon_id", all.y = TRUE)
    counts$sum <- ifelse(is.na(sums_merged$sum), 0, sums_merged$sum)
  }

  # Check if any points were not counted in polygons (e.g. because the polygons
  # do not cover all the points)
  if (nrow(points) > sum(counts$n)) {
    rlang::warn(c(
      paste(
        format(nrow(points) - sum(counts$n), big.mark = ","),
        "points are outside the area covered by the supplied polygons."
      ),
      "i" = "These points have not been used in generating the results."
    ))
  }

  # Remove working columns and convert to SF object
  counts <- sf::st_as_sf(
    tibble::as_tibble(counts[, !(names(counts) %in% c(".polygon_id", "x"))]),
    sf_column_name = "geometry"
  )

  counts

}
