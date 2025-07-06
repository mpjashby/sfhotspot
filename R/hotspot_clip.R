#' Extract points inside polygon
#' 
#' @param data \code{\link[sf]{sf}} data frame containing points.
#' @param boundary \code{\link[sf]{sf}} data frame containing polygons.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{FALSE}.
#' @param ... Further arguments passed to \code{\link[sf]{st_intersection}}.
#' 
#' @details
#' 
#' This function is a wrapper around \code{\link[sf]{st_intersection}} that
#'   performs some additional checks and reports useful information.
#' 
#' @return an SF data frame containing those points that are covered by the
#'   polygons.
#' 
#' @export 

hotspot_clip <- function(data, boundary, quiet = FALSE, ...) {

  # Check inputs that are not checked in a helper function
  validate_inputs(
    data = data, 
    grid = boundary, 
    name_grid = "boundary", 
    quiet = quiet
  )

  # Count number of rows in data
  initial_rows <- nrow(data)

  # Get name of geometry column in boundary file
  geometry_column <- attr(boundary, "sf_column")

  # Convert boundary dataset to a single (multi)polygon and remove everything
  # except the geometry
  boundary_outline <- sf::st_union(boundary[, geometry_column])

  # Clip data
  clipped_data <- suppressWarnings(sf::st_intersection(data, boundary_outline))

  # Report number of rows removed
  if (rlang::is_false(quiet)) {

    final_rows <- nrow(clipped_data)
    rows_removed <- initial_rows - final_rows

    cli::cli_inform(
      paste0(
        "Removed {format(rows_removed, big.mark = ',', scientific = FALSE)} ",
        "rows ({sprintf('%0.1f%%', (rows_removed / initial_rows) * 100)} of ",
        "original rows) from {.var data}"
      )
    )

  }

  # Return clipped data
  clipped_data

}