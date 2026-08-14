#' Extract spatial features inside a polygon
#' 
#' @param data \code{\link[sf]{sf}} data frame containing points or polygons.
#' @param boundary \code{\link[sf]{sf}} data frame containing polygons.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{FALSE}.
#' @param ... Further arguments passed to \code{\link[sf]{st_intersection}}.
#' 
#' @details
#' 
#' This function is a wrapper around \code{\link[sf]{st_intersection}} that
#' performs some additional checks and reports useful information. If
#' \code{data} has a specialised result class produced by this package, that
#' class is preserved in the clipped result.
#' 
#' @return an SF data frame containing those spatial features that are covered
#'   by the polygons.
#' 
#' @export 

hotspot_clip <- function(data, boundary, quiet = FALSE, ...) {

  # Check inputs that are not checked in a helper function
  validate_inputs(
    data = data, 
    grid = boundary, 
    name_grid = "boundary", 
    data_type = c("POINT", "POLYGON", "MULTIPOLYGON"),
    quiet = quiet
  )

  # Count number of rows in data
  initial_rows <- nrow(data)

  # Record any package-specific result class, since `st_intersection()` does
  # not preserve classes it does not recognise
  result_class <- intersect(
    class(data),
    c("hspt_n", "hspt_k", "hspt_c", "hspt_d")
  )

  # Get name of geometry column in boundary file
  geometry_column <- attr(boundary, "sf_column")

  # Convert boundary dataset to a single (multi)polygon and remove everything
  # except the geometry
  boundary_outline <- sf::st_union(boundary[, geometry_column])

  # Clip data
  clipped_data <- suppressWarnings(sf::st_intersection(data, boundary_outline))

  # Restore any package-specific result class
  if (length(result_class) > 0) {
    class(clipped_data) <- c(result_class, class(clipped_data))
  }

  # Report number of rows removed
  if (rlang::is_false(quiet)) {

    final_rows <- nrow(clipped_data)
    rows_removed <- initial_rows - final_rows

    if (rows_removed > 0) {
      cli::cli_inform(
        paste0(
          "Removed {format(rows_removed, big.mark = ',', scientific = FALSE)} ",
          "rows ({sprintf('%0.1f%%', (rows_removed / initial_rows) * 100)} of ",
          "original rows) from {.var data}"
        )
      )
    }

  }

  # Return clipped data
  clipped_data

}
