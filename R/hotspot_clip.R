#' Extract spatial features inside a polygon
#'
#' @param data \code{\link[sf]{sf}} data frame containing spatial features.
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
#' class is preserved in the clipped result. A warning is produced if clipping
#' reduces the dimension of any geometry, such as from a polygon to a line.
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
    data_type = NULL,
    quiet = quiet
  )

  # Count number of rows in data
  initial_rows <- nrow(data)

  # Record any package-specific result class, since `st_intersection()` does
  # not preserve classes it does not recognise
  result_class <- intersect(
    class(data),
    c("hspt_n", "hspt_k", "hspt_c", "hspt_d", "hspt_g")
  )

  # Get name of geometry column in boundary file
  geometry_column <- attr(boundary, "sf_column")

  # Convert boundary dataset to a single (multi)polygon and remove everything
  # except the geometry
  boundary_outline <- sf::st_union(boundary[, geometry_column])

  # Add a temporary ID so input and output geometry types can be compared even
  # if clipping removes features or produces more than one output feature
  source_id <- ".hotspot_clip_source_id"
  while (source_id %in% names(data)) {
    source_id <- paste0(source_id, "_")
  }
  data[[source_id]] <- seq_len(nrow(data))
  input_types <- as.character(sf::st_geometry_type(data))
  input_dimensions <- sf::st_dimension(data)

  # Clip data, suppressing the warning produced because `st_intersection()`
  # cannot know whether attribute values apply to only part of each geometry
  clipped_data <- withCallingHandlers(
    sf::st_intersection(data, boundary_outline),
    warning = function(w) {
      if (
        identical(
          conditionMessage(w),
          paste(
            "attribute variables are assumed to be spatially constant",
            "throughout all geometries"
          )
        )
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )

  # Warn if clipping produced lower-dimensional geometry, e.g. if a polygon
  # touching the boundary only at an edge was reduced to a line
  output_types <- as.character(sf::st_geometry_type(clipped_data))
  output_dimensions <- sf::st_dimension(clipped_data)
  lower_dimension <- output_dimensions <
    input_dimensions[clipped_data[[source_id]]]

  if (any(lower_dimension) && rlang::is_false(quiet)) {
    transitions <- paste0(
      input_types[clipped_data[[source_id]][lower_dimension]],
      " -> ",
      output_types[lower_dimension]
    )
    transition_counts <- sort(table(transitions), decreasing = TRUE)
    transition_summary <- paste0(
      as.integer(transition_counts),
      " ",
      names(transition_counts),
      collapse = ", "
    )
    cli::cli_warn(
      c(
        paste0(
          "Clipping reduced the geometry dimension of ",
          "{sum(lower_dimension)} output feature{?s}."
        ),
        "i" = "Geometry changes: {transition_summary}.",
        "!" = paste0(
          "This may cause problems with using the result. ",
          "Consider plotting the result on a map to check."
        )
      )
    )
  }

  clipped_data[[source_id]] <- NULL

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
