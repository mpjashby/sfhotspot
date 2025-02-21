#' Create either a rectangular or hexagonal two-dimensional grid
#'
#' @param data \code{\link[sf]{sf}} data frame.
#' @param cell_size \code{numeric} value specifying the size of each equally
#'   spaced grid cell, using the same units (metres, degrees, etc.) as used in
#'   the \code{sf} data frame given in the \code{data} argument. If this
#'   argument is \code{NULL} (the default), the cell size will be calculated
#'   automatically (see Details).
#' @param grid_type \code{character} specifying whether the grid should be made
#'   up of squares (\code{"rect"}, the default) or hexagons (\code{"hex"}).
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{FALSE}.
#' @param ... Further arguments passed to \code{link[sf]{st_make_grid}}.
#'
#' @return A simple features tibble containing polygons representing grid cells.
#'
#' The grid will be based on the convex hull of \code{data}, expanded by a
#' buffer of \code{cell_size / 2} to ensure all the points in \code{data} fall
#' within the resulting grid.
#'
#' @export
#'

hotspot_grid <- function(
  data,
  cell_size = NULL,
  grid_type = "rect",
  quiet = FALSE,
  ...
) {
  create_grid(
    data = data,
    cell_size = cell_size,
    grid_type = grid_type,
    quiet = quiet,
    ...
  )
}

create_grid <- function(
  data,
  cell_size = NULL,
  grid_type = "rect",
  quiet = TRUE,
  ...
) {

  # Check inputs
  validate_sf(
    data,
    allow_null = TRUE,
    quiet = quiet,
    call = rlang::caller_env()
  )
  validate_cell_size(cell_size, call = rlang::caller_env())
  rlang::arg_match(grid_type, c("rect", "hex"))

  # Set cell size if not specified
  if (rlang::is_null(cell_size))
    cell_size <- set_cell_size(data, round = TRUE, quiet = quiet)

  # Create buffered convex hull around data
  geometry_types <- as.character(sf::st_geometry_type(data))
  if (all(geometry_types %in% c("POLYGON", "MULTIPOLYGON"))) {
    hull <- sf::st_buffer(sf::st_union(data), dist = cell_size / 2)
  } else {
    hull <- sf::st_buffer(
      sf::st_convex_hull(sf::st_union(data)),
      dist = cell_size / 2
    )
  }

  # Warn if there will be so many cells that the function will be very slow
  hull_bbox <- sf::st_bbox(hull)
  cells_n_x <- (hull_bbox$xmax - hull_bbox$xmin) / cell_size
  cells_n_y <- (hull_bbox$ymax - hull_bbox$ymin) / cell_size
  if (cells_n_x * cells_n_y > 100000 & quiet == FALSE) {
    # Although this is a warning, warnings are only printed when a function
    # finishes, which is no use. Messages are printed immediately, so this has
    # to be a message. See https://github.com/mpjashby/sfhotspot/issues/33
    cli::cli_inform(
      c(
        "!" = "The grid will contain a large number of cells",
        "i" = "This may cause other functions to run slowly or not work",
        "i" = paste0(
          "Use larger {.arg cell_size} or clip {.var data} to a smaller area ",
          "using {.fn st_intersection()}"
        )
      ),
      call = rlang::caller_env()
    )
  }

  # Create grid
  grid <- sf::st_make_grid(
    hull,
    cellsize = cell_size,
    square = grid_type == "rect",
    ...
  )

  # Construct result
  result <- sf::st_as_sf(
    tibble::tibble(geometry = grid),
    crs = sf::st_crs(data),
    sf_column_name = "geometry"
  )

  # Clip result grid to convex hull of data, retaining full grid cells
  result <- sf::st_make_valid(
    result[sf::st_intersects(result, hull, sparse = FALSE)[, 1], ]
  )

  # Keep only the polygons (not points or lines) in the grid
  result <- result[sf::st_is(result$geometry, "POLYGON"), ]

  # Error if there are no valid rows in the data
  if (nrow(result) == 0 | !inherits(result, "sf"))
    cli::cli_abort(
      c(
        "Could not create a grid of cells from supplied point data.",
        "i" = paste(
          "Try plotting {.var data} to check the points it contains can be",
          "meaningfully covered by a grid."
        )
      ),
      call = rlang::caller_env()
    )

  # Return result
  result

}
