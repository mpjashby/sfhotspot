#' Validate the inputs given to hotspot functions
#'
#' Many of the user-facing functions in this package accept common arguments,
#' which must be validated before use. This function validates those arguments
#' and either throws the appropriate error or returns \code{NULL} invisibly.
#'
#' @param data \code{\link[sf]{sf}} data frame containing points.
#' @param grid \code{\link[sf]{sf}} data frame containing polygons.
#' @param quiet a single logical value.
#' @param name_data name of the data argument in the calling function.
#' @param call the environment in which the function is called.
#'
#' @noRd

validate_inputs <- function(
  data,
  grid = NULL,
  quiet = TRUE,
  name_data = "data",
  call = rlang::caller_env()
) {

  # Validate `data` and `grid`
  validate_sf(data, label = "data", type = "POINT", quiet = quiet, call = call)
  if (!rlang::is_null(grid)) {
    validate_sf(
      grid,
      label = "grid",
      type = c("POLYGON", "MULTIPOLYGON"),
      allow_null = TRUE,
      quiet = quiet,
      call = call
    )
  }

  # Check `data` and `grid` use the same CRS
  # This is checked here because some functions of SF (e.g. `st_intersects()`)
  # can only work if the two layers use the same CRS, but we can provide a
  # more helpful error message
  if (!rlang::is_null(grid)) {
    if (sf::st_crs(data) != sf::st_crs(grid))
      cli::cli_abort(
        c(
          paste0(
            "{.var {name_data}} and {.var grid} must use the same co-ordinate ",
            "reference system (CRS)."
          ),
          "i" = paste0(
            "{.var {name_data}} uses CRS {.q {format(sf::st_crs(data))}} ",
            "({sf::st_crs(data, parameters = TRUE)$srid})."
          ),
          "i" = paste0(
            "{.var grid} uses CRS {.q {format(sf::st_crs(grid))}} ",
            "({sf::st_crs(grid, parameters = TRUE)$srid})."
          )
        ),
        call = call
      )
  }

  # Check that data and grid overlap
  if (!rlang::is_null(grid)) {
    check_overlap <- sf::st_intersects(
      sf::st_union(data),
      sf::st_union(grid),
      sparse = FALSE
    )
    if (rlang::is_false(check_overlap[1, 1])) {
      cli::cli_abort(
        c(
          "{.var {name_data}} and {.var grid} must overlap.",
          "i" = "Check co-ordinates are correct (e.g. by mapping them)."
        ),
        call = call
      )
    }
  }

  # Validate `quiet`
  if (!rlang::is_logical(quiet, n = 1))
    cli::cli_abort(
      "{.var quiet} must be one of {.code TRUE} or {.code FALSE}.",
      call = call
    )

  invisible(NULL)

}



#' Validate bandwidth parameters
#'
#' This function validates function arguments related to bandwidth.
#'
#' @param bandwidth \code{numeric} value specifying the bandwidth to be used in
#'   calculating kernel density estimates.
#' @param adjust single positive \code{numeric} value by which the
#'   value of \code{bandwidth} is multiplied.
#' @param list whether multiple bandwidths are provided.
#' @param call the environment in which the function is called.
#'
#' @noRd

validate_bandwidth <- function(
  bandwidth = NULL,
  adjust = 1,
  list = FALSE,
  cell_size = NULL,
  quiet = TRUE,
  call = rlang::caller_env()
) {

  list_str <- ifelse(list, "Each element of ", "")

  # Check bandwidth is numeric and strictly positive
  if (!rlang::is_null(bandwidth) & !rlang::is_double(bandwidth, n = 1))
    cli::cli_abort(
      "{list_str}{.var bandwidth} must be a single numeric value or NULL.",
      call = call
    )
  if (!rlang::is_null(bandwidth)) {
    if (bandwidth <= 0) {
      cli::cli_abort(
        "{list_str}{.var bandwidth} must be greater than zero.",
        call = call
      )
    }
  }

  # Check adjust is numeric and strictly positive
  if (!rlang::is_double(adjust, n = 1)) {
    cli::cli_abort(
      "{list_str}{.var bandwidth_adjust} must be a single numeric value.",
      call = call
    )
  }
  if (adjust <= 0) {
    cli::cli_abort(
      "{list_str}{.var bandwidth_adjust} must be greater than zero.",
      call = call
    )
  }

  validate_cell_size(cell_size)

  # Check bandwidth is larger than cell size
  if (
    !rlang::is_null(bandwidth) &
    !rlang::is_null(cell_size) &
    rlang::is_false(quiet)
  ) {
    if (bandwidth < cell_size) {
      cli::cli_warn(
        c(
          "Bandwidth is smaller than cell size",
          "i" = paste0(
            "If bandwidth is smaller than cell size, density estimates for ",
            "each cell will be based on counts of few or no adjacent cells. ",
            "This is unlikely to be what you want."
          ),
          "i" = paste0(
            "Did you accidentally specify {.var bandwidth} instead of ",
            "{.var bandwidth_adjust}?"
          )
        ),
        call = call
      )
    }
  }

  invisible(NULL)

}



#' Validate cell size
#'
#' @param cell_size Single numeric value to be used to create a grid of cells.
#'
#' @noRd

validate_cell_size <- function(cell_size, call = rlang::caller_env()) {

  if (!rlang::is_null(cell_size) & !rlang::is_double(cell_size, n = 1))
    cli::cli_abort(
      "{.var cell_size} must be a single numeric value or NULL.",
      call = call
    )
  if (!rlang::is_null(cell_size)) {
    if (cell_size <= 0)
      cli::cli_abort("{.var cell_size} must be greater than zero.", call = call)
  }

}



#' Validate SF objects
#'
#' @param obj object to be tested.
#' @param label name of object, which will be used in error labels.
#' @param type geometry type.
#'
#' @noRd

validate_sf <- function(
    obj,
    label = "data",
    type = NULL,
    allow_null = FALSE,
    quiet = TRUE,
    call = rlang::caller_env()
  ) {

  or_null <- ifelse(allow_null, " or NULL", "")

  # Check obj is an SF object
  inherit_error <- FALSE
  if (allow_null) {
    if (!inherits(obj, "sf") & !rlang::is_null(obj)) inherit_error <- TRUE
  } else {
    if (!inherits(obj, "sf")) inherit_error <- TRUE
  }
  if (inherit_error) {
    cli::cli_abort(
      c(
        "{.var {label}} must be an SF object{or_null}.",
        "x" = "You have supplied {.obj_type_friendly {obj}}."
      ),
      call = call
    )
  }

  # Check obj has no empty geometries
  empty <- sf::st_is_empty(obj)

  if (sum(empty) > 0) {

    if (sum(empty) == nrow(obj)) {
      msg <- c("x" = "All rows have missing geometry.")
    } else {
      fe <- which(empty)[1]
      msg <- c(
        "x" = "{sum(empty)} row{?s} contain{?s/} incorrect missing geometry.",
        "x" = "First problem: row {fe} has missing geometry."
      )
    }

    cli::cli_abort(
      c("{.var {label}} contains rows with missing geometry.", msg),
      call = call
    )

  }

  # Check obj has correct geometry type
  if (!rlang::is_null(type)) {

    wrong <- !sf::st_is(obj, type)

    if (sum(wrong) > 0) {

      gtypes <- length(unique(sf::st_geometry_type(obj)))

      if (sum(wrong) == nrow(obj) & gtypes == 1) {
        msg <- c("x" = "All rows have {sf::st_geometry_type(obj)[1]} geometry.")
      } else {
        fw <- which(wrong)[1]
        msg <- c(
          "x" = "{sum(wrong)} row{?s} contain{?s/} incorrect geometry type.",
          "x" = paste0(
            "First problem: row {fw} has {sf::st_geometry_type(obj[fw, ])} ",
            "geometry."
          )
        )
      }

      cli::cli_abort(
        c(
          "{.var {label}} must be an SF object with {.or {type}} geometry.",
          msg
        ),
        call = call
      )

    }

  }

  # Check for co-ordinates at Null Island or local equivalents
  if (all(sf::st_is(obj, "POINT"))) {
    coords <- as.data.frame(sf::st_coordinates(obj))
    coords$is_zero <- coords$X == 0 & coords$Y == 0
    if (any(coords$is_zero) & rlang::is_false(quiet)) {
      cli::cli_warn(
        c(
          "{.var {label}} has points with the co-ordinates {.q 0, 0}.",
          "i" = "This usually indicates a problem with the data.",
          "i" = "Check co-ordinates are correct (e.g. by mapping them)."
        ),
        call = call
      )
    }
  }

  invisible(NULL)

}
