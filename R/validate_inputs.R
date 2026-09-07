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
#' @param data_type geometry types allowed in the data argument.
#' @param call the environment in which the function is called.
#'
#' @noRd

validate_inputs <- function(
  data,
  grid = NULL,
  quiet = TRUE,
  name_data = "data",
  name_grid = "grid",
  data_type = "POINT",
  require_units = FALSE,
  call = rlang::caller_env()
) {

  # Validate `data` and `grid`
  validate_sf(
    data, 
    label = name_data, 
    type = data_type,
    require_units = require_units,
    quiet = quiet, 
    call = call
  )
  if (!rlang::is_null(grid)) {
    validate_sf(
      grid,
      label = name_grid,
      type = c("POLYGON", "MULTIPOLYGON"),
      allow_null = TRUE,
      require_units = require_units,
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
            "{.var {name_data}} and {.var {name_grid}} must use the same ",
            "co-ordinate reference system (CRS)."
          ),
          "i" = paste0(
            "{.var {name_data}} uses CRS {.q {format(sf::st_crs(data))}} ",
            "({sf::st_crs(data, parameters = TRUE)$srid})."
          ),
          "i" = paste0(
            "{.var {name_grid}} uses CRS {.q {format(sf::st_crs(grid))}} ",
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
          "{.var {name_data}} and {.var {name_grid}} must overlap.",
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
    allow_empty = FALSE,
    require_crs = TRUE,
    require_units = FALSE,
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

  if (allow_null && rlang::is_null(obj)) {
    return(invisible(NULL))
  }

  # Check that object has more than zero rows
  if (nrow(obj) <= 0) {

    cli::cli_abort(
      c(
        "{.var {label}} contains zero rows of data.",
        "i" = "Has a previous line of code unexpectedly removed all rows?"
      ),
      call = call
    )
    
  }


  # A CRS is needed to interpret co-ordinates and to compare spatial objects.
  crs <- sf::st_crs(obj)
  if (require_crs && (is.na(crs) || is.na(crs$wkt) || !nzchar(crs$wkt))) {
    cli::cli_abort(
      c(
        "Co-ordinate reference system for {.var {label}} is missing.",
        "i" = "Check or set the CRS using {.fn st_crs}."
      ),
      call = call
    )
  }

  # Distance-based operations also need to know what one co-ordinate unit
  # represents. Do not reject uncommon units as long as they are specified.
  if (require_units) {
    units <- sf::st_crs(obj, parameters = TRUE)$units_gdal
    if (
      rlang::is_null(units) ||
        length(units) == 0 ||
        is.na(units) ||
        !nzchar(units)
    ) {
      cli::cli_abort(
        c(
          "Unit metadata for the CRS of {.var {label}} is missing.",
          "i" = paste0(
            "This operation creates a grid or uses a distance parameter, ",
            "so the co-ordinate units must be known."
          ),
          "i" = "Set a complete CRS or transform {.var {label}} to a CRS with known units."
        ),
        call = call
      )
    }
  }

  # Check obj has no empty geometries
  empty <- sf::st_is_empty(obj)

  if (!allow_empty && sum(empty) > 0) {

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


#' Prepare spatial data for hotspot analysis
#'
#' @noRd
prepare_spatial_data <- function(
  data,
  quiet = FALSE,
  label = "data",
  call = rlang::caller_env()
) {
  # Perform checks needed before geometry can safely be normalised.
  validate_sf(
    data,
    label = label,
    type = NULL,
    allow_empty = TRUE,
    quiet = TRUE,
    call = call
  )

  empty <- sf::st_is_empty(data)
  if (any(empty)) {
    removed <- sum(empty)
    data <- data[!empty, , drop = FALSE]
    if (rlang::is_false(quiet)) {
      cli::cli_inform(
        "Removed {removed} row{?s} from {.var {label}} because {?it has/they have} empty geometry.",
        call = call
      )
    }
    if (nrow(data) == 0) {
      cli::cli_abort(
        c(
          "No rows with non-empty geometry remain in {.var {label}}.",
          "i" = "All input geometries were empty."
        ),
        call = call
      )
    }
  }

  # GEOS does not support M dimensions, and hotspot methods use only X and Y.
  data <- sf::st_zm(data, drop = TRUE, what = "ZM")

  data
}


#' Prepare point data for hotspot analysis
#'
#' @noRd
prepare_point_data <- function(
  data,
  attributes = NULL,
  quiet = FALSE,
  label = "data",
  call = rlang::caller_env()
) {
  data <- prepare_spatial_data(
    data,
    quiet = quiet,
    label = label,
    call = call
  )

  geometry_types <- unique(as.character(sf::st_geometry_type(data)))
  point_types <- c("POINT", "MULTIPOINT")
  if (!all(geometry_types %in% point_types)) {
    validate_sf(data, label = label, type = "POINT", quiet = quiet, call = call)
  }

  if (any(sf::st_is(data, "MULTIPOINT"))) {
    attributes <- stats::na.omit(attributes)
    attributes <- attributes[nzchar(attributes)]
    if (length(attributes) > 0) {
      cli::cli_abort(
        c(
          "{.var {label}} cannot contain MULTIPOINT geometry when attribute data are used.",
          "i" = "Casting MULTIPOINT to POINT would repeat values from {.var {attributes}} for each resulting point."
        ),
        call = call
      )
    }
    data <- suppressWarnings(sf::st_cast(data, "POINT"))
  }

  validate_sf(data, label = label, type = "POINT", quiet = TRUE, call = call)
  data
}


#' Validate and class a hotspot result
#'
#' @noRd
new_hotspot_results <- function(
  result,
  class = NULL,
  ...,
  call = rlang::caller_env()
) {
  if (!inherits(result, "sf")) {
    cli::cli_abort("The hotspot operation did not produce an SF object.", call = call)
  }
  if (nrow(result) == 0) {
    cli::cli_abort(
      c(
        "The hotspot operation produced zero output rows.",
        "i" = "Check the input geometries and analysis parameters."
      ),
      call = call
    )
  }
  empty <- sf::st_is_empty(result)
  if (any(empty)) {
    cli::cli_abort(
      c(
        "The hotspot operation produced empty output geometry.",
        "x" = "{sum(empty)} output row{?s} contain{?s/} empty geometry."
      ),
      call = call
    )
  }

  class <- unique(class)
  base_classes <- setdiff(base::class(result), class)
  structure(result, class = c(class, base_classes), ...)
}
