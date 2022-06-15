#' Validate the inputs given to hotspot functions
#'
#' Many of the user-facing functions in this package accept common arguments,
#' which must be validated before use. This function validates those arguments
#' and either throws the appropriate error or returns \code{NULL} invisibly.
#'
#' @param data \code{\link[sf]{sf}} data frame containing points.
#' @param grid \code{\link[sf]{sf}} data frame containing polygons.
#' @param quiet a single logical value.
#'
#' @noRd

validate_inputs <- function(
  data,
  grid,
  quiet,
  ...,
  call = rlang::caller_env()
) {

  # Validate `data`
  if (!inherits(data, "sf"))
    rlang::abort("`data` must be an SF object.", call = call)
  if (any(!sf::st_is(data, "POINT")))
    rlang::abort("`data` must be an SF object containing points.", call = call)
  if (any(sf::st_is_empty(data))) {
    rlang::abort(
      c(
        "`data` contains empty geometries.",
        "i" = paste(
          "Identify and remove empty geometries, e.g. using `st_is_empty()`."
        )
      ),
      call = call
    )
  }

  # Check for co-ordinates at Null Island or local equivalents
  coords <- as.data.frame(sf::st_coordinates(data))
  coords$is_zero <- coords$X == 0 & coords$Y == 0
  if (any(coords$is_zero) & rlang::is_false(quiet)) {
    rlang::warn(c(
      "`data` contains points with co-ordinates at position `0, 0`.",
      "i" = paste(
        "These co-ordinates can indicate a problem with the data, e.g. an",
        "error during data recording or preparation."
      ),
      "i" = "Check data (e.g. by mapping) to ensure co-ordinates are correct."
    ))
  }

  # Validate `grid`
  if (!rlang::is_null(grid)) {
    if (!inherits(grid, "sf"))
      rlang::abort("`grid` must be either an SF object or `NULL`.", call = call)
    if (any(!sf::st_is(grid, "POLYGON")))
      rlang::abort(
        "`grid` must be `NULL` or an SF object containing polygons.",
        call = call
      )
    if (any(sf::st_is_empty(grid))) {
      rlang::abort(
        c(
          "`grid` contains empty geometries.",
          "i" = paste(
            "Identify and remove empty geometries, e.g. using `st_is_empty()`."
          )
        ),
        call = call
      )
    }
  }

  # Check `data` and `grid` use the same CRS
  # This is checked here because some functions of SF (e.g. `st_intersects()`)
  # can only work if the two layers use the same CRS, but we can provide a
  # more helpful error message
  if (!rlang::is_null(grid)) {
    if (sf::st_crs(data) != sf::st_crs(grid))
      rlang::abort(
        c(
          paste(
            "`data` and `grid` must use the same co-ordinate reference system",
            "(CRS)"
          ),
          "i" = paste0(
            "`data` uses the CRS '",
            format(sf::st_crs(data)),
            "' (",
            sf::st_crs(data, parameters = TRUE)$srid,
            ")"
          ),
          "i" = paste0(
            "`grid` uses the CRS '",
            format(sf::st_crs(grid)),
            "' (",
            sf::st_crs(grid, parameters = TRUE)$srid,
            ")"
          )
        ),
        call = call
      )
  }

  # Validate `quiet`
  if (!rlang::is_logical(quiet, n = 1))
    rlang::abort("`quiet` must be one of `TRUE` or `FALSE`.", call = call)

  invisible(NULL)

}
