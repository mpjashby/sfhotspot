#' Validate the inputs given to hotspot functions
#'
#' Many of the user-facing functions in this package accept common arguments,
#' which must be validated before use. This function validates those arguments
#' and either throws the appropriate error or returns \code{NULL} invisibly.
#'
#' @param data \code{\link[sf]{sf}} data frame containing points.
#' @param quiet a single logical value.
#'
#' @noRd

validate_inputs <- function(data, quiet, ..., call = rlang::caller_env()) {

  # Validate `data`
  if (!inherits(data, "sf"))
    rlang::abort("`data` must be an SF object", call = call)
  if (any(!sf::st_is(data, "POINT")))
    rlang::abort("`data` must be an SF object containing points", call = call)
  if (any(sf::st_is_empty(data))) {
    rlang::abort(
      c(
        "`data` contains empty geometries",
        "i" = "identify and remove empty geometries, e.g. using `st_is_empty()`"
      ),
      call = call
    )
  }

  # Check for co-ordinates at Null Island or local equivalents
  coords <- as.data.frame(sf::st_coordinates(data))
  coords$is_zero <- coords$X == 0 & coords$Y == 0
  if (any(coords$is_zero) & rlang::is_false(quiet)) {
    rlang::warn(c(
      "`data` contains points with co-ordinates at position `0, 0`",
      "i" = paste(
        "these co-ordinates can indicate a problem with the data, e.g. an",
        "error during data recording or preparation"
      ),
      "i" = "check data (e.g. by mapping) to ensure co-ordinates are correct"
    ))
  }

  # Validate `quiet`
  if (!rlang::is_logical(quiet, n = 1))
    rlang::abort("`quiet` must be one of `TRUE` or `FALSE`", call = call)

  invisible(NULL)

}
