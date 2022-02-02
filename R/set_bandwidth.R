#' Automatically determine a suitable cell size for a two-dimensional grid
#'
#' @param data \code{\link[sf]{sf}} data frame containing points.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{TRUE}.
#'
#' @return A single numeric value representing the recommended bandwidth, using
#'   the same spatial units as specified in the co-ordinate reference system of
#'   the supplied \code{data} object.
#'
#' @noRd

set_bandwidth <- function (data, quiet = TRUE) {

  # Check inputs
  if (!inherits(data, "sf"))
    rlang::abort("`data` must be an SF object")
  if (any(!sf::st_is(data, "POINT")))
    rlang::abort("`data` must be an SF object containing points")
  if (!rlang::is_logical(quiet))
    rlang::abort("`quiet` must be one of `TRUE` or `FALSE`")

  # Find spatial unit
  unit <- sf::st_crs(data, parameters = TRUE)$units_gdal
  unit_pl <- ifelse(
    unit %in% c("metre", "meter"),
    "metres",
    ifelse(
      unit %in% c("foot", "US survey foot"),
      "feet",
      ifelse(unit == "degree", "degrees", paste("(unit =", unit))
    )
  )

  bandwidth <- bandwidth_nrd_sf(data)

  if (rlang::is_false(quiet)) {
    rlang::inform(paste(
      "Bandwidth set to", format(bandwidth, big.mark = ","), unit_pl,
      "automatically based on rule of thumb"
    ))
  }

  bandwidth

}
