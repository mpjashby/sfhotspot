#' Automatically determine a suitable cell size for a two-dimensional grid
#'
#' @param data \code{\link[sf]{sf}} data frame containing points.
#' @param adjust single positive \code{numeric} value by which the returned
#'   value will subsequently be multiplied.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{TRUE}.
#'
#' @return A single numeric value representing the recommended bandwidth, using
#'   the same spatial units as specified in the co-ordinate reference system of
#'   the supplied \code{data} object.
#'
#' Note: although this function issues a message reporting the adjusted
#' bandwidth (i.e. \code{bandwidth * adjust}), the return value from this
#' function is (because of how this function is used in combination with others)
#' the \emph{unadjusted} bandwidth.
#'
#' @noRd

set_bandwidth <- function(data, adjust = 1, quiet = TRUE, label = "") {

  # Check inputs
  validate_inputs(data = data, quiet = quiet, call = rlang::caller_env())
  validate_bandwidth(adjust = adjust, call = rlang::caller_env())

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

  # Calculate bandwidth
  bandwidth <- bandwidth_nrd_sf(data)

  # Adjust bandwidth for reporting
  bandwidth_report <- ifelse(bandwidth > 1000, round(bandwidth), bandwidth)
  bandwidth_adjust <- ifelse(
    bandwidth * adjust > 1000,
    round(bandwidth * adjust),
    bandwidth * adjust
  )

  if (rlang::is_false(quiet)) {
    rlang::inform(paste0(
      ifelse(adjust != 1, "Adjusted bandwidth", "Bandwidth"),
      ifelse(label != "", paste0(" ", label), ""),
      " set to ", format(bandwidth_adjust, big.mark = ","), " ", unit_pl,
      ifelse(
        adjust != 1,
        paste0(
          " (", adjust, " * ", format(bandwidth_report, big.mark = ","), " ",
          unit_pl, ")"
        ),
        ""
      ),
      " automatically based on rule of thumb"
    ))
  }

  bandwidth

}
