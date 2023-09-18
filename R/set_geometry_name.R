#' Set name of geometry column
#'
#' Many of the functions in this package assume that SF objects will have a
#' geometry column with the name `geometry`. This function changes the name of
#' the geometry column in an object to be called `geometry`. If the supplied
#' object is not an SF object, it is returned unchanged.
#'
#' @param x an object
#'
#' @noRd

set_geometry_name <- function(x) {

  if (inherits(x, "sf")) {
    sf::st_set_geometry(x, "geometry")
  } else {
    x
  }

}
