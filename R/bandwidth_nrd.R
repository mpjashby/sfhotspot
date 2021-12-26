#' Choose bandwidth for kernel-density estimation using a rule of thumb
#'
#' The code here is identical to that in \code{\link[MASS]{bandwidth.nrd}}
#'
#' @param data \code{\link[sf]{sf}} data frame containing points.

bandwidth_nrd_sf <- function (data) {

  coords <- suppressWarnings(sf::st_coordinates(data))

  mean(bandwidth_nrd(coords[, 1]), bandwidth_nrd(coords[, 2]))

}

bandwidth_nrd <- function (x) {

  r <- stats::quantile(x, c(0.25, 0.75))
  h <- (r[2] - r[1])/1.34
  4 * 1.06 * min(sqrt(stats::var(x)), h) * length(x)^(-1/5)

}
