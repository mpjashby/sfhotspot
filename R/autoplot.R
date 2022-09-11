#' Plot map of grid counts
#'
#' Plot the output produced by \code{\link{hotspot_count}} with reasonable
#' default values.
#'
#' @param object An object with the class \code{hspt_n}, e.g. as produced by
#' \code{\link{hotspot_count}}.
#' @param ... further arguments passed to \code{\link[ggplot2]{geom_sf}}, e.g.
#'   \code{alpha}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object or layer that can be used as
#' part of a \code{\link[ggplot2]{ggplot}} stack.
#'
#' \code{autoplot} returns a \code{ggplot} object, meaning you can further
#' control the appearance of the plot by adding calls to further \code{ggplot2}
#' functions.
#'
#' @export
autoplot.hspt_n <- function(object, ...) {

  # Create plot
  ggplot2::ggplot() +
    autolayer(object, ...) +
    ggplot2::scale_colour_distiller(
      type = "seq",
      palette = "Blues",
      direction = 1,
      aesthetics = c("colour", "fill")
    ) +
    ggplot2::labs(
      colour = "count of\npoints in\neach cell",
      fill = "count of\npoints in\neach cell"
    ) +
    ggplot2::theme_minimal()

}



#' @describeIn autoplot.hspt_n Create a ggplot layer of grid counts
#' @importFrom rlang .data
autolayer.hspt_n <- function(object, ...) {

  # Validate inputs
  if (!inherits(object, "sf")) rlang::abort("`object` must be an SF object")
  if (!rlang::has_name(object, "n"))
    rlang::abort("`object` must contain a column called `n`")
  if (!rlang::is_double(object$n))
    rlang::abort("The `n` column in `object` must be numeric")

  # Create layer
  ggplot2::geom_sf(
    mapping = ggplot2::aes(colour = .data$n, fill = .data$n),
    data = object,
    inherit.aes = FALSE,
    ...
  )

}



#' Plot map of kernel-density values
#'
#' Plot the output produced by \code{\link{hotspot_kde}} with reasonable
#' default values.
#'
#' @param object An object with the class \code{hspt_k}, e.g. as produced by
#' \code{\link{hotspot_kde}}.
#' @param ... further arguments passed to \code{\link[ggplot2]{geom_sf}}, e.g.
#'   \code{alpha}.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object or layer that can be used as
#' part of a \code{\link[ggplot2]{ggplot}} stack.
#'
#' \code{autoplot} returns a \code{ggplot} object, meaning you can further
#' control the appearance of the plot by adding calls to further \code{ggplot2}
#' functions.
#'
#' @export
autoplot.hspt_k <- function(object, ...) {

  # Create plot
  ggplot2::ggplot() +
    autolayer(object, ...) +
    ggplot2::scale_colour_distiller(
      type = "seq",
      palette = "Blues",
      direction = 1,
      aesthetics = c("colour", "fill")
    ) +
    ggplot2::labs(
      colour = "estimated\ndensity\nof points",
      fill = "estimated\ndensity\nof points"
    ) +
    ggplot2::theme_minimal()

}



#' @describeIn autoplot.hspt_k Create a ggplot layer of kernel-density values
#' @importFrom rlang .data
autolayer.hspt_k <- function(object, ...) {

  # Validate inputs
  if (!inherits(object, "sf")) rlang::abort("`object` must be an SF object")
  if (!rlang::has_name(object, "kde"))
    rlang::abort("`object` must contain a column called `kde`")
  if (!rlang::is_double(object$kde))
    rlang::abort("The `kde` column in `object` must be numeric")

  # Create layer
  ggplot2::geom_sf(
    mapping = ggplot2::aes(colour = .data$kde, fill = .data$kde),
    data = object,
    inherit.aes = FALSE,
    ...
  )

}



#' Plot map of hotspot classifications
#'
#' Plot the output produced by \code{\link{hotspot_classify}} with reasonable
#' default values.
#'
#' @param object An object with the class \code{hspt_c}, e.g. as produced by
#'   \code{\link{hotspot_classify}}.
#' @param ... Currently ignored, but may be used for further options in future.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' This function returns a \code{ggplot} object, meaning you can further control
#' the appearance of the plot by adding calls to further \code{ggplot2}
#' functions.
#'
#' @importFrom rlang .data
#' @export
autoplot.hspt_c <- function(object, ...) {

  # Validate inputs
  if (!inherits(object, "sf")) rlang::abort("`object` must be an SF object")
  if (!rlang::has_name(object, "hotspot_category"))
    rlang::abort("`object` must contain a column called `hotspot_category`")

  # Create plot
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      mapping = ggplot2::aes(
        colour = .data$hotspot_category,
        fill = .data$hotspot_category
      ),
      data = object,
      ...
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "persistent hotspot" = "#D50032",
        "intermittent hotspot" = "#F6BE00",
        "emerging hotspot" = "#500778",
        "former hotspot" = "grey70",
        "no pattern" = "grey90"
      ),
      aesthetics = c("colour", "fill")
    ) +
    ggplot2::labs(
      colour = "hotspot\ncategory",
      fill = "hotspot\ncategory"
    ) +
    ggplot2::theme_minimal()

}



#' Plot map of changes in grid counts
#'
#' Plot the output produced by \code{\link{hotspot_change}} with reasonable
#' default values.
#'
#' @param object An object with the class \code{hspt_d}, e.g. as produced by
#'   \code{\link{hotspot_change}}.
#' @param ... Currently ignored, but may be used for further options in future.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object.
#'
#' This function returns a \code{ggplot} object, meaning you can further control
#' the appearance of the plot by adding calls to further \code{ggplot2}
#' functions.
#'
#' @importFrom rlang .data
#' @export
autoplot.hspt_d <- function(object, ...) {

  # Validate inputs
  if (!inherits(object, "sf")) rlang::abort("`object` must be an SF object")
  if (!rlang::has_name(object, "change"))
    rlang::abort("`object` must contain a column called `change`")
  if (!rlang::is_double(object$change))
    rlang::abort("The `change` column in `object` must be numeric")

  # Create plot
  ggplot2::ggplot() +
    autolayer(object, ...) +
    ggplot2::scale_colour_gradient2(aesthetics = c("colour", "fill")) +
    ggplot2::labs(
      colour = "difference\nin number\nof points",
      fill = "difference\nin number\nof points"
    ) +
    ggplot2::theme_minimal()

}



#' @describeIn autoplot.hspt_d Create a ggplot layer of change in grid counts
#' @importFrom rlang .data
autolayer.hspt_d <- function(object, ...) {

  # Validate inputs
  if (!inherits(object, "sf")) rlang::abort("`object` must be an SF object")
  if (!rlang::has_name(object, "change"))
    rlang::abort("`object` must contain a column called `change`")
  if (!rlang::is_double(object$change))
    rlang::abort("The `change` column in `object` must be numeric")

  # Create layer
  ggplot2::geom_sf(
    mapping = ggplot2::aes(colour = .data$change, fill = .data$change),
    data = object,
    inherit.aes = FALSE,
    ...
  )

}
