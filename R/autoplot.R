plot_value_layer <- function(object, value, ...) {
  object$.plot_value <- value
  ggplot2::geom_sf(
    mapping = ggplot2::aes(fill = .data$.plot_value),
    data = object,
    colour = NA,
    inherit.aes = FALSE,
    ...
  )
}

validate_plot_column <- function(object, column) {
  validate_sf(object, label = "object", quiet = TRUE)
  if (!rlang::has_name(object, column)) {
    cli::cli_abort("{.var object} must contain a column called {.var {column}}")
  }
  if (!rlang::is_bare_numeric(object[[column]])) {
    cli::cli_abort(
      "The {.var {column}} column in {.var object} must be numeric"
    )
  }
}

symmetric_limits <- function(x) {
  finite_x <- x[is.finite(x)]
  max_abs <- if (length(finite_x) == 0) 0 else max(abs(finite_x))
  if (max_abs == 0) {
    max_abs <- 1
  }
  c(-max_abs, max_abs)
}

ratio_limits <- function(x) {
  finite_x <- x[is.finite(x) & x > 0]
  max_log <- if (length(finite_x) == 0) 0 else max(abs(log10(finite_x)))
  if (max_log == 0) {
    max_log <- log10(2)
  }
  c(10^-max_log, 10^max_log)
}

#' Plot map of grid counts
#'
#' Plot the output produced by [hotspot_count()] with reasonable default
#' values. Weighted counts are plotted when the object contains a `sum` column;
#' otherwise unweighted counts in the `n` column are plotted.
#'
#' @param object An object with class `hspt_n`, e.g. as produced by
#'   [hotspot_count()].
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` returns a [ggplot2::ggplot] object. `autolayer()`
#'   returns a layer that can be added to a [ggplot2::ggplot] object.
#' @export
autoplot.hspt_n <- function(object, ...) {
  weighted <- rlang::has_name(object, "sum")
  ggplot2::ggplot() +
    autolayer(object, ...) +
    ggplot2::scale_fill_distiller(
      type = "seq",
      palette = "Blues",
      direction = 1,
      limits = c(0, NA),
      na.value = "transparent"
    ) +
    ggplot2::labs(fill = if (weighted) "weighted count" else "count") +
    ggplot2::theme_void()
}

#' @describeIn autoplot.hspt_n Create a ggplot layer of grid counts.
#' @importFrom rlang .data
#' @export
autolayer.hspt_n <- function(object, ...) {
  value_column <- if (rlang::has_name(object, "sum")) "sum" else "n"
  validate_plot_column(object, value_column)
  plot_value <- object[[value_column]]
  plot_value[!is.finite(plot_value)] <- NA_real_
  plot_value_layer(object, plot_value, ...)
}

#' Plot map of kernel-density values
#'
#' Plot the output produced by [hotspot_kde()] with reasonable default values.
#'
#' @param object An object with class `hspt_k`, e.g. as produced by
#'   [hotspot_kde()].
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` returns a [ggplot2::ggplot] object. `autolayer()`
#'   returns a layer that can be added to a [ggplot2::ggplot] object.
#' @export
autoplot.hspt_k <- function(object, ...) {
  ggplot2::ggplot() +
    autolayer(object, ...) +
    ggplot2::scale_fill_distiller(
      type = "seq",
      palette = "Blues",
      direction = 1,
      breaks = range(object$kde, na.rm = TRUE),
      labels = c("low", "high"),
      na.value = "transparent"
    ) +
    ggplot2::labs(fill = "density") +
    ggplot2::theme_void()
}

#' @describeIn autoplot.hspt_k Create a ggplot layer of kernel-density values.
#' @importFrom rlang .data
#' @export
autolayer.hspt_k <- function(object, ...) {
  validate_plot_column(object, "kde")
  plot_value <- object$kde
  plot_value[!is.finite(plot_value)] <- NA_real_
  plot_value_layer(object, plot_value, ...)
}

hotspot_category_colours <- c(
  "persistent hotspot" = "#B2182B",
  "emerging hotspot" = "#D6604D",
  "intermittent hotspot" = "#EF8A62",
  "former hotspot" = "#FDDBC7",
  "no pattern" = "#F0F0F0",
  "former coldspot" = "#D1E5F0",
  "intermittent coldspot" = "#67A9CF",
  "emerging coldspot" = "#4393C3",
  "persistent coldspot" = "#2166AC",
  "mixed hot/coldspot" = "#762A83"
)

#' Plot map of hotspot classifications
#'
#' Plot the output produced by [hotspot_classify()] with reasonable defaults.
#'
#' @param object An object with class `hspt_c`, e.g. as produced by
#'   [hotspot_classify()].
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` returns a [ggplot2::ggplot] object. `autolayer()`
#'   returns a layer that can be added to a [ggplot2::ggplot] object.
#' @export
autoplot.hspt_c <- function(object, ...) {
  ggplot2::ggplot() +
    autolayer(object, ...) +
    ggplot2::scale_fill_manual(
      values = hotspot_category_colours,
      breaks = names(hotspot_category_colours),
      drop = FALSE,
      na.value = "transparent"
    ) +
    ggplot2::labs(fill = "hotspot category") +
    ggplot2::theme_void()
}

#' @describeIn autoplot.hspt_c Create a ggplot layer of hotspot classifications.
#' @importFrom rlang .data
#' @export
autolayer.hspt_c <- function(object, ...) {
  validate_sf(object, label = "object", quiet = TRUE)
  if (!rlang::has_name(object, "hotspot_category")) {
    cli::cli_abort(
      "{.var object} must contain a column called {.var hotspot_category}"
    )
  }
  if (!rlang::is_character(object$hotspot_category)) {
    cli::cli_abort(
      "The {.var hotspot_category} column in {.var object} must be character"
    )
  }
  unknown <- setdiff(
    unique(stats::na.omit(object$hotspot_category)),
    names(hotspot_category_colours)
  )
  if (length(unknown) > 0) {
    cli::cli_abort(
      "Unknown value{?s} in {.var hotspot_category}: {.val {unknown}}"
    )
  }
  ggplot2::geom_sf(
    mapping = ggplot2::aes(fill = .data$hotspot_category),
    data = object,
    colour = NA,
    inherit.aes = FALSE,
    ...
  )
}

#' Plot map of changes in grid counts
#'
#' Plot the output produced by [hotspot_change()] with reasonable defaults.
#'
#' @param object An object with class `hspt_d`, e.g. as produced by
#'   [hotspot_change()].
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` returns a [ggplot2::ggplot] object. `autolayer()`
#'   returns a layer that can be added to a [ggplot2::ggplot] object.
#' @export
autoplot.hspt_d <- function(object, ...) {
  validate_plot_column(object, "change")
  ggplot2::ggplot() +
    autolayer(object, ...) +
    ggplot2::scale_fill_gradient2(
      midpoint = 0,
      limits = symmetric_limits(object$change),
      na.value = "transparent"
    ) +
    ggplot2::labs(fill = "change\n(after \u2212 before)") +
    ggplot2::theme_void()
}

#' @describeIn autoplot.hspt_d Create a ggplot layer of change in grid counts.
#' @importFrom rlang .data
#' @export
autolayer.hspt_d <- function(object, ...) {
  validate_plot_column(object, "change")
  plot_value <- object$change
  plot_value[!is.finite(plot_value)] <- NA_real_
  plot_value_layer(object, plot_value, ...)
}

validate_dual_kde <- function(object) {
  validate_plot_column(object, "kde")
  method <- attr(object, "method", exact = TRUE)
  if (
    !rlang::is_character(method, n = 1) ||
      !method %in% c("ratio", "log", "diff", "sum")
  ) {
    cli::cli_abort(c(
      "{.var object} must have a valid {.attr method} attribute.",
      "i" = "Expected one of {.or {.val {c('ratio', 'log', 'diff', 'sum')}}}."
    ))
  }
  method
}

#' Plot map of dual kernel-density values
#'
#' Plot the output produced by [hotspot_dual_kde()] using a scale appropriate
#' to the comparison method. Ratios use a logarithmic diverging scale centred
#' on one; logged ratios and differences use diverging scales centred on zero;
#' sums use a sequential scale.
#'
#' @param object An object with class `hspt_dk`, e.g. as produced by
#'   [hotspot_dual_kde()]. The object must have a valid `method` attribute.
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` returns a [ggplot2::ggplot] object. `autolayer()`
#'   returns a layer that can be added to a [ggplot2::ggplot] object.
#' @export
autoplot.hspt_dk <- function(object, ...) {
  method <- validate_dual_kde(object)
  plot <- ggplot2::ggplot() + autolayer(object, ...)
  if (method == "ratio") {
    plot <- plot +
      ggplot2::scale_fill_gradient2(
        midpoint = 1,
        transform = "log10",
        limits = ratio_limits(object$kde),
        na.value = "transparent"
      )
    title <- "density ratio"
  } else if (method %in% c("log", "diff")) {
    plot <- plot +
      ggplot2::scale_fill_gradient2(
        midpoint = 0,
        limits = symmetric_limits(object$kde),
        na.value = "transparent"
      )
    title <- if (method == "log") "log density ratio" else "density difference"
  } else {
    plot <- plot +
      ggplot2::scale_fill_distiller(
        type = "seq",
        palette = "Blues",
        direction = 1,
        limits = c(0, NA),
        na.value = "transparent"
      )
    title <- "combined density"
  }
  plot + ggplot2::labs(fill = title) + ggplot2::theme_void()
}

#' @describeIn autoplot.hspt_dk Create a ggplot layer of dual density values.
#' @importFrom rlang .data
#' @export
autolayer.hspt_dk <- function(object, ...) {
  method <- validate_dual_kde(object)
  plot_value <- object$kde
  plot_value[!is.finite(plot_value)] <- NA_real_
  if (method == "ratio") {
    plot_value[plot_value <= 0] <- NA_real_
  }
  plot_value_layer(object, plot_value, ...)
}

validate_gistar_plot <- function(object, critical_p, sign) {
  validate_plot_column(object, "gistar")
  if (
    !rlang::is_bare_numeric(critical_p) ||
      length(critical_p) != 1 ||
      !is.finite(critical_p) ||
      critical_p <= 0 ||
      critical_p > 1
  ) {
    cli::cli_abort(paste(
      "{.arg critical_p} must be a single finite number greater than 0",
      "and no greater than 1"
    ))
  }
  if (!rlang::is_character(sign, n = 1)) {
    cli::cli_abort(
      "{.arg sign} must be one of {.or {.val {c('both', 'hot', 'cold')}}}"
    )
  }
  rlang::arg_match(sign, c("both", "hot", "cold"))
}

#' Plot map of Getis-Ord Gi* results
#'
#' If `object` contains a `kde` column, density is shown only in cells in which
#' the Gi*/Gi result passes the specified significance and sign conditions.
#' The `pvalue` column is used as supplied and is not adjusted by the plotting
#' methods. Cells that do not satisfy the conditions are transparent. If
#' `object` does not contain a `kde` column, the Gi*/Gi value is plotted using a
#' diverging scale centred on zero and `critical_p` and `sign` do not affect the
#' mapped values.
#'
#' @param object An object with class `hspt_g`, e.g. as produced by
#'   [hotspot_gistar()].
#' @param critical_p A single numeric value specifying the largest p-value to
#'   treat as statistically significant when plotting density.
#' @param sign Which significant results should show density: `"both"` (the
#'   default), `"hot"` for positive Gi*/Gi values, or `"cold"` for negative
#'   values.
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` returns a [ggplot2::ggplot] object. `autolayer()`
#'   returns a layer that can be added to a [ggplot2::ggplot] object.
#' @export
autoplot.hspt_g <- function(
  object,
  critical_p = 0.05,
  sign = c("both", "hot", "cold"),
  ...
) {
  sign <- rlang::arg_match(sign)
  validate_gistar_plot(object, critical_p, sign)
  has_kde <- rlang::has_name(object, "kde")
  plot <- ggplot2::ggplot() +
    autolayer(object, critical_p = critical_p, sign = sign, ...)
  if (has_kde) {
    plot <- plot +
      ggplot2::scale_fill_distiller(
        type = "seq",
        palette = "Blues",
        direction = 1,
        breaks = range(object$kde, na.rm = TRUE),
        labels = c("low", "high"),
        na.value = "transparent"
      )
    title <- "density*"
    caption <- switch(
      sign,
      hot = "* in areas with more points than expected by chance",
      cold = "* in areas with fewer points than expected by chance",
      both = "* in areas with more or fewer points than expected by chance"
    )
  } else {
    plot <- plot +
      ggplot2::scale_fill_gradient2(
        midpoint = 0,
        limits = symmetric_limits(object$gistar),
        na.value = "transparent"
      )
    title <- "Gi* statistic"
    caption <- NULL
  }
  plot + ggplot2::labs(fill = title, caption = caption) + ggplot2::theme_void()
}

#' @describeIn autoplot.hspt_g Create a ggplot layer of Getis-Ord Gi* results.
#' @importFrom rlang .data
#' @export
autolayer.hspt_g <- function(
  object,
  critical_p = 0.05,
  sign = c("both", "hot", "cold"),
  ...
) {
  sign <- rlang::arg_match(sign)
  validate_gistar_plot(object, critical_p, sign)
  if (rlang::has_name(object, "kde")) {
    validate_plot_column(object, "pvalue")
    validate_plot_column(object, "kde")
    include <- is.finite(object$pvalue) & object$pvalue < critical_p
    if (sign == "hot") {
      include <- include & object$gistar > 0
    }
    if (sign == "cold") {
      include <- include & object$gistar < 0
    }
    plot_value <- ifelse(include & is.finite(object$kde), object$kde, NA_real_)
  } else {
    plot_value <- object$gistar
    plot_value[!is.finite(plot_value)] <- NA_real_
  }
  plot_value_layer(object, plot_value, ...)
}

validate_isoband_plot <- function(object) {
  validate_sf(object, label = "object", quiet = TRUE)
  required <- c("lower", "upper", "band", "label")
  missing <- setdiff(required, names(object))
  if (length(missing) > 0) {
    cli::cli_abort(
      "{.var object} must contain {.and {.var {required}}} columns."
    )
  }
  if (
    !rlang::is_bare_numeric(object$lower) ||
      !rlang::is_bare_numeric(object$upper) ||
      !is.ordered(object$band) ||
      !is.ordered(object$label)
  ) {
    cli::cli_abort(
      paste(
        "{.var object} must contain numeric bounds and ordered",
        "{.var band} and {.var label} factors."
      )
    )
  }
  metadata <- attr(object, "isoband", exact = TRUE)
  if (!is.list(metadata) ||
      !metadata$plot_type %in% c(
        "sequential", "diverging_zero", "diverging_one"
      ) ||
      !rlang::is_character(metadata$title, n = 1)) {
    cli::cli_abort("{.var object} has missing or invalid isoband metadata.")
  }
  metadata
}

isoband_representatives <- function(lower, upper) {
  result <- (lower + upper) / 2
  finite_values <- c(lower, upper)[is.finite(c(lower, upper))]
  span <- if (length(finite_values) > 1) diff(range(finite_values)) else 1
  if (!is.finite(span) || span == 0) span <- 1
  result[is.infinite(lower)] <- upper[is.infinite(lower)] - span
  result[is.infinite(upper)] <- lower[is.infinite(upper)] + span
  result
}

isoband_colours <- function(object, metadata) {
  n <- nrow(object)
  if (metadata$plot_type == "sequential") {
    return(grDevices::colorRampPalette(c("#F7FBFF", "#08306B"))(n))
  }
  values <- isoband_representatives(object$lower, object$upper)
  midpoint <- metadata$midpoint
  limits <- range(c(values, midpoint), finite = TRUE)
  positions <- numeric(length(values))
  below <- values < midpoint
  above <- values > midpoint
  positions[values == midpoint] <- 0.5
  if (any(below)) {
    positions[below] <- 0.5 *
      (values[below] - limits[[1]]) / (midpoint - limits[[1]])
  }
  if (any(above)) {
    positions[above] <- 0.5 + 0.5 *
      (values[above] - midpoint) / (limits[[2]] - midpoint)
  }
  spanning <- object$lower < midpoint & object$upper > midpoint
  positions[spanning] <- 0.5
  grDevices::rgb(
    grDevices::colorRamp(c("#2166AC", "#F7F7F7", "#B2182B"))(positions),
    maxColorValue = 255
  )
}

#' Plot isobands
#'
#' Plot the output produced by [hotspot_isoband()] using a sequential or
#' diverging discrete scale appropriate to the original hotspot result and
#' selected value. Legend entries use the ordered `label` column containing
#' concise, automatically formatted ranges.
#'
#' @param object An object with class `hspt_ib`, as produced by
#'   [hotspot_isoband()].
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` returns a [ggplot2::ggplot] object. `autolayer()`
#'   returns a layer that can be added to a [ggplot2::ggplot] object.
#' @export
autoplot.hspt_ib <- function(object, ...) {
  metadata <- validate_isoband_plot(object)
  colours <- isoband_colours(object, metadata)
  visible_bands <- as.character(object$label)
  names(colours) <- visible_bands
  ggplot2::ggplot() +
    autolayer(object, ...) +
    ggplot2::scale_fill_manual(
      values = colours,
      breaks = visible_bands,
      drop = FALSE,
      na.value = "transparent"
    ) +
    ggplot2::labs(fill = metadata$title) +
    ggplot2::theme_void()
}

#' @describeIn autoplot.hspt_ib Create a ggplot layer of isobands.
#' @importFrom rlang .data
#' @export
autolayer.hspt_ib <- function(object, ...) {
  validate_isoband_plot(object)
  ggplot2::geom_sf(
    mapping = ggplot2::aes(fill = .data$label),
    data = object,
    colour = NA,
    inherit.aes = FALSE,
    ...
  )
}
