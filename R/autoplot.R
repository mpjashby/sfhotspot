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

map_tile_attribution <- function(type, attribution = NULL) {
  if (!is.null(attribution)) {
    if (!rlang::is_string(attribution) || !nzchar(attribution)) {
      cli::cli_abort(
        "{.arg basemap_attribution} must be a single non-empty string."
      )
    }
    return(attribution)
  }

  attributions <- c(
    osm = "\u00a9 OpenStreetMap contributors",
    opencycle = paste(
      "\u00a9 OpenStreetMap contributors;",
      "map tiles \u00a9 Thunderforest"
    ),
    hotstyle = paste(
      "\u00a9 OpenStreetMap contributors;",
      "map tiles \u00a9 Humanitarian OpenStreetMap Team"
    ),
    loviniahike = paste(
      "\u00a9 OpenStreetMap contributors;",
      "map tiles \u00a9 Waymarked Trails"
    ),
    loviniacycle = paste(
      "\u00a9 OpenStreetMap contributors;",
      "map tiles \u00a9 Waymarked Trails"
    ),
    stamenbw = paste(
      "Map tiles by Stamen Design (CC BY 3.0);",
      "data \u00a9 OpenStreetMap contributors"
    ),
    stamenwatercolor = paste(
      "Map tiles by Stamen Design (CC BY 3.0);",
      "data \u00a9 OpenStreetMap contributors"
    ),
    osmtransport = paste(
      "\u00a9 OpenStreetMap contributors;",
      "map tiles \u00a9 Thunderforest"
    ),
    thunderforestlandscape = paste(
      "\u00a9 OpenStreetMap contributors;",
      "map tiles \u00a9 Thunderforest"
    ),
    thunderforestoutdoors = paste(
      "\u00a9 OpenStreetMap contributors;",
      "map tiles \u00a9 Thunderforest"
    ),
    cartodark = paste(
      "Map tiles \u00a9 CARTO;",
      "data \u00a9 OpenStreetMap contributors"
    ),
    cartolight = paste(
      "Map tiles \u00a9 CARTO;",
      "data \u00a9 OpenStreetMap contributors"
    )
  )
  if (!rlang::is_string(type) || !type %in% names(attributions)) {
    cli::cli_abort(c(
      "No attribution is known for {.arg basemap_type} {.val {type}}.",
      "i" = "Attribution is usually a legal requirement for using tiles.",
      "i" = paste(
        "Supply the provider's required attribution using",
        "{.arg basemap_attribution}."
      )
    ))
  }
  unname(attributions[[type]])
}

build_autoplot <- function(
  object,
  layer_args = list(),
  basemap_type = "none",
  basemap_zoom = NULL,
  basemap_attribution = NULL,
  quiet = TRUE
) {
  if (!rlang::is_string(basemap_type)) {
    cli::cli_abort("{.arg basemap_type} must be a single string.")
  }
  if (!rlang::is_bool(quiet)) {
    cli::cli_abort("{.arg quiet} must be {.val TRUE} or {.val FALSE}.")
  }

  plot <- ggplot2::ggplot()
  if (basemap_type != "none") {
    rlang::check_installed(
      "ggspatial",
      reason = "to add a base map"
    )
    tile_args <- list(
      type = basemap_type,
      zoom = basemap_zoom,
      zoomin = 0,
      progress = if (quiet) "none" else "text"
    )
    plot <- plot + do.call(ggspatial::annotation_map_tile, tile_args)
    layer_args$alpha <- 0.75
    plot <- plot +
      ggplot2::labs(
        caption = map_tile_attribution(basemap_type, basemap_attribution)
      )
  }

  add_layer <- function() {
    plot + do.call(autolayer, c(list(object), layer_args))
  }
  if (quiet) suppressMessages(add_layer()) else add_layer()
}

#' Plot map of grid counts
#'
#' Plot the output produced by [hotspot_count()] with reasonable default
#' values. Weighted counts are plotted when the object contains a `sum` column;
#' otherwise unweighted counts in the `n` column are plotted.
#'
#' @param object An object with class `hspt_n`, e.g. as produced by
#'   [hotspot_count()].
#' @param basemap_type A map type passed to the `type` argument of
#'   [ggspatial::annotation_map_tile()], or `"none"` (the default) for no base
#'   map. A base map requires the suggested `ggspatial` package and an internet
#'   connection unless the required tiles are cached. When a base map is used,
#'   the hotspot layer is drawn with `alpha = 0.75`, overriding any `alpha`
#'   value supplied in `...`.
#' @param basemap_zoom The zoom level passed to
#'   [ggspatial::annotation_map_tile()], or `NULL` to choose it automatically.
#' @param basemap_attribution Attribution for the tile provider, or `NULL` to
#'   use the statement known for `basemap_type`. A non-empty string is required
#'   for a custom or otherwise unknown map type.
#' @param quiet If `TRUE` (the default), suppress progress bars and other
#'   messages. If `FALSE`, show tile-download progress and other messages.
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` returns a [ggplot2::ggplot] object. `autolayer()`
#'   returns a layer that can be added to a [ggplot2::ggplot] object.
#' @export
autoplot.hspt_n <- function(
  object,
  ...,
  basemap_type = "none",
  basemap_zoom = NULL,
  basemap_attribution = NULL,
  quiet = TRUE
) {
  weighted <- rlang::has_name(object, "sum")
  build_autoplot(
    object,
    layer_args = list(...),
    basemap_type = basemap_type,
    basemap_zoom = basemap_zoom,
    basemap_attribution = basemap_attribution,
    quiet = quiet
  ) +
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
#' @inheritParams autoplot.hspt_n
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` returns a [ggplot2::ggplot] object. `autolayer()`
#'   returns a layer that can be added to a [ggplot2::ggplot] object.
#' @export
autoplot.hspt_k <- function(
  object,
  ...,
  basemap_type = "none",
  basemap_zoom = NULL,
  basemap_attribution = NULL,
  quiet = TRUE
) {
  build_autoplot(
    object,
    layer_args = list(...),
    basemap_type = basemap_type,
    basemap_zoom = basemap_zoom,
    basemap_attribution = basemap_attribution,
    quiet = quiet
  ) +
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
#' @inheritParams autoplot.hspt_n
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` returns a [ggplot2::ggplot] object. `autolayer()`
#'   returns a layer that can be added to a [ggplot2::ggplot] object.
#' @export
autoplot.hspt_c <- function(
  object,
  ...,
  basemap_type = "none",
  basemap_zoom = NULL,
  basemap_attribution = NULL,
  quiet = TRUE
) {
  build_autoplot(
    object,
    layer_args = list(...),
    basemap_type = basemap_type,
    basemap_zoom = basemap_zoom,
    basemap_attribution = basemap_attribution,
    quiet = quiet
  ) +
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
#' @inheritParams autoplot.hspt_n
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` returns a [ggplot2::ggplot] object. `autolayer()`
#'   returns a layer that can be added to a [ggplot2::ggplot] object.
#' @export
autoplot.hspt_d <- function(
  object,
  ...,
  basemap_type = "none",
  basemap_zoom = NULL,
  basemap_attribution = NULL,
  quiet = TRUE
) {
  validate_plot_column(object, "change")
  build_autoplot(
    object,
    layer_args = list(...),
    basemap_type = basemap_type,
    basemap_zoom = basemap_zoom,
    basemap_attribution = basemap_attribution,
    quiet = quiet
  ) +
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
#' @inheritParams autoplot.hspt_n
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` returns a [ggplot2::ggplot] object. `autolayer()`
#'   returns a layer that can be added to a [ggplot2::ggplot] object.
#' @export
autoplot.hspt_dk <- function(
  object,
  ...,
  basemap_type = "none",
  basemap_zoom = NULL,
  basemap_attribution = NULL,
  quiet = TRUE
) {
  method <- validate_dual_kde(object)
  plot <- build_autoplot(
    object,
    layer_args = list(...),
    basemap_type = basemap_type,
    basemap_zoom = basemap_zoom,
    basemap_attribution = basemap_attribution,
    quiet = quiet
  )
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
#' @inheritParams autoplot.hspt_n
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
  ...,
  basemap_type = "none",
  basemap_zoom = NULL,
  basemap_attribution = NULL,
  quiet = TRUE
) {
  sign <- rlang::arg_match(sign)
  validate_gistar_plot(object, critical_p, sign)
  has_kde <- rlang::has_name(object, "kde")
  plot <- build_autoplot(
    object,
    layer_args = c(
      list(critical_p = critical_p, sign = sign),
      list(...)
    ),
    basemap_type = basemap_type,
    basemap_zoom = basemap_zoom,
    basemap_attribution = basemap_attribution,
    quiet = quiet
  )
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
  if (!is.null(caption) && !is.null(plot$labels$caption)) {
    caption <- paste(caption, plot$labels$caption, sep = "\n")
  } else if (is.null(caption)) {
    caption <- plot$labels$caption
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
  if (
    !is.list(metadata) ||
      !metadata$plot_type %in%
        c(
          "sequential",
          "diverging_zero",
          "diverging_one"
        ) ||
      !rlang::is_character(metadata$title, n = 1)
  ) {
    cli::cli_abort("{.var object} has missing or invalid isoband metadata.")
  }
  metadata
}

isoband_representatives <- function(lower, upper) {
  result <- (lower + upper) / 2
  finite_values <- c(lower, upper)[is.finite(c(lower, upper))]
  span <- if (length(finite_values) > 1) diff(range(finite_values)) else 1
  if (!is.finite(span) || span == 0) {
    span <- 1
  }
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
      (values[below] - limits[[1]]) /
      (midpoint - limits[[1]])
  }
  if (any(above)) {
    positions[above] <- 0.5 +
      0.5 *
        (values[above] - midpoint) /
        (limits[[2]] - midpoint)
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
#' @inheritParams autoplot.hspt_n
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` returns a [ggplot2::ggplot] object. `autolayer()`
#'   returns a layer that can be added to a [ggplot2::ggplot] object.
#' @export
autoplot.hspt_ib <- function(
  object,
  ...,
  basemap_type = "none",
  basemap_zoom = NULL,
  basemap_attribution = NULL,
  quiet = TRUE
) {
  metadata <- validate_isoband_plot(object)
  colours <- isoband_colours(object, metadata)
  visible_bands <- as.character(object$label)
  names(colours) <- visible_bands
  build_autoplot(
    object,
    layer_args = list(...),
    basemap_type = basemap_type,
    basemap_zoom = basemap_zoom,
    basemap_attribution = basemap_attribution,
    quiet = quiet
  ) +
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

#' Plot DBSCAN hotspot clusters
#'
#' Plot the polygon clusters produced by [hotspot_dbscan()] with reasonable
#' defaults. Polygons can be filled according to their count, proportion or
#' rank, and optionally labelled with the same values.
#'
#' @param object An object with class `hspt_s`, as produced by
#'   [hotspot_dbscan()].
#' @inheritParams autoplot.hspt_n
#' @param fill A single string specifying the column used for the fill
#'   aesthetic: `"n"` (the default), `"prop"` or `"rank"`. Use `"none"` to
#'   draw unfilled polygons with borders coloured according to the first value
#'   in `label`, or according to `"n"` if labels are not shown.
#' @param label One or more strings specifying the labels shown in each cluster:
#'   `"none"` (the default), or any combination of `"n"`, `"prop"` and
#'   `"rank"`. Multiple labels are separated by newlines. Proportions are
#'   formatted as percentages and ranks as ordinal numbers.
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` returns a [ggplot2::ggplot] object. `autolayer()`
#'   returns one or more layers that can be added to a [ggplot2::ggplot] object.
#' @export
autoplot.hspt_s <- function(
  object,
  fill = c("n", "prop", "rank", "none"),
  label = "none",
  ...,
  basemap_type = "none",
  basemap_zoom = NULL,
  basemap_attribution = NULL,
  quiet = TRUE
) {
  fill <- rlang::arg_match(fill)
  label <- match_dbscan_labels(label)
  plot <- build_autoplot(
    object,
    layer_args = c(list(fill = fill, label = label), list(...)),
    basemap_type = basemap_type,
    basemap_zoom = basemap_zoom,
    basemap_attribution = basemap_attribution,
    quiet = quiet
  )
  scale_title <- switch(
    if (fill == "none" && label[[1]] == "none") {
      "n"
    } else if (fill == "none") {
      label[[1]]
    } else {
      fill
    },
    n = "count",
    prop = "proportion",
    rank = "rank"
  )

  if (fill == "none") {
    plot <- plot +
      ggplot2::scale_colour_distiller(
        type = "seq",
        palette = "Blues",
        direction = 1,
        na.value = "transparent"
      ) +
      ggplot2::labs(colour = scale_title)
  } else {
    plot <- plot +
      ggplot2::scale_fill_distiller(
        type = "seq",
        palette = "Blues",
        direction = 1,
        na.value = "transparent"
      ) +
      ggplot2::labs(fill = scale_title)
  }

  plot + ggplot2::theme_void()
}

match_dbscan_labels <- function(label) {
  label <- rlang::arg_match(
    label,
    c("none", "n", "prop", "rank"),
    multiple = TRUE
  )
  if ("none" %in% label && length(label) > 1) {
    cli::cli_abort(
      "{.val none} cannot be combined with other values in {.arg label}."
    )
  }
  if (anyDuplicated(label)) {
    cli::cli_abort("Values in {.arg label} must not be duplicated.")
  }
  label
}

format_dbscan_labels <- function(value, type) {
  if (type == "prop") {
    return(paste0(format(100 * value, digits = 3, trim = TRUE), "%"))
  }
  if (type == "rank") {
    remainder_100 <- value %% 100
    suffix <- ifelse(
      remainder_100 >= 11 & remainder_100 <= 13,
      "th",
      c("th", "st", "nd", "rd", rep("th", 6))[value %% 10 + 1]
    )
    return(paste0(value, suffix))
  }
  as.character(value)
}

dbscan_label_point <- function(x) {
  withCallingHandlers(
    sf::st_point_on_surface(sf::st_zm(x)),
    warning = function(cnd) {
      if (
        identical(
          conditionMessage(cnd),
          paste(
            "st_point_on_surface may not give correct results for",
            "longitude/latitude data"
          )
        )
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

#' @describeIn autoplot.hspt_s Create ggplot layers for DBSCAN hotspot clusters.
#' @importFrom rlang .data
#' @export
autolayer.hspt_s <- function(
  object,
  fill = c("n", "prop", "rank", "none"),
  label = "none",
  ...
) {
  fill <- rlang::arg_match(fill)
  label <- match_dbscan_labels(label)
  plot_column <- if (fill == "none") {
    if (label[[1]] == "none") "n" else label[[1]]
  } else {
    fill
  }
  validate_plot_column(object, plot_column)

  plot_value <- object[[plot_column]]
  plot_value[!is.finite(plot_value)] <- NA_real_
  if (fill == "none") {
    object$.plot_value <- plot_value
    dots <- list(...)
    dots$mapping <- ggplot2::aes(colour = .data$.plot_value)
    dots$data <- object
    dots$fill <- NA
    dots$inherit.aes <- FALSE
    if (is.null(dots$linewidth)) {
      dots$linewidth <- 0.8
    }
    polygon_layer <- do.call(ggplot2::geom_sf, dots)
  } else {
    polygon_layer <- plot_value_layer(object, plot_value, ...)
  }
  if (label[[1]] == "none") {
    return(polygon_layer)
  }

  formatted_labels <- lapply(label, function(type) {
    validate_plot_column(object, type)
    value <- format_dbscan_labels(object[[type]], type)
    if (type == "n" && length(label) > 1) {
      value <- paste0("n = ", value)
    }
    value
  })
  object$.plot_label <- do.call(paste, c(formatted_labels, sep = "\n"))
  list(
    polygon_layer,
    ggplot2::geom_sf_text(
      mapping = ggplot2::aes(label = .data$.plot_label),
      data = object,
      fun.geometry = dbscan_label_point,
      inherit.aes = FALSE
    )
  )
}
