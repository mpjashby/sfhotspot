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

label_iso <- function(x, digits = NULL) {
  prefixes <- c(
    "-30" = "q",
    "-27" = "r",
    "-24" = "y",
    "-21" = "z",
    "-18" = "a",
    "-15" = "f",
    "-12" = "p",
    "-9" = "n",
    "-6" = "\u00b5",
    "-3" = "m",
    "0" = "",
    "3" = "k",
    "6" = "M",
    "9" = "G",
    "12" = "T",
    "15" = "P",
    "18" = "E",
    "21" = "Z",
    "24" = "Y",
    "27" = "R",
    "30" = "Q"
  )
  significant_digits <- if (is.null(digits)) 3L else digits
  vapply(
    x,
    function(value) {
      if (!is.finite(value)) {
        return(as.character(value))
      }
      si_exponent <- if (value == 0) {
        0
      } else {
        max(-30, min(30, 3 * floor(log10(abs(value)) / 3)))
      }
      scaled_value <- value / 10^si_exponent
      if (scaled_value == 0) {
        scaled_value <- 0
      }
      magnitude <- if (scaled_value == 0) {
        0
      } else {
        floor(log10(abs(scaled_value)))
      }
      decimal_places <- max(0, significant_digits - magnitude - 1L)
      number <- formatC(
        scaled_value,
        format = "f",
        digits = decimal_places,
        big.mark = ","
      )
      if (is.null(digits) && grepl(".", number, fixed = TRUE)) {
        number <- sub("0+$", "", number)
        number <- sub("\\.$", "", number)
      }
      paste0(number, prefixes[[as.character(si_exponent)]])
    },
    character(1)
  )
}

label_percent_significant <- function(x, digits = 2L) {
  vapply(
    x,
    function(value) {
      percentage <- signif(100 * value, digits = digits)
      paste0(format(percentage, trim = TRUE, scientific = FALSE), "%")
    },
    character(1)
  )
}

density_end_breaks <- function(limits) {
  finite_limits <- limits[is.finite(limits)]
  if (length(finite_limits) == 0) {
    return(numeric())
  }
  range(finite_limits)
}

keep_oob <- function(x, range = c(0, 1)) {
  x
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
  quiet = TRUE,
  layer_function = autolayer
) {
  if (!rlang::is_string(basemap_type)) {
    cli::cli_abort("{.arg basemap_type} must be a single string.")
  }
  if (!rlang::is_bool(quiet)) {
    cli::cli_abort("{.arg quiet} must be {.val TRUE} or {.val FALSE}.")
  }

  plot <- ggplot2::ggplot()
  if (basemap_type != "none") {
    validate_basemap_object(object)
    bbox <- sf::st_bbox(object)
    padding <- 0.1 *
      max(
        unname(bbox[["xmax"]] - bbox[["xmin"]]),
        unname(bbox[["ymax"]] - bbox[["ymin"]])
      )
    padded_bbox <- bbox
    padded_bbox[c("xmin", "ymin")] <-
      padded_bbox[c("xmin", "ymin")] - padding
    padded_bbox[c("xmax", "ymax")] <-
      padded_bbox[c("xmax", "ymax")] + padding
    basemap_bbox <- sf::st_bbox(
      sf::st_transform(sf::st_as_sfc(padded_bbox), 3857)
    )
    object <- sf::st_transform(object, 3857)
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
    plot + do.call(layer_function, c(list(object), layer_args))
  }
  plot <- if (quiet) suppressMessages(add_layer()) else add_layer()
  if (basemap_type != "none") {
    plot <- plot +
      ggplot2::scale_x_continuous(
        limits = unname(basemap_bbox[c("xmin", "xmax")]),
        expand = ggplot2::expansion(0),
        oob = keep_oob
      ) +
      ggplot2::scale_y_continuous(
        limits = unname(basemap_bbox[c("ymin", "ymax")]),
        expand = ggplot2::expansion(0),
        oob = keep_oob
      )
  }
  plot
}

#' Plot map of grid counts
#'
#' Plot the output produced by [hotspot_count()] with reasonable default
#' values. Weighted counts are plotted when the object contains a `sum` column;
#' otherwise unweighted counts in the `n` column are plotted.
#'
#' @param object An object with class `hspt_n`, e.g. as produced by
#'   [hotspot_count()].
#' @param mapping For `hotspot_map()`, this must be `NULL`. Hotspot-result
#'   objects determine their own aesthetic mappings.
#' @param caption For `hotspot_map()`, additional text to display in the map
#'   caption, or `NULL` for no additional text. Base-map attribution and any
#'   explanatory caption generated by the plotting method are retained, with
#'   explanatory text above this caption and attribution below it.
#' @param basemap_type A map type passed to the `type` argument of
#'   [ggspatial::annotation_map_tile()], or `"none"` for no base map.
#'   `autoplot()` defaults to `"none"` and `hotspot_map()` defaults to `"osm"`.
#'   A base map requires an internet connection unless the required tiles are
#'   cached. When a base map is used, the hotspot layer is drawn with
#'   `alpha = 0.75`, overriding any `alpha` value supplied in `...`.
#' @param basemap_zoom The zoom level passed to
#'   [ggspatial::annotation_map_tile()], or `NULL` to choose it automatically.
#' @param basemap_attribution Attribution for the tile provider, or `NULL` to
#'   use the statement known for `basemap_type`. A non-empty string is required
#'   for a custom or otherwise unknown map type.
#' @param quiet If `TRUE` (the default), suppress progress bars and other
#'   messages. If `FALSE`, show tile-download progress and other messages.
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` and `hotspot_map()` return [ggplot2::ggplot] objects.
#'   `autolayer()` returns a layer that can be added to a
#'   [ggplot2::ggplot] object.
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
      labels = label_iso,
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
#' @return `autoplot()` and `hotspot_map()` return [ggplot2::ggplot] objects.
#'   `autolayer()` returns a layer that can be added to a
#'   [ggplot2::ggplot] object.
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
      breaks = density_end_breaks,
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
#' @return `autoplot()` and `hotspot_map()` return [ggplot2::ggplot] objects.
#'   `autolayer()` returns a layer that can be added to a
#'   [ggplot2::ggplot] object.
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
#' @return `autoplot()` and `hotspot_map()` return [ggplot2::ggplot] objects.
#'   `autolayer()` returns a layer that can be added to a
#'   [ggplot2::ggplot] object.
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
      labels = label_iso,
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
#' to the comparison method. Ratios, logged ratios and sums use sequential
#' scales. Only differences use a diverging scale centred on zero. Legends
#' label the lower and upper ends of each scale as `"low"` and `"high"`,
#' respectively.
#'
#' @param object An object with class `hspt_dk`, e.g. as produced by
#'   [hotspot_dual_kde()]. The object must have a valid `method` attribute.
#' @inheritParams autoplot.hspt_n
#' @param ... Further arguments passed to [ggplot2::geom_sf()], e.g. `alpha`.
#' @return `autoplot()` and `hotspot_map()` return [ggplot2::ggplot] objects.
#'   `autolayer()` returns a layer that can be added to a
#'   [ggplot2::ggplot] object.
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
      ggplot2::scale_fill_distiller(
        type = "seq",
        palette = "Blues",
        direction = 1,
        breaks = density_end_breaks,
        labels = c("low", "high"),
        na.value = "transparent"
      )
    title <- "density ratio"
  } else if (method == "log") {
    plot <- plot +
      ggplot2::scale_fill_distiller(
        type = "seq",
        palette = "Blues",
        direction = 1,
        breaks = density_end_breaks,
        labels = c("low", "high"),
        na.value = "transparent"
      )
    title <- "log density ratio"
  } else if (method == "diff") {
    plot <- plot +
      ggplot2::scale_fill_gradient2(
        midpoint = 0,
        limits = symmetric_limits(object$kde),
        breaks = density_end_breaks,
        labels = c("low", "high"),
        na.value = "transparent"
      )
    title <- "density difference"
  } else {
    plot <- plot +
      ggplot2::scale_fill_distiller(
        type = "seq",
        palette = "Blues",
        direction = 1,
        limits = c(0, NA),
        breaks = density_end_breaks,
        labels = c("low", "high"),
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
#' methods. Cells that do not satisfy the conditions are transparent. When
#' `sign = "both"`, cold-spot densities are negated for plotting and a diverging
#' scale distinguishes cold spots from hot spots. When only one sign is shown,
#' a medium-to-dark sequential scale avoids making the least-dense significant
#' cells appear nearly white. If `object` does not contain a `kde` column, the
#' Gi*/Gi value is plotted using a diverging scale centred on zero and
#' `critical_p` and `sign` do not affect the mapped values.
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
#' @return `autoplot()` and `hotspot_map()` return [ggplot2::ggplot] objects.
#'   `autolayer()` returns a layer that can be added to a
#'   [ggplot2::ggplot] object.
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
    if (sign == "both") {
      plot <- plot +
        ggplot2::scale_fill_gradient2(
          low = "#2166AC",
          mid = "#F7F7F7",
          high = "#B2182B",
          midpoint = 0,
          limits = symmetric_limits(gistar_density_values(
            object,
            critical_p,
            sign
          )),
          breaks = density_end_breaks,
          labels = c("cold", "hot"),
          na.value = "transparent"
        )
    } else {
      plot <- plot +
        ggplot2::scale_fill_gradient(
          low = "#6BAED6",
          high = "#084594",
          breaks = density_end_breaks,
          labels = c("low", "high"),
          na.value = "transparent"
        )
    }
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
        labels = label_iso,
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
  plot +
    ggplot2::labs(fill = title, caption = caption) +
    ggplot2::theme_void()
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
    plot_value <- gistar_density_values(object, critical_p, sign)
  } else {
    plot_value <- object$gistar
    plot_value[!is.finite(plot_value)] <- NA_real_
  }
  plot_value_layer(object, plot_value, ...)
}

gistar_density_values <- function(object, critical_p, sign) {
  include <- is.finite(object$pvalue) & object$pvalue < critical_p
  if (sign == "hot") {
    include <- include & object$gistar > 0
  }
  if (sign == "cold") {
    include <- include & object$gistar < 0
  }
  values <- object$kde
  if (sign == "both") {
    values <- values * base::sign(object$gistar)
  }
  ifelse(include & is.finite(values), values, NA_real_)
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
#' @return `autoplot()` and `hotspot_map()` return [ggplot2::ggplot] objects.
#'   `autolayer()` returns a layer that can be added to a
#'   [ggplot2::ggplot] object.
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
#' @param col_fill A single string specifying the column used for the fill
#'   aesthetic: `"n"` (the default), `"prop"` or `"rank"`. Use `"none"` to
#'   draw unfilled polygons with the default ggplot2 border colour. Set
#'   `colour` in `...` to use a different border colour.
#' @param col_label One or more strings specifying the columns used for labels
#'   shown in each cluster:
#'   `"none"` (the default), or any combination of `"n"`, `"prop"` and
#'   `"rank"`. Multiple labels are separated by newlines. Proportions are
#'   formatted as percentages and ranks as ordinal numbers. Labels use a
#'   semi-transparent background chosen to contrast with the base map.
#' @param ... Static aesthetics and other arguments passed to
#'   [ggplot2::geom_sf()], e.g. `colour`, `fill` or `alpha`.
#' @return `autoplot()` and `hotspot_map()` return [ggplot2::ggplot] objects.
#'   `autolayer()` returns one or more layers that can be added to a
#'   [ggplot2::ggplot] object.
#' @export
autoplot.hspt_s <- function(
  object,
  col_fill = c("n", "prop", "rank", "none"),
  col_label = "none",
  ...,
  basemap_type = "none",
  basemap_zoom = NULL,
  basemap_attribution = NULL,
  quiet = TRUE
) {
  col_fill <- rlang::arg_match(col_fill)
  col_label <- match_dbscan_labels(col_label)
  label_style <- dbscan_label_style(basemap_type)
  layer_function <- function(object, col_fill, col_label, ...) {
    dbscan_layers(
      object,
      col_fill,
      col_label,
      label_fill = label_style$fill,
      label_colour = label_style$colour,
      ...
    )
  }
  plot <- build_autoplot(
    object,
    layer_args = c(
      list(col_fill = col_fill, col_label = col_label),
      list(...)
    ),
    basemap_type = basemap_type,
    basemap_zoom = basemap_zoom,
    basemap_attribution = basemap_attribution,
    quiet = quiet,
    layer_function = layer_function
  )
  if (col_fill != "none") {
    scale_title <- switch(
      col_fill,
      n = "count",
      prop = "proportion",
      rank = "rank"
    )
    scale_labels <- if (col_fill == "prop") {
      label_percent_significant
    } else {
      label_iso
    }
    plot <- plot +
      ggplot2::scale_fill_distiller(
        type = "seq",
        palette = "Blues",
        direction = 1,
        labels = scale_labels,
        na.value = "transparent"
      ) +
      ggplot2::labs(fill = scale_title)
  }

  plot + ggplot2::theme_void()
}

match_dbscan_labels <- function(col_label) {
  col_label <- rlang::arg_match(
    col_label,
    c("none", "n", "prop", "rank"),
    multiple = TRUE
  )
  if ("none" %in% col_label && length(col_label) > 1) {
    cli::cli_abort(
      "{.val none} cannot be combined with other values in {.arg col_label}."
    )
  }
  if (anyDuplicated(col_label)) {
    cli::cli_abort("Values in {.arg col_label} must not be duplicated.")
  }
  col_label
}

dbscan_label_style <- function(basemap_type) {
  dark <- basemap_type %in% "cartodark"
  list(
    fill = grDevices::adjustcolor(
      if (dark) "grey20" else "white",
      alpha.f = 0.75
    ),
    colour = if (dark) "white" else "black"
  )
}

format_dbscan_labels <- function(value, type) {
  if (type == "prop") {
    return(label_percent_significant(value))
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

dbscan_layers <- function(
  object,
  col_fill,
  col_label,
  label_fill,
  label_colour,
  ...
) {
  if (col_fill == "none") {
    dots <- list(...)
    dots$data <- object
    dots$fill <- NA
    dots$inherit.aes <- FALSE
    if (is.null(dots$linewidth)) {
      dots$linewidth <- 0.8
    }
    polygon_layer <- do.call(ggplot2::geom_sf, dots)
  } else {
    validate_plot_column(object, col_fill)
    plot_value <- object[[col_fill]]
    plot_value[!is.finite(plot_value)] <- NA_real_
    polygon_layer <- plot_value_layer(object, plot_value, ...)
  }
  if (col_label[[1]] == "none") {
    return(polygon_layer)
  }

  formatted_labels <- lapply(col_label, function(type) {
    validate_plot_column(object, type)
    value <- format_dbscan_labels(object[[type]], type)
    if (type == "n" && length(col_label) > 1) {
      value <- paste0("n = ", value)
    }
    value
  })
  object$.plot_label <- do.call(paste, c(formatted_labels, sep = "\n"))
  list(
    polygon_layer,
    ggplot2::geom_sf_label(
      mapping = ggplot2::aes(label = .data$.plot_label),
      data = object,
      fun.geometry = dbscan_label_point,
      fill = label_fill,
      colour = label_colour,
      linewidth = 0,
      inherit.aes = FALSE
    )
  )
}

#' @describeIn autoplot.hspt_s Create ggplot layers for DBSCAN hotspot clusters.
#' @importFrom rlang .data
#' @export
autolayer.hspt_s <- function(
  object,
  col_fill = c("n", "prop", "rank", "none"),
  col_label = "none",
  ...
) {
  col_fill <- rlang::arg_match(col_fill)
  col_label <- match_dbscan_labels(col_label)
  label_style <- dbscan_label_style("none")
  dbscan_layers(
    object,
    col_fill,
    col_label,
    label_fill = label_style$fill,
    label_colour = label_style$colour,
    ...
  )
}
