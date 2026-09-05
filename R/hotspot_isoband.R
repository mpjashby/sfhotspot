#' Convert a hotspot grid to isobands
#'
#' Generalise values in a regular square grid into polygon bands. The result is
#' an [sf::sf] object with one row for each non-empty band and can be plotted
#' with [autoplot()] or [autolayer()].
#'
#' @param data An [sf::sf] object containing a regular square grid and at least
#'   one numeric column, typically produced by a `hotspot_*()` function.
#' @param value The unquoted name of the numeric column to convert. If `NULL`,
#'   a suitable column is inferred from the sfhotspot class of `data`, or from
#'   the sole numeric column in an otherwise unrecognised SF object.
#' @param breaks Either a single positive integer giving the requested number
#'   of bands (five by default), or a strictly increasing numeric vector giving
#'   the band boundaries. If multiple boundaries are supplied, `style` is
#'   silently ignored.
#' @param style Method used to calculate breaks when `breaks` has length one.
#'   Values other than `"pconventions"` are passed to the `style` argument of
#'   [classInt::classIntervals()]. `"pconventions"` uses two-sided normal-theory
#'   thresholds corresponding to p-values of 0.1, 0.05, 0.01 and 0.001 and
#'   ignores `breaks`.
#' @param critical_p For output from [hotspot_gistar()] containing a `kde`
#'   column, the largest p-value for a cell to be included. The default is
#'   `0.05`. This argument is ignored for other inputs.
#' @param quiet If `TRUE`, suppress informative messages about automatically
#'   selected values and potentially unhelpful intervals.
#'
#' @details
#' The default `style = "equal"` divides the observed range into equal-width
#' intervals, providing a discrete analogue of a linear continuous colour
#' scale. Extreme values can make equal-width bands uninformative; a message is
#' produced when at least 90% of finite values fall in one calculated band.
#'
#' For change values, Gi*/Gi statistics, and dual-KDE differences or logged
#' ratios, automatically calculated breaks place zero at a boundary or at the
#' centre of a band. Explicit break vectors are never altered.
#'
#' When `data` is output from [hotspot_gistar()] and contains KDE values, KDE
#' is converted and cells with `pvalue >= critical_p` are excluded before the
#' bands are calculated. Without KDE values, the Gi*/Gi statistic is converted.
#'
#' Values are interpolated between grid-cell centres. Lattice positions absent
#' from a grid clipped to its original analysis area are treated as missing.
#' Consequently, the isobands need not cover the complete area of every outer
#' grid cell.
#'
#' @return An `sf` tibble with class `hspt_ib`. It has one row per non-empty
#'   band and columns `lower`, `upper`, `band`, `label`, and `geometry`. The
#'   `band` and `label` columns are ordered factors; `band` contains technical
#'   interval notation and `label` contains ranges formatted for display.
#'
#' @examples
#' \donttest{
#' memphis_robberies_jan |>
#'   hotspot_kde() |>
#'   hotspot_isoband() |>
#'   autoplot()
#' }
#'
#' @export
hotspot_isoband <- function(
  data,
  value = NULL,
  breaks = 5,
  style = "equal",
  critical_p = 0.05,
  quiet = FALSE
) {
  # Validate arguments that apply to every type of input
  validate_sf(
    data,
    type = c("POLYGON", "MULTIPOLYGON"),
    quiet = quiet,
    call = rlang::caller_env()
  )
  if (!rlang::is_logical(quiet, n = 1)) {
    cli::cli_abort("{.arg quiet} must be {.code TRUE} or {.code FALSE}.")
  }
  if (
    !rlang::is_bare_numeric(critical_p) ||
      length(critical_p) != 1 ||
      !is.finite(critical_p) ||
      critical_p <= 0 ||
      critical_p > 1
  ) {
    cli::cli_abort(paste(
      "{.arg critical_p} must be a single finite number greater than 0",
      "and no greater than 1."
    ))
  }
  if (!rlang::is_character(style, n = 1)) {
    cli::cli_abort("{.arg style} must be a single character value.")
  }

  # Identify the originating hotspot function so that the most useful value
  # column and later plotting defaults can be inferred
  source_class <- isoband_source_class(data)

  # Resolve the value column, using the class-based default when the user has
  # not supplied a column name
  value_quo <- rlang::enquo(value)
  if (rlang::quo_is_null(value_quo)) {
    value_name <- isoband_default_value(data, source_class, quiet)
  } else {
    value_name <- tryCatch(
      rlang::as_name(value_quo),
      error = function(cnd) {
        cli::cli_abort(
          "{.arg value} must be the unquoted name of one column in {.arg data}.",
          parent = cnd
        )
      }
    )
  }
  if (!rlang::has_name(data, value_name)) {
    cli::cli_abort(
      "{.arg value} must name a column in {.arg data}; {.var {value_name}} does not exist."
    )
  }
  if (!rlang::is_bare_numeric(data[[value_name]])) {
    cli::cli_abort(
      "The {.var {value_name}} column selected by {.arg value} must be numeric."
    )
  }

  # Retain dual-KDE method metadata because differences, ratios and sums need
  # different treatment when selecting breaks and plotting the result
  values <- data[[value_name]]
  source_method <- if (source_class == "hspt_dk") {
    validate_dual_kde(data)
  } else {
    NULL
  }

  # Gi*/Gi output containing KDE values is converted only for cells that meet
  # the requested significance threshold; missing lattice positions are later
  # passed to isoband as NA values
  if (source_class == "hspt_g" && value_name == "kde") {
    validate_plot_column(data, "pvalue")
    values[!is.finite(data$pvalue) | data$pvalue >= critical_p] <- NA_real_
  }
  finite_values <- values[is.finite(values)]
  if (length(finite_values) == 0) {
    cli::cli_abort(c(
      "No finite values are available to create isobands.",
      "i" = if (source_class == "hspt_g" && value_name == "kde") {
        "No cells have {.var pvalue} less than {.arg critical_p}."
      } else {
        "Check the {.var {value_name}} column in {.arg data}."
      }
    ))
  }
  if (length(unique(finite_values)) < 2) {
    cli::cli_abort(
      "The {.var {value_name}} column must contain at least two distinct finite values."
    )
  }

  # Reconstruct the rectangular value matrix required by isoband from the
  # centres of the SF grid cells
  lattice <- isoband_lattice(data)
  z <- matrix(
    NA_real_,
    nrow = length(lattice$y),
    ncol = length(lattice$x)
  )
  z[cbind(lattice$row, lattice$column)] <- values

  # Signed statistics have a meaningful zero. Automatically calculated breaks
  # must expose that meaning without forcing the complete range to be symmetric
  meaningful_zero <-
    (source_class == "hspt_d" && value_name == "change") ||
    (source_class == "hspt_g" && value_name == "gistar") ||
    (source_class == "hspt_dk" &&
      value_name == "kde" &&
      source_method %in% c("diff", "log"))
  boundaries <- isoband_breaks(
    finite_values,
    breaks = breaks,
    style = style,
    meaningful_zero = meaningful_zero,
    quiet = quiet
  )

  # Diagnose classifications in which extreme values or unused intervals are
  # likely to make the resulting map less informative
  occupancy <- tabulate(
    findInterval(finite_values, boundaries, all.inside = TRUE),
    nbins = length(boundaries) - 1
  )
  if (
    !quiet &&
      length(breaks) == 1 &&
      breaks >= 3 &&
      style == "equal" &&
      max(occupancy) / length(finite_values) >= 0.9
  ) {
    cli::cli_inform(c(
      "!" = "The calculated equal-width bands may be uninformative.",
      "i" = "At least 90% of values fall in a single band.",
      "i" = "The range may be dominated by extreme values; consider another {.arg style} or explicit {.arg breaks}."
    ))
  }
  empty_numeric <- which(occupancy == 0)
  if (!quiet && length(empty_numeric) > 0) {
    cli::cli_inform(
      "{length(empty_numeric)} calculated band{?s} contain{?s/} no observed values."
    )
  }

  # isoband treats every upper boundary as exclusive, whereas classInt includes
  # the maximum in its final interval. Increase only the internal final limit
  # by a negligible amount while retaining the original reported boundary
  internal_high <- utils::tail(boundaries, -1)
  if (is.finite(utils::tail(internal_high, 1))) {
    last <- length(internal_high)
    internal_high[last] <- internal_high[last] +
      max(abs(internal_high[last]), 1) * sqrt(.Machine$double.eps)
  }
  raw_bands <- isoband::isobands(
    lattice$x,
    lattice$y,
    z,
    utils::head(boundaries, -1),
    internal_high
  )

  # Convert isoband's polygon lists into an SF tibble, repair any invalid
  # polygons created where values coincide with boundaries, and discard bands
  # that have no geometry
  geometries <- isoband::iso_to_sfg(raw_bands)
  result <- sf::st_as_sf(tibble::tibble(
    lower = utils::head(boundaries, -1),
    upper = utils::tail(boundaries, -1),
    geometry = sf::st_sfc(unname(geometries), crs = sf::st_crs(data))
  ))
  result <- sf::st_make_valid(result)
  non_empty <- !sf::st_is_empty(result)
  if (!any(non_empty)) {
    cli::cli_abort(c(
      "No non-empty isoband geometries could be created.",
      "i" = "The retained values may be too sparse on the grid to interpolate."
    ))
  }
  if (!quiet && any(!non_empty)) {
    cli::cli_inform(
      "Removed {sum(!non_empty)} empty isoband geometr{?y/ies}."
    )
  }
  result <- result[non_empty, ]
  labels <- isoband_labels(
    result$lower,
    result$upper,
    final_upper = utils::tail(boundaries, 1)
  )
  result$band <- factor(labels, levels = labels, ordered = TRUE)
  display_labels <- isoband_display_labels(result$lower, result$upper)
  result$label <- factor(
    display_labels,
    levels = display_labels,
    ordered = TRUE
  )
  result <- result[, c("lower", "upper", "band", "label", "geometry")]

  # Record provenance and resolved plotting semantics on the new hspt_ib class.
  # The original hspt_* class is intentionally not inherited because an
  # isoband object no longer has the row-per-grid-cell structure it promises
  plot_type <- if (meaningful_zero) {
    "diverging_zero"
  } else if (
    source_class == "hspt_dk" && value_name == "kde" && source_method == "ratio"
  ) {
    "diverging_one"
  } else {
    "sequential"
  }
  title <- isoband_plot_title(
    source_class,
    value_name,
    source_method,
    weighted = rlang::has_name(data, "sum") && value_name == "sum"
  )
  metadata <- list(
    source_class = source_class,
    source_value = value_name,
    source_method = source_method,
    style = if (style == "pconventions") {
      style
    } else if (length(breaks) > 1) {
      "fixed"
    } else {
      style
    },
    critical_p = if (source_class == "hspt_g" && value_name == "kde") {
      critical_p
    } else {
      NULL
    },
    plot_type = plot_type,
    midpoint = switch(plot_type, diverging_zero = 0, diverging_one = 1, NULL),
    title = title
  )
  structure(result, class = c("hspt_ib", class(result)), isoband = metadata)
}

isoband_source_class <- function(data) {
  # Prefer the most specific recognised class when objects inherit from more
  # than one sfhotspot class; otherwise handle the input as an ordinary SF grid
  known <- c("hspt_dk", "hspt_n", "hspt_k", "hspt_d", "hspt_c", "hspt_g")
  matched <- known[known %in% class(data)]
  if (length(matched) == 0) "sf" else matched[[1]]
}

isoband_default_value <- function(data, source_class, quiet) {
  # Each hotspot result class has a conventional value column. Gi* output is
  # exceptional because KDE is preferred when it has been retained
  value <- switch(
    source_class,
    hspt_n = if (rlang::has_name(data, "sum")) "sum" else "n",
    hspt_dk = "kde",
    hspt_k = "kde",
    hspt_d = "change",
    hspt_g = if (rlang::has_name(data, "kde")) "kde" else "gistar",
    hspt_c = cli::cli_abort(c(
      "Cannot infer numeric values from hotspot classifications.",
      "i" = "The {.var hotspot_category} column is categorical and cannot be converted to isobands."
    )),
    NULL
  )
  if (!is.null(value)) {
    return(value)
  }
  columns <- names(sf::st_drop_geometry(data))
  numeric_columns <- columns[vapply(
    data[columns],
    rlang::is_bare_numeric,
    logical(1)
  )]
  if (length(numeric_columns) == 1) {
    if (!quiet) {
      cli::cli_inform(
        "Using {.var {numeric_columns}} because it is the only numeric column in {.arg data}."
      )
    }
    return(numeric_columns)
  }
  if (length(numeric_columns) == 0) {
    cli::cli_abort("{.arg data} does not contain a numeric column to convert.")
  }
  cli::cli_abort(c(
    "Cannot determine which values to convert to isobands.",
    "i" = "Numeric columns in {.arg data}: {.or {.var {numeric_columns}}}.",
    "i" = "Select one with {.arg value}."
  ))
}

isoband_lattice <- function(data) {
  # Verify that every feature is an equal-sized, axis-aligned square before
  # deriving its row and column from the cell centre
  geometry <- sf::st_geometry(data)
  if (!all(sf::st_geometry_type(geometry) == "POLYGON")) {
    cli::cli_abort(
      "{.arg data} must contain simple square POLYGON grid cells."
    )
  }
  boxes <- lapply(geometry, sf::st_bbox)
  widths <- vapply(boxes, function(x) unname(x["xmax"] - x["xmin"]), numeric(1))
  heights <- vapply(
    boxes,
    function(x) unname(x["ymax"] - x["ymin"]),
    numeric(1)
  )
  cell_size <- stats::median(c(widths, heights))
  tolerance <- max(abs(cell_size) * 1e-7, .Machine$double.eps^0.5)
  if (
    !is.finite(cell_size) ||
      cell_size <= 0 ||
      any(abs(widths - cell_size) > tolerance) ||
      any(abs(heights - cell_size) > tolerance)
  ) {
    cli::cli_abort(
      "{.arg data} must contain equal-sized square cells on a regular rectangular lattice.",
      "i" = "a regular grid of cells can be created with {.fn hotspot_grid}."
    )
  }
  rectangle <- vapply(
    seq_along(geometry),
    function(i) {
      coordinates <- unclass(geometry[[i]])[[1]]
      if (ncol(coordinates) < 2 || nrow(coordinates) != 5) {
        return(FALSE)
      }
      corners <- unique(round(coordinates[, 1:2, drop = FALSE] / tolerance))
      nrow(corners) == 4 &&
        all(coordinates[, 1] >= boxes[[i]]["xmin"] - tolerance) &&
        all(coordinates[, 1] <= boxes[[i]]["xmax"] + tolerance) &&
        all(coordinates[, 2] >= boxes[[i]]["ymin"] - tolerance) &&
        all(coordinates[, 2] <= boxes[[i]]["ymax"] + tolerance) &&
        all(
          abs(coordinates[, 1] - boxes[[i]]["xmin"]) <= tolerance |
            abs(coordinates[, 1] - boxes[[i]]["xmax"]) <= tolerance
        ) &&
        all(
          abs(coordinates[, 2] - boxes[[i]]["ymin"]) <= tolerance |
            abs(coordinates[, 2] - boxes[[i]]["ymax"]) <= tolerance
        )
    },
    logical(1)
  )
  if (!all(rectangle)) {
    cli::cli_abort(
      "{.arg data} must contain square cells on a regular rectangular lattice; hexagonal or irregular cells cannot be converted.",
      "i" = "a regular grid of cells can be created with {.fn hotspot_grid}."
    )
  }
  centres <- sf::st_coordinates(sf::st_centroid(geometry))[, 1:2, drop = FALSE]
  x_origin <- min(centres[, 1])
  y_origin <- min(centres[, 2])
  column <- round((centres[, 1] - x_origin) / cell_size) + 1L
  row_from_bottom <- round((centres[, 2] - y_origin) / cell_size) + 1L
  expected_x <- x_origin + (column - 1) * cell_size
  expected_y <- y_origin + (row_from_bottom - 1) * cell_size
  if (
    any(abs(centres[, 1] - expected_x) > tolerance) ||
      any(abs(centres[, 2] - expected_y) > tolerance) ||
      anyDuplicated(paste(column, row_from_bottom, sep = ":"))
  ) {
    cli::cli_abort(
      "{.arg data} cells must occupy unique positions on a regular rectangular lattice."
    )
  }
  # isoband expects y coordinates from top to bottom, so reverse the otherwise
  # ascending row coordinates and translate the cell positions accordingly
  x <- x_origin + seq.int(0, max(column) - 1) * cell_size
  y <- y_origin + rev(seq.int(0, max(row_from_bottom) - 1)) * cell_size
  if (length(x) < 2 || length(y) < 2) {
    cli::cli_abort(
      "{.arg data} must span at least two rows and two columns to create isobands."
    )
  }
  list(
    x = x,
    y = y,
    column = column,
    row = length(y) - row_from_bottom + 1L
  )
}

isoband_breaks <- function(
  values,
  breaks,
  style,
  meaningful_zero,
  quiet
) {
  # Conventional Gi* thresholds are fixed and deliberately bypass classInt
  if (style == "pconventions") {
    thresholds <- stats::qnorm(1 - c(0.1, 0.05, 0.01, 0.001) / 2)
    return(c(-Inf, -rev(thresholds), thresholds, Inf))
  }
  if (!rlang::is_bare_numeric(breaks) || length(breaks) == 0) {
    cli::cli_abort("{.arg breaks} must be a numeric value or vector.")
  }
  # A vector defines exact boundaries; a scalar asks classInt to calculate
  # approximately that many intervals using the chosen style
  explicit <- length(breaks) > 1
  if (explicit) {
    if (
      any(!is.finite(breaks)) ||
        anyDuplicated(breaks) ||
        is.unsorted(breaks, strictly = TRUE)
    ) {
      cli::cli_abort(
        "Explicit {.arg breaks} must be finite, unique, and strictly increasing."
      )
    }
    if (
      min(values) < breaks[[1]] ||
        max(values) > utils::tail(breaks, 1)
    ) {
      cli::cli_abort(
        "Explicit {.arg breaks} must cover the complete range of selected values."
      )
    }
    return(as.numeric(breaks))
  }
  if (!is.finite(breaks) || breaks < 1 || breaks != as.integer(breaks)) {
    cli::cli_abort(
      "A scalar {.arg breaks} must be a single positive whole number."
    )
  }
  interval <- tryCatch(
    classInt::classIntervals(
      values,
      n = as.integer(breaks),
      style = style,
      warnSmallN = FALSE,
      warnLargeN = !quiet
    ),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Could not calculate isoband boundaries.",
          "i" = conditionMessage(cnd)
        ),
        parent = cnd
      )
    }
  )
  result <- unique(as.numeric(interval$brks))
  if (length(result) < 2 || is.unsorted(result, strictly = TRUE)) {
    cli::cli_abort(
      "The selected interval algorithm did not produce usable boundaries."
    )
  }
  if (!quiet && length(result) - 1 != breaks) {
    cli::cli_inform(
      "Requested {breaks} bands but {.val {style}} produced {length(result) - 1}."
    )
  }
  if (meaningful_zero) {
    result <- isoband_add_zero(result)
  }
  result
}

isoband_add_zero <- function(breaks) {
  # Snap a numerically near-zero boundary to zero before modifying an interval
  tolerance <- max(abs(breaks[is.finite(breaks)]), 1) *
    sqrt(.Machine$double.eps)
  if (any(abs(breaks) <= max(tolerance))) {
    breaks[which.min(abs(breaks))] <- 0
    return(breaks)
  }
  if (min(breaks) > 0) {
    breaks[[1]] <- 0
    return(breaks)
  }
  if (max(breaks) < 0) {
    breaks[[length(breaks)]] <- 0
    return(breaks)
  }
  # When zero lies inside the range, replace the nearer internal boundary. This
  # preserves the requested number of bands without imposing symmetry
  containing <- findInterval(0, breaks)
  lower <- containing
  upper <- containing + 1L
  candidates <- c(lower, upper)
  candidates <- candidates[candidates > 1 & candidates < length(breaks)]
  if (length(candidates) > 0) {
    replace <- candidates[which.min(abs(breaks[candidates]))]
    breaks[[replace]] <- 0
    return(breaks)
  }
  sort(c(breaks, 0))
}

isoband_labels <- function(lower, upper, final_upper) {
  # Match classInt's left-closed convention and close only the final interval
  format_bound <- function(x) {
    ifelse(
      is.infinite(x),
      ifelse(x < 0, "-Inf", "Inf"),
      format(x, trim = TRUE, digits = 6)
    )
  }
  close <- ifelse(upper == final_upper, "]", ")")
  paste0("[", format_bound(lower), ", ", format_bound(upper), close)
}

isoband_display_labels <- function(lower, upper) {
  # Format all finite boundaries using the same notation and number of decimal
  # places. Begin with two significant digits and add precision until every
  # boundary is distinct.
  boundaries <- sort(unique(c(lower, upper)))
  finite_boundaries <- boundaries[is.finite(boundaries)]
  non_zero <- abs(finite_boundaries[finite_boundaries != 0])
  scientific <- any(non_zero < 1e-4 | non_zero >= 1e6)
  for (digits in 2:17) {
    if (scientific) {
      decimal_places <- digits - 1L
      scientific_values <- finite_boundaries
      scientific_values[scientific_values == 0] <- 0
      formatted <- sprintf(
        paste0("%.", decimal_places, "e"),
        scientific_values
      )
    } else {
      boundary_magnitudes <- floor(log10(non_zero))
      decimal_places <- max(0, digits - 1L - boundary_magnitudes)
      rounded <- round(finite_boundaries, digits = decimal_places)
      rounded[rounded == 0] <- 0
      formatted <- sprintf(
        paste0("%.", decimal_places, "f"),
        rounded
      )
    }
    formatted <- sub("^-", "\u2212", formatted)
    if (!anyDuplicated(formatted)) {
      break
    }
  }
  names(formatted) <- as.character(finite_boundaries)
  lookup <- function(x) unname(formatted[as.character(x)])
  ifelse(
    is.infinite(lower),
    paste0("< ", lookup(upper)),
    ifelse(
      is.infinite(upper),
      paste0("\u2265 ", lookup(lower)),
      paste0(lookup(lower), "\u2013", lookup(upper))
    )
  )
}

isoband_plot_title <- function(
  source_class,
  value,
  method = NULL,
  weighted = FALSE
) {
  # Preserve the terminology used by the corresponding continuous hotspot map
  if (source_class == "hspt_n") {
    return(if (weighted) "weighted count" else "count")
  }
  if (source_class == "hspt_d") {
    return("change\n(after \u2212 before)")
  }
  if (source_class == "hspt_g" && value == "gistar") {
    return("Gi* statistic")
  }
  if (source_class == "hspt_g" && value == "kde") {
    return("density in statistically significant cells")
  }
  if (source_class == "hspt_dk") {
    return(switch(
      method,
      ratio = "density ratio",
      log = "log density ratio",
      diff = "density difference",
      sum = "combined density"
    ))
  }
  if (value == "kde") {
    return("density")
  }
  value
}
