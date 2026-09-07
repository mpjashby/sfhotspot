#' Identify hotspots using DBSCAN
#'
#' Identify clusters of points using density-based spatial clustering and
#' represent each cluster as a buffered convex or concave hull. Clusters are
#' ranked by the number of points they contain.
#'
#' @param data An [sf::sf] object containing point geometries.
#' @param eps A single positive number specifying the DBSCAN neighbourhood
#'   radius in the units of the analysis co-ordinate reference system (CRS).
#'   If `NULL`, the radius is calculated automatically from the mean point
#'   density within the convex hull of `data`.
#' @param min_pts A single integer specifying the minimum number of points in
#'   an `eps` neighbourhood, including the point itself, for a point to be a
#'   core point. The default is `5`.
#' @param density_adjust A single positive number controlling automatic
#'   selection of `eps`. Ignored unless `eps = NULL`. A value of `1` (the
#'   background-density reference) identifies neighbourhoods with approximately
#'   at least the mean density within the convex hull of `data`; the default of
#'   `2` corresponds to approximately twice that density.
#' @param hull The type of hull used to represent each cluster: `"convex"`
#'   or `"concave"` (the default).
#' @param hull_ratio For concave hulls, a number from zero to one specifying the
#'   fraction convex. Zero produces a maximally concave hull and one produces a
#'   convex hull. The default is `0.75`. Ignored when `hull = "convex"`.
#' @param transform DBSCAN uses Euclidean distances and therefore requires a
#'   projected CRS. If `TRUE` (the default), geographic input is transformed
#'   automatically with [st_transform_auto()] before analysis and the result is
#'   transformed back afterwards. If `FALSE`, geographic input produces an
#'   error.
#' @param quiet If `TRUE`, suppress informative messages about automatically
#'   selected parameters and geometry preparation.
#' @param ... Further arguments passed to [dbscan::dbscan()], such as
#'   `borderPoints` and nearest-neighbour search controls. The arguments `x`,
#'   `eps`, `minPts`, and `weights` cannot be supplied through `...`.
#'
#' @details
#' When `eps = NULL`, the neighbourhood radius is calculated as
#'
#' `sqrt(((min_pts - 1) * A) / (pi * n * density_adjust))`,
#'
#' where `A` is the area of the convex hull of the input point coordinates and
#' `n` is the number of coordinates. This means that the expected number of
#' points in an `eps` neighbourhood is approximately `density_adjust` times the
#' mean density of points within the convex hull. A value of
#' `density_adjust = 1` identifies clusters with at least the mean density;
#' the default of `2` requires approximately twice the mean density, and still
#' larger values identify clusters with higher density.
#'
#' `MULTIPOINT` geometries are cast to individual points before analysis. The
#' output `n` column counts all input point coordinates intersecting each final
#' cluster polygon, not only points assigned to that DBSCAN cluster. DBSCAN
#' includes border points in clusters by default; supplying
#' `borderPoints = FALSE` through `...` instead performs DBSCAN* and excludes
#' border points from the points used to construct the hull.
#'
#' Each cluster geometry is the selected hull of all assigned points, buffered
#' by `eps` and clipped to the convex hull of all input points. Different
#' cluster polygons may overlap after hull construction and buffering.
#'
#' @return An `sf` tibble with class `hspt_s` and one row per non-noise
#'   cluster. It contains `cluster`, the original DBSCAN cluster identifier;
#'   `rank`, the priority rank; `n`, the number of input point coordinates
#'   intersecting the polygon; `prop`, `n` as a proportion of all input point
#'   coordinates; and `geometry`. Since cluster polygons may overlap, the sum
#'   of `prop` can exceed one. Ranking is by decreasing `n`, then increasing
#'   polygon area, then increasing cluster identifier.
#'
#' @examples
#' \donttest{
#' hotspot_dbscan(memphis_robberies_jan)
#'
#' hotspot_dbscan(
#'   memphis_robberies_jan,
#'   density_adjust = 3,
#'   hull = "convex"
#' )
#' }
#'
#' @export
hotspot_dbscan <- function(
  data,
  eps = NULL,
  min_pts = 5,
  density_adjust = 2,
  hull = c("concave", "convex"),
  hull_ratio = 0.75,
  transform = TRUE,
  quiet = FALSE,
  ...
) {
  # Capture the caller once so errors raised by private helpers point to the
  # user's call rather than to an internal validation or geometry function.
  # Materialising `...` also lets us inspect it before forwarding it to dbscan.
  call <- rlang::caller_env()
  dots <- rlang::list2(...)

  # Validate scalar arguments and prevent users from overriding arguments that
  # this wrapper must control. Match `hull` separately so rlang supplies its
  # standard, informative error for an unknown choice.
  validate_dbscan_params(
    eps = eps,
    min_pts = min_pts,
    density_adjust = density_adjust,
    hull_ratio = hull_ratio,
    transform = transform,
    dots = dots,
    call = call
  )
  hull <- rlang::arg_match(hull, c("convex", "concave"))

  # Use the common package preparation pipeline so empty geometries are
  # removed, Z/M dimensions are dropped and MULTIPOINT rows are expanded to
  # individual POINT rows. From here on, one row represents one coordinate,
  # which is the denominator used for the output `prop` column.
  data <- prepare_point_data(data, quiet = quiet, call = call)

  # Both DBSCAN and buffering interpret numeric distances in CRS units, so a
  # complete CRS with known unit metadata is required even when eps is supplied.
  validate_inputs(
    data = data,
    quiet = quiet,
    require_units = TRUE,
    call = call
  )

  # In unweighted DBSCAN no neighbourhood can contain more observations than
  # exist in the dataset, so a larger min_pts can never produce a cluster.
  if (min_pts > nrow(data)) {
    cli::cli_abort(
      c(
        "{.arg min_pts} cannot exceed the number of point coordinates in {.var data}.",
        "i" = "{.var data} contains {nrow(data)} point coordinate{?s}; {.arg min_pts} is {min_pts}."
      ),
      call = call
    )
  }

  # dbscan's fast point-matrix method uses Euclidean distance. Geographic
  # coordinates must therefore be projected before calculating areas,
  # distances, clusters or buffers. Retain the exact input CRS so the final
  # polygons can be restored rather than assuming the input was EPSG:4326.
  original_crs <- sf::st_crs(data)
  analysis_data <- data
  transformed <- FALSE
  if (sf::st_is_longlat(analysis_data)) {
    if (rlang::is_true(transform)) {
      analysis_data <- st_transform_auto(analysis_data, quiet = quiet)
      transformed <- TRUE
    } else {
      cli::cli_abort(
        c(
          "Cannot calculate DBSCAN clusters for lon/lat data. You can:",
          "*" = "set {.arg transform} to {.q TRUE} to allow auto-transformation or",
          "*" = "transform {.var data} manually to use a projected CRS."
        ),
        call = call
      )
    }
  }

  # Concave hulls are implemented by GEOS only in recent versions. Check this
  # before fitting the model so an unsupported request fails quickly.
  if (hull == "concave") {
    require_concave_hull(call = call)
  }

  # DBSCAN accepts a numeric matrix rather than an SF object. The convex hull
  # of all prepared points serves two purposes: its area defines the reference
  # density for automatic eps, and its geometry bounds the returned hotspots.
  coordinates <- sf::st_coordinates(analysis_data)[, c("X", "Y"), drop = FALSE]
  study_hull <- sf::st_convex_hull(sf::st_union(analysis_data))
  study_area <- as.numeric(sf::st_area(study_hull))
  eps_auto <- rlang::is_null(eps)

  # Resolve eps before fitting so every later operation uses exactly the same
  # effective neighbourhood distance, including polygon buffering.
  if (eps_auto) {
    eps <- set_dbscan_eps(
      area = study_area,
      n = nrow(coordinates),
      min_pts = min_pts,
      density_adjust = density_adjust,
      data = analysis_data,
      quiet = quiet,
      call = call
    )
  }

  # A zero-area input hull cannot be used for automatic eps selection. With an
  # explicit eps, buffer it so clipping can still produce polygonal output.
  clip_boundary <- if (is.finite(study_area) && study_area > 0) {
    study_hull
  } else {
    sf::st_buffer(study_hull, dist = eps)
  }

  # `dots` was captured and checked above, so do.call() forwards only named,
  # non-reserved options such as borderPoints and nearest-neighbour controls.
  # Keeping the wrapper-controlled arguments in the leading list ensures the
  # clustering always uses the validated data, eps and min_pts values.
  clustering <- do.call(
    dbscan::dbscan,
    c(list(x = coordinates, eps = eps, minPts = min_pts), dots)
  )

  # dbscan uses cluster zero for noise. Noise does not generate a hotspot
  # polygon, although noise points can later contribute to n when they fall
  # inside a polygon generated by a cluster. Fail here with parameter-specific
  # advice rather than returning the generic zero-row result error.
  clustered <- clustering$cluster > 0L
  if (!any(clustered)) {
    unit <- dbscan_unit_label(analysis_data)
    cli::cli_abort(
      c(
        "No DBSCAN hotspots were identified.",
        "i" = "Parameters used: {.arg eps} = {format(eps, big.mark = ',')} {unit}; {.arg min_pts} = {min_pts}.",
        "i" = if (eps_auto) {
          "Try decreasing {.arg density_adjust}, supplying a larger {.arg eps}, or decreasing {.arg min_pts}."
        } else {
          "Try increasing {.arg eps} or decreasing {.arg min_pts}."
        }
      ),
      call = call
    )
  }

  # Construct one generalised intervention footprint per DBSCAN cluster. Hulls
  # use every point assigned by the selected DBSCAN variant: ordinary DBSCAN
  # includes border points, whereas borderPoints = FALSE supplies core points
  # only. Buffering by eps guarantees polygonal output even for coincident or
  # collinear points, and clipping prevents results extending outside the
  # inferred study area. Different cluster polygons are deliberately not
  # dissolved because they retain distinct DBSCAN identities.
  cluster_ids <- sort(unique(clustering$cluster[clustered]))
  geometries <- lapply(cluster_ids, function(cluster_id) {
    points <- sf::st_union(analysis_data[clustering$cluster == cluster_id, ])
    cluster_hull <- if (hull == "convex") {
      sf::st_convex_hull(points)
    } else {
      sf::st_concave_hull(points, ratio = hull_ratio, allow_holes = FALSE)
    }
    geometry <- sf::st_intersection(
      sf::st_buffer(cluster_hull, dist = eps),
      clip_boundary
    )

    # Buffering and intersection can expose topology defects or return a
    # GEOMETRYCOLLECTION where polygon boundaries touch. Repair the geometry
    # and retain only its polygonal components for the promised output type.
    geometry <- sf::st_make_valid(geometry)
    if (any(sf::st_is(geometry, "GEOMETRYCOLLECTION"))) {
      geometry <- suppressWarnings(sf::st_collection_extract(
        geometry,
        "POLYGON"
      ))
    }
    geometry
  })

  # Concatenate the one-feature geometry vectors while preserving DBSCAN's
  # original cluster identifiers. Counts are calculated from the completed
  # polygons below rather than from DBSCAN membership.
  geometry <- do.call(c, geometries)
  result <- sf::st_as_sf(tibble::tibble(
    cluster = as.integer(cluster_ids),
    geometry = geometry
  ))

  # Check the complete set before ranking so no cluster silently disappears
  # because hull construction, validity repair or clipping produced no surface.
  valid_polygon <- !sf::st_is_empty(result) &
    sf::st_dimension(result) == 2 &
    sf::st_is(result, c("POLYGON", "MULTIPOLYGON"))
  if (!all(valid_polygon)) {
    cli::cli_abort(
      c(
        "Could not create a valid polygon for every DBSCAN hotspot.",
        "x" = "{sum(!valid_polygon)} cluster{?s} did not produce polygonal geometry.",
        "i" = "Check the input geometries and analysis parameters."
      ),
      call = call
    )
  }

  # Count every prepared input coordinate that intersects each final polygon.
  # st_intersects() includes points on polygon boundaries and matches the
  # predicate used by the package's existing point-counting machinery. This
  # deliberately includes noise points and points assigned to another DBSCAN
  # cluster when a generalised polygon covers them. Overlapping polygons can
  # therefore count the same input point more than once.
  result$n <- as.integer(lengths(sf::st_intersects(result, analysis_data)))

  # Express each polygon count relative to every prepared input coordinate,
  # including noise. Because polygons may overlap, these row-wise proportions
  # are not components of a partition and need not sum to one.
  result$prop <- result$n / nrow(analysis_data)

  # Assign operational priority ranks in the projected analysis CRS. More
  # points rank first; for equal counts, the smaller polygon has greater point
  # density and ranks first. The original cluster ID is a deterministic final
  # tie-breaker. The temporary area values are not part of the tidy output.
  polygon_area <- as.numeric(sf::st_area(result))
  result <- result[order(-result$n, polygon_area, result$cluster), ]
  result$rank <- seq_len(nrow(result))
  result <- result[, c("cluster", "rank", "n", "prop", "geometry")]

  # Area-based ranking must happen before this step. Back-transform only the
  # finished polygons and preserve the projected analysis CRS as provenance.
  analysis_crs <- sf::st_crs(analysis_data)
  if (transformed) {
    result <- sf::st_transform(result, original_crs)
  }

  # Store the resolved analysis choices for reproducibility and future plotting
  # methods. Arguments that had no effect are represented by NULL rather than a
  # potentially misleading supplied value.
  metadata <- list(
    eps = eps,
    eps_auto = eps_auto,
    min_pts = as.integer(min_pts),
    density_adjust = if (eps_auto) density_adjust else NULL,
    hull = hull,
    hull_ratio = if (hull == "concave") hull_ratio else NULL,
    border_points = clustering$borderPoints,
    analysis_crs = analysis_crs
  )

  # The common constructor validates the SF result and puts hspt_s before the
  # standard SF/tibble classes while leaving ordinary SF behaviour intact.
  new_hotspot_results(result, class = "hspt_s", dbscan = metadata, call = call)
}

# Validate DBSCAN-specific scalar arguments and the forwarded argument list.
# Data-dependent conditions, such as min_pts exceeding the number of prepared
# coordinates, are checked in hotspot_dbscan() after MULTIPOINT expansion.
validate_dbscan_params <- function(
  eps,
  min_pts,
  density_adjust,
  hull_ratio,
  transform,
  dots,
  call = rlang::caller_env()
) {
  # eps may be NULL to request automatic selection; all other accepted numeric
  # parameters are mandatory finite scalars within their documented ranges.
  if (
    !rlang::is_null(eps) &&
      (!rlang::is_bare_numeric(eps) ||
        length(eps) != 1 ||
        !is.finite(eps) ||
        eps <= 0)
  ) {
    cli::cli_abort(
      "{.arg eps} must be NULL or a single finite number greater than zero.",
      call = call
    )
  }
  if (
    !rlang::is_bare_numeric(min_pts) ||
      length(min_pts) != 1 ||
      !is.finite(min_pts) ||
      min_pts < 2 ||
      min_pts != floor(min_pts)
  ) {
    cli::cli_abort(
      "{.arg min_pts} must be a single finite integer of at least two.",
      call = call
    )
  }
  if (
    !rlang::is_bare_numeric(density_adjust) ||
      length(density_adjust) != 1 ||
      !is.finite(density_adjust) ||
      density_adjust <= 0
  ) {
    cli::cli_abort(
      "{.arg density_adjust} must be a single finite number greater than zero.",
      call = call
    )
  }
  if (
    !rlang::is_bare_numeric(hull_ratio) ||
      length(hull_ratio) != 1 ||
      !is.finite(hull_ratio) ||
      hull_ratio < 0 ||
      hull_ratio > 1
  ) {
    cli::cli_abort(
      "{.arg hull_ratio} must be a single finite number from zero to one.",
      call = call
    )
  }
  if (!rlang::is_logical(transform, n = 1)) {
    cli::cli_abort(
      "{.arg transform} must be one of {.q TRUE} or {.q FALSE}.",
      call = call
    )
  }

  # Positional arguments in `...` are ambiguous once wrapper arguments are
  # combined with dbscan arguments, so require every forwarded option to have
  # an explicit name.
  dot_names <- names(dots)
  if (
    length(dots) > 0 && (rlang::is_null(dot_names) || any(!nzchar(dot_names)))
  ) {
    cli::cli_abort(
      "All arguments supplied through `...` must be named.",
      call = call
    )
  }

  # These arguments determine behaviour promised by hotspot_dbscan() and must
  # not be replaced downstream. In particular, accepting weights would make
  # the unweighted automatic-density formula and coordinate counts misleading.
  reserved <- intersect(dot_names, c("x", "eps", "minPts", "weights"))
  if (length(reserved) > 0) {
    details <- if ("weights" %in% reserved) {
      "Weighted DBSCAN is not supported by {.fn hotspot_dbscan} at this time."
    } else {
      "Use the corresponding arguments of {.fn hotspot_dbscan} instead."
    }
    cli::cli_abort(
      c(
        "{.arg ...} must not contain reserved argument{?s}: {.arg {reserved}}.",
        "i" = details
      ),
      call = call
    )
  }
  invisible(NULL)
}

# Calculate the neighbourhood radius whose circular area is expected to contain
# min_pts observations (the focal point plus min_pts - 1 other points) at the
# requested multiple of the mean density inside the input-point convex hull.
set_dbscan_eps <- function(
  area,
  n,
  min_pts,
  density_adjust,
  data,
  quiet,
  call = rlang::caller_env()
) {
  # Coincident or collinear points have a zero-area convex hull, so mean areal
  # density—and consequently automatic eps—has no defined finite value.
  if (!is.finite(area) || area <= 0) {
    cli::cli_abort(
      c(
        "Could not calculate an automatic neighbourhood distance from {.var data}.",
        "i" = "The convex hull of the input point coordinates has zero area.",
        "i" = "Supply {.arg eps} explicitly or plot {.var data} to check its point locations."
      ),
      call = call
    )
  }

  # Rearranging density = count / circular area gives the required radius.
  # Subtract one because DBSCAN includes the focal point in minPts.
  eps <- sqrt(((min_pts - 1) * area) / (pi * n * density_adjust))
  if (!is.finite(eps) || eps <= 0) {
    cli::cli_abort(
      "Automatic selection did not produce a finite positive {.arg eps} value.",
      call = call
    )
  }

  # Match the package convention of reporting automatically chosen values while
  # keeping quiet = TRUE suitable for scripts and repeated analyses.
  if (rlang::is_false(quiet)) {
    unit <- dbscan_unit_label(data)
    eps_report <- if (eps > 1000) round(eps) else signif(eps, 4)
    cli::cli_inform(
      c(
        "Neighbourhood distance set automatically from mean point density.",
        "i" = "{.arg eps} = {format(eps_report, big.mark = ',')} {unit}."
      ),
      call = call
    )
  }
  eps
}

# Convert common CRS unit names to reader-friendly plurals for condition
# messages, while retaining an explicit label for uncommon but valid units.
dbscan_unit_label <- function(data) {
  unit <- sf::st_crs(data, parameters = TRUE)$units_gdal
  switch(
    unit,
    metre = "metres",
    meter = "metres",
    foot = "feet",
    `US survey foot` = "feet",
    degree = "degrees",
    paste0("units (", unit, ")")
  )
}

# Fail with actionable advice when a user requests the GEOS concave-hull
# operation on a system where sf cannot provide it. Strip development suffixes
# from the GEOS version before comparing dotted numeric versions.
require_concave_hull <- function(call = rlang::caller_env()) {
  version <- sf::sf_extSoftVersion()[["GEOS"]]
  clean_version <- sub("[^0-9.].*$", "", version)
  if (utils::compareVersion(clean_version, "3.11.0") < 0) {
    cli::cli_abort(
      c(
        "Concave cluster hulls require GEOS 3.11.0 or later.",
        "i" = "This system uses GEOS {version}.",
        "i" = "Use {.code hull = \"convex\"} or update the spatial libraries used by {.pkg sf}."
      ),
      call = call
    )
  }
  invisible(NULL)
}
