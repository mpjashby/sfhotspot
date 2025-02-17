#' Calculate Getis-Ord
#' \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G^*_i}}
#' (gi-star) or
#' \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G_i}}
#' statistics
#'
#' @param counts \code{\link[sf]{sf}} data frame.
#' @param n Name of the column in \code{counts} containing counts of events for
#'   each row.
#' @param nb_dist The distance around a cell that contains the neighbours of
#'   that cell, which are used in calculating the statistic. If this argument is
#'   \code{NULL} (the default), \code{nb_dist} is set as \code{cell_size *
#'   sqrt(2)} so that only the cells immediately adjacent to each cell are
#'   treated as being its neighbours.
#' @param cell_size \code{numeric} value specifying the size of each equally
#'   spaced grid cell, using the same units (metres, degrees, etc.) as used in
#'   the \code{sf} data frame given in the \code{counts} argument. If this
#'   argument is \code{NULL} (the default), the cell size will be calculated
#'   automatically (see Details).
#' @param include_self Should points in a given cell be counted as well as
#'   counts in neighbouring cells when calculating the values of
#'   \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G^*_i}}
#'   (if \code{include_self = TRUE}, the default) or
#'   \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub>}}{\eqn{G_i}}
#'   (if \code{include_self = FALSE}) values? You are unlikely to want to change
#'   the default value.
#' @param p_adjust_method The method to be used to adjust \emph{p}-values for
#'   multiple comparisons. \code{NULL} (the default) uses the default method
#'   used by \code{\link[stats]{p.adjust}}, but any of the character values in
#'   \code{stats::p.adjust.methods} may be specified.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{TRUE}.
#' @return An \code{\link[sf]{sf}} tibble of regular grid cells with
#'   corresponding point counts,
#'   \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub>}}{\eqn{G_i}} or
#'   \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G^*_i}}
#'   values for each cell. Values greater than zero indicate more points than
#'   would be expected for randomly distributed points and values less than zero
#'   indicate fewer points. Critical values of
#'   \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub>}}{\eqn{G_i}} and
#'   \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G^*_i}}
#'   are given in the manual page for \code{\link[spdep]{localG}}.
#' @noRd

gistar <- function(
  counts,
  n,
  nb_dist = NULL,
  cell_size = NULL,
  include_self = TRUE,
  p_adjust_method = NULL,
  quiet = TRUE
) {

  # Process arguments that are column names
  n <- rlang::as_name(rlang::enquo(n))

  # Check inputs
  validate_sf(counts, "counts", quiet = quiet, call = rlang::caller_env())
  if (!n %in% names(counts))
    cli::cli_abort(
      "{.var n} must be the name of a column in the {.var counts} object"
    )
  if (!rlang::is_double(counts[[n]]))
    cli::cli_abort("The column specified in {.var n} must be numeric")
  validate_cell_size(cell_size)
  if (!rlang::is_null(nb_dist) & !rlang::is_double(nb_dist, n = 1))
    cli::cli_abort("{.var nb_dist} must be a single numeric value or NULL")
  if (!rlang::is_null(nb_dist))
    if (nb_dist <= 0) cli::cli_abort("{.var nb_dist} must be greater than zero")
  if (!rlang::is_logical(include_self, n = 1))
    cli::cli_abort("{.arg include_self} must be {.q TRUE} or {.q FALSE}")
  if (!rlang::is_null(p_adjust_method)) {
    if (
      length(p_adjust_method) != 1 |
      !p_adjust_method %in% stats::p.adjust.methods
    ) {
      cli::cli_abort(paste0(
        "{.arg p_adjust_method} must be NULL or one of ",
        "{.or {.val {stats::p.adjust.methods}}}"
      ))
    }
  }
  if (!rlang::is_logical(quiet, n = 1))
    cli::cli_abort("{.arg quiet} must be one of {.q TRUE} or {.q FALSE}")

  # Replace name of geometry column in SF objects if necessary
  counts <- set_geometry_name(counts)

  # Get centroids
  centroids <- suppressWarnings(sf::st_centroid(counts))

  # Set neighbour distance if not specified
  if (rlang::is_null(nb_dist)) {

    # Derive cell size from grid cells if required
    if (rlang::is_null(cell_size)) {
      cell_size <- as.numeric(mean(sf::st_distance(
        centroids,
        centroids[sf::st_nearest_feature(centroids), ],
        by_element = TRUE
      )))
    }

    # Derive neighbour distance from cell size
    nb_dist <- cell_size * sqrt(2)

  }

  # Find neighbours
  nb <- spdep::dnearneigh(sf::st_coordinates(centroids), 0, nb_dist)

  # Determine if each cell should be treated as a neighbour of itself
  if (include_self) nb <- spdep::include.self(nb)

  # Calculate gi* statistic
  gi <- spdep::localG(counts[[n]], listw = spdep::nb2listw(nb, style = "B"))

  # Join results
  result <- counts
  result$gistar <- as.numeric(gi)
  result$pvalue <- spdep::p.adjustSP(
    2 * stats::pnorm(-abs(as.numeric(result$gistar))),
    nb,
    method = ifelse(rlang::is_null(p_adjust_method), "none", p_adjust_method)
  )

  # Return result
  result

}
