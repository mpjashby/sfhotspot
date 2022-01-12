#' Control the parameters used to classify hotspots
#'
#' This function allows specification of parameters that affect the output from
#' \code{\link{hotspot_classify}}.
#'
#' @param hotspot_prop A single numeric value specifying the minimum proportion
#'   of periods for which a cell must contain significant clusters of points
#'   before the cell can be classified as a hot or cold spot of any type.
#' @param persistent_prop A single numeric value specifying the minimum
#'   proportion of periods for which a cell must contain significant clusters of
#'   points before the cell can be classified as a persistent hot or cold spot.
#' @param recent_prop A single numeric value specifying the proportion of
#'   periods that should be treated as being recent in the classification of
#'   emerging and former hotspots.
#' @param critical_p A threshold \emph{p}-value below which values should be
#'   treated as being statistically significant.
#' @param nb_dist The distance around a cell that contains the neighbours of
#'   that cell, which are used in calculating the statistic. If this argument is
#'   \code{NULL} (the default), \code{nb_dist} is set as \code{cell_size *
#'   sqrt(2)} so that only the cells immediately adjacent to each cell are
#'   treated as being its neighbours.
#' @param include_self Should points in a given cell be counted as well as
#'   counts in neighbouring cells when calculating the values of
#'   \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G^*_i}}
#'   (if \code{include_self = TRUE}, the default) or
#'   \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G_i}}
#'   (if \code{include_self = FALSE}) values? You are unlikely to want to change
#'   the default value.
#' @param p_adjust_method The method to be used to adjust \emph{p}-values for
#'   multiple comparisons. \code{NULL} (the default) uses the default method
#'   used by \code{\link[stats]{p.adjust}}, but any of the character values in
#'   \code{stats::p.adjust.methods} may be specified.
#'
#' @return A list that can be used as the input to the \code{params} argument to
#'   \code{\link{hotspot_classify}}.
#'
#' @export

hotspot_classify_params <- function (
  hotspot_prop = 0.1,
  persistent_prop = 0.8,
  recent_prop = 0.2,
  critical_p = 0.05,
  nb_dist = NULL,
  include_self = TRUE,
  p_adjust_method = NULL
) {

  # Check inputs that are not checked in another helper function
  lapply(
    list(hotspot_prop, persistent_prop, recent_prop, critical_p),
    function (x) {
      arg_name <- rlang::as_label(rlang::quo(x))
      if (!rlang::is_double(x) | length(x) != 1)
        rlang::abort(paste0("`", arg_name, "` must be a single numeric value"))
      if (x >= 1 | x <= 0)
        rlang::abort(
          paste0("`", arg_name, "` must be greater than 0 and less than 1")
        )
    }
  )
  if (!rlang::is_null(nb_dist) & !rlang::is_double(nb_dist, n = 1))
    rlang::abort("`nb_dist` must be `NULL` or a single numeric value")
  if (!rlang::is_null(nb_dist))
    if (nb_dist <= 0) rlang::abort("`nb_dist` must be greater than zero")
  if (!rlang::is_logical(include_self, n = 1))
    rlang::abort("`include_self` must be `TRUE` or `FALSE`")
  if (!rlang::is_null(p_adjust_method)) {
    if (length(p_adjust_method) != 1)
      rlang::abort(paste0(
        "`p_adjust_method` must be either `NULL` or one of \"",
        paste(stats::p.adjust.methods, collapse = "\", \""), "\""
      ))
    if (!p_adjust_method %in% stats::p.adjust.methods)
      rlang::abort(paste0(
        "`p_adjust_method` must be either `NULL` or one of \"",
        paste(stats::p.adjust.methods, collapse = "\", \""), "\""
      ))
  }

  # Construct parameters object
  list(
    "hotspot_prop" = hotspot_prop,
    "persistent_prop" = persistent_prop,
    "recent_prop" = recent_prop,
    "critical_p" = critical_p,
    "nb_dist" = nb_dist,
    "include_self" = include_self,
    "p_adjust_method" = p_adjust_method
  )

}
