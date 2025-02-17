#' Identify change in hotspots over time
#'
#' Identify change in the number of points (typically representing events)
#' between two periods (before and after a specified date) or in two groups
#' (e.g. on weekdays or at weekends).
#'
#' @param data \code{\link[sf]{sf}} data frame containing points.
#' @param time Name of the column in \code{data} containing \code{Date} or
#'   \code{POSIXt} values representing the date associated with each point.
#'   Ignored if \code{groups} is not \code{NULL}. If this argument is
#'   \code{NULL} and \code{data} contains a single column of \code{Date} or
#'   \code{POSIXt} values, that column will be used automatically.
#' @param boundary A single \code{Date} or \code{POSIXt} value representing the
#'   point after which points should be treated as having occurred in the second
#'   time period. See 'Details'.
#' @param groups Name of a column in \code{data} containing exactly two unique
#'   non-missing values, which will be used to identify whether each row should
#'   be counted in the first (before) or second (after) groups. Which groups to
#'   use will be determined by calling \code{sort(unique(groups))}. If
#'   \code{groups} is not a factor, a message will be printed confirming which
#'   value has been used for which group. See 'Details'.
#' @param cell_size \code{numeric} value specifying the size of each equally
#'   spaced grid cell, using the same units (metres, degrees, etc.) as used in
#'   the \code{sf} data frame given in the \code{data} argument. Ignored if
#'   \code{grid} is not \code{NULL}. If this argument and \code{grid} are
#'   \code{NULL} (the default), the cell size will be calculated automatically
#'   (see Details).
#' @param grid_type \code{character} specifying whether the grid should be made
#'   up of squares (\code{"rect"}, the default) or hexagons (\code{"hex"}).
#'   Ignored if \code{grid} is not \code{NULL}.
#' @param grid \code{\link[sf]{sf}} data frame containing points containing
#'   polygons, which will be used as the grid for which counts are made.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{FALSE}.
#' @return An \code{\link[sf]{sf}} tibble of regular grid cells with
#'   corresponding hot-spot classifications for each cell. This can be plotted
#'   using \code{\link{autoplot}}.
#'
#' @details
#'
#' This function creates a regular two-dimensional grid of cells (unless a
#' custom grid is specified with \code{grid}) and calculates the difference
#' between the number of points in each grid cell:
#'
#'   * before and after a set point in time, if \code{boundary} is specified,
#'   * between two groups of points, if a column of grouping values is specified
#'     with \code{groups},
#'   * before and after the mid-point of the dates/times present in the data, if
#'     both \code{boundary} and \code{groups} are \code{NULL} (the default).
#'
#' If both \code{boundary} and \code{groups} are not \code{NULL}, the value of
#' \code{boundary} will be ignored.
#'
#' ## Coverage of the output data
#'
#' The grid produced by this function covers the convex hull of the input data
#' layer. This means the result may include zero counts for cells that are
#' outside the area for which data were provided, which could be misleading. To
#' handle this, consider cropping the output layer to the area for which data
#' are available. For example, if you only have crime data for a particular
#' district, crop the output dataset to the district boundary using
#' \code{\link[sf]{st_intersection}}.
#'
#' ## Automatic cell-size selection
#'
#' If no cell size is given then the cell size will be set so that there are 50
#' cells on the shorter side of the grid. If the \code{data} SF object is
#' projected in metres or feet, the number of cells will be adjusted upwards so
#' that the cell size is a multiple of 100.
#'
#' @seealso [hotspot_dual_kde()] for comparing the density of two layers, which
#' will often be more useful than comparing counts if the point locations
#' represent and underlying continuous distribution.
#'
#' @examples
#'
#' # Compare counts from the first half of the period covered by the data to
#' # counts from the second half
#' \donttest{
#' hotspot_change(memphis_robberies)
#' }
#'
#' # Create a grouping variable, then compare counts across values of that
#' # variable
#' \donttest{
#' memphis_robberies$weekend <-
#'   weekdays(memphis_robberies$date) %in% c("Saturday", "Sunday")
#' hotspot_change(memphis_robberies, groups = weekend)
#' }
#'
#' @export

hotspot_change <- function(
  data,
  time = NULL,
  boundary = NULL,
  groups = NULL,
  cell_size = NULL,
  grid_type = "rect",
  grid = NULL,
  quiet = FALSE
) {

  # Process arguments that are column names
  time <- ifelse(
    rlang::quo_is_null(rlang::enquo(time)),
    FALSE,
    rlang::as_name(rlang::enquo(time))
  )
  groups <- ifelse(
    rlang::quo_is_null(rlang::enquo(groups)),
    FALSE,
    rlang::as_name(rlang::enquo(groups))
  )

  # Check inputs that are not checked in a helper function
  validate_inputs(data = data, grid = grid, quiet = quiet)
  if (!rlang::is_false(time) & !time %in% names(data))
    cli::cli_abort(
      "{.arg time} must be NULL or the name of a column in {.var data}."
    )
  if (
    !rlang::is_null(boundary) &
    !rlang::inherits_any(boundary, c("Date", "POSIXt"))
  ) {
    cli::cli_abort(paste0(
      "{.arg boundary} must be NULL or a single ",
      "{.cls {c('Date', 'POSIXt')}} value."
    ))
  }
  if (!rlang::is_null(boundary)) {
    if (length(boundary) != 1)
      cli::cli_abort(paste0(
        "{.arg boundary} must be NULL or a single ",
        "{.cls {c('Date', 'POSIXt')}} value."
      ))
  }
  if (!rlang::is_false(groups) & !groups %in% names(data))
    cli::cli_abort(
      "{.arg groups} must be NULL or the name of a column in {.var data}."
    )

  # Further checks on arguments that are column names
  if (!rlang::is_false(time)) {
    if (!rlang::inherits_any(data[[time]], c("Date", "POSIXt"))) {
      cli::cli_abort(paste0(
        "{.var time} must be NULL or the name of a column of type ",
        "{.cls {c('Date', 'POSIXt')}} in {.var data}."
      ))
    }
  }
  if (!rlang::is_false(groups)) {
    unique_groups <- sort(unique(stats::na.omit(data[[groups]])))
    if (length(unique_groups) != 2) {

      cli::cli_abort(c(
        paste(
          "{.arg groups} must be NULL or the name of a column in {.var data}",
          "containing exactly *two* unique non-missing values."
        ),
        "i" = ifelse(
          length(unique_groups) == 0,
          "{.arg groups} currently has no non-missing values.",
          "Current values of {.arg groups}: {unique_groups}."
        )
      ))
    }
  }

  # Replace name of geometry column in SF objects if necessary
  grid <- set_geometry_name(grid)

  # Find time column if not specified
  if (rlang::is_false(groups) & rlang::is_false(time)) {
    date_cols <- which(
      unlist(lapply(data, rlang::inherits_any, c("Date", "POSIXt")))
    )
    if (length(date_cols) > 1) {
      cli::cli_abort(c(
        paste0(
          "More than one column in {.var data} contains ",
          "{.cls {c('Date', 'POSIXt')}} values."
        ),
        "i" = "Specify in {.arg time} argument which column to use."
      ))
    } else if (length(date_cols) == 0) {
      cli::cli_abort(
        "No columns in {.var data} contain {.cls {c('Date', 'POSIXt')}} values."
      )
    } else {
      time <- names(data)[date_cols[1]]
    }
  }

  # Set boundary date if not specified
  if (rlang::is_null(boundary) & rlang::is_false(groups)) {
    b <- min(data[[time]]) + (max(data[[time]]) - min(data[[time]])) / 2
    if (rlang::is_false(quiet)) {
      cli::cli_inform(paste0(
        "Boundary point set as ",
        ifelse(
          inherits(b, "Date"),
          format(b, "%d %B %Y"),
          format(b, "%H:%M hours on %d %B %Y")
        ),
        " automatically"
      ))
    }
  } else {
    b <- boundary
  }

  # Error if boundary date is not within range of dates in data
  if (rlang::is_false(groups)) {
    if (b < min(data[[time]]) | b > max(data[[time]])) {
      cli::cli_abort(paste0(
        "{.arg boundary} must be a single {.cls {c('Date', 'POSIXt')}} value ",
        "within the range of values in {.var data}."
      ))
    }
  }

  # Split data
  if (!rlang::is_false(groups)) {
    data_groups <- sort(unique(stats::na.omit(data[[groups]])))
    data_before <- data[data[[groups]] == data_groups[1], ]
    data_after <- data[data[[groups]] == data_groups[2], ]
    if (rlang::is_false(quiet) & !is.factor(data_groups[1])) {
      cli::cli_inform(c(
        "Comparing periods based on values of {.var {groups}}:",
        "*" = paste0(
          "Rows with {.code {groups} == {rlang::expr_text(data_groups[1])}} ",
          "used as 'before' period"
        ),
        "*" = paste0(
          "Rows with {.code {groups} == {rlang::expr_text(data_groups[2])}} ",
          "used as 'after' period"
        )
      ))
    }
  } else {
    data_before <- data[data[[time]] < b, ]
    data_after <- data[data[[time]] >= b, ]
  }

  # Error if there are no points in before/after periods
  if (nrow(data_before) < 1)
    cli::cli_abort("No rows in {.var data} in 'before' period")
  if (nrow(data_after) < 1)
    cli::cli_abort("No rows in {.var data} in 'after' period")

  # Create grid
  if (rlang::is_null(grid)) {
    grid <- create_grid(
      data,
      cell_size = cell_size,
      grid_type = grid_type,
      quiet = quiet
    )
  }

  # Count points in before/after periods
  points_before <- count_points_in_polygons(data_before, grid)
  points_after <- count_points_in_polygons(data_after, grid)

  # Construct final object
  grid$n_before <- points_before$n
  grid$n_after <- points_after$n
  grid$change <- points_after$n - points_before$n
  result <- grid[, c("n_before", "n_after", "change", "geometry")]

  # Return result
  structure(result, class = c("hspt_d", class(result)))

}
