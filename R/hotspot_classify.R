#' Classify hot-spots
#'
#' Classify cells in a grid based on changes in the clustering of points
#' (typically representing events) in a two-dimensional regular grid over time.
#'
#' @param data \code{\link[sf]{sf}} data frame containing points.
#' @param time Name of the column in \code{data} containing \code{Date} or
#'   \code{POSIXt} values representing the date associated with each point. If
#'   this argument is \code{NULL} and \code{data} contains a single column of
#'   \code{Date} or \code{POSIXt} values, that column will be used
#'   automatically.
#' @param period A character value containing a number followed by a unit of
#'   time, e.g. for example, "12 months" or "3.5 days", where the unit of time
#'   is one of second, minute, hour, day, week, month, quarter or year (or their
#'   plural forms).
#' @param start A \code{Date} or \code{POSIXt} value specifying when the first
#'   temporal period should start. If \code{NULL} (the default), the first
#'   period will start at the beginning of the earliest date found in the data
#'   (if \code{period} is specified in days, weeks, months, quarters or years)
#'   or at the earliest time found in the data otherwise.
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
#' @param collapse If the range of dates in the data is not a multiple of
#'   \code{period}, the final period will be shorter than the others. In that
#'   case, should this shorter period be collapsed into the penultimate period?
#' @param params A list of optional parameters that can affect the output. The
#'   list can be produced most easily using the
#'   \code{\link{hotspot_classify_params}} helper function.
#' @param quiet if set to \code{TRUE}, messages reporting the values of any
#'   parameters set automatically will be suppressed. The default is
#'   \code{FALSE}.
#' @return An \code{\link[sf]{sf}} tibble of regular grid cells with
#'   corresponding hot-spot classifications for each cell. This can be plotted
#'   using \code{\link{autoplot}}.
#'
#' Hot-spots are spatial areas that contain more points than would be expected
#' by chance; cold-spots are areas that contain fewer points than would be
#' expected. Whether an area is a hot-spot can vary over time. This function
#' creates a space-time cube, determines whether an area is a hot-spot for each
#' of several consecutive time periods and uses that to classify areas according
#' to whether they are persistent, intermittent, emerging or former hot- or
#' cold-spots.
#'
#' ## Hot and cold spots
#'
#' Hot- and cold-spots are identified by calculating the Getis-Ord
#' \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G^*_i}}
#' (gi-star) or
#' \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G_i}}
#' \eqn{Z}-score statistic for each cell in a regular grid for each time period.
#' Cells are classified as follows, using the parameters provided in the
#' `params` argument:
#'
#' * _Persistent hot-/cold-spots_ are cells that have been hot-/cold-spots
#'   consistently over time. Formally: if the \emph{p}-value is less than
#'   `critical_p` for at least `persistent_prop` proportion of time periods.
#' * _Emerging hot-/cold-spots_ are cells that have become hot-/cold-spots
#'   recently but were not previously. Formally: if the \emph{p}-value is less
#'   than `critical_p` for at least `hotspot_prop` of time periods defined as
#'   recent by `recent_prop` but the \emph{p}-value was _not_ less than
#'   `critical_p` for at least `hotspot_prop` of time periods defined as
#'   non-recent by `1 - recent_prop`.
#' * _Former hot-/cold-spots_ are cells that used to be hot-/cold-spots but have
#'   not been more recently. Formally: if the \emph{p}-value was less than
#'   `critical_p` for at least `hotspot_prop` of time periods defined as
#'   non-recent by `1 - recent_prop` but the \emph{p}-value was _not_ less than
#'   `critical_p` for for at least `hotspot_prop` of time periods defined as
#'   recent by `recent_prop`.
#' * _Intermittent hot-/cold-spots_ are cells that have been hot-/cold-spots,
#'   but not as frequently as persistent hotspots and not only during
#'   recent/non-recent periods. Formally: if the \emph{p}-value is less than
#'   `critical_p` for at least `hotspot_prop` of time periods but the cell is
#'   not an emerging or former hotspot.
#' * _No pattern_ if none of the above categories apply.
#'
#' ## Coverage of the output data
#'
#' The grid produced by this function covers the convex hull of the input data
#' layer. This means the result may include
#' \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G^*_i}} or
#' \ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G_i}}
#' values for cells that are outside the area for which data were provided,
#' which could be misleading. To handle this, consider cropping the output layer
#' to the area for which data are available. For example, if you only have crime
#' data for a particular district, crop the output dataset to the district
#' boundary using \code{\link[sf]{st_intersection}}.
#'
#' ## Automatic cell-size selection
#'
#' If no cell size is given then the cell size will be set so that there are 50
#' cells on the shorter side of the grid. If the `data` SF object is projected
#' in metres or feet, the number of cells will be adjusted upwards so that the
#' cell size is a multiple of 100.
#'
#' @references
#' Chainey, S. (2020). \emph{Understanding Crime: Analyzing the Geography of
#' Crime}. Redlands, CA: ESRI.
#'
#' @export

hotspot_classify <- function(
  data,
  time = NULL,
  period = NULL,
  start = NULL,
  cell_size = NULL,
  grid_type = "rect",
  grid = NULL,
  collapse = FALSE,
  params = hotspot_classify_params(),
  quiet = FALSE
) {

  # Check for dependencies
  rlang::check_installed("lubridate", reason = "to use `hotspot_classify()`")

  # Process arguments that are column names
  time <- ifelse(
    rlang::quo_is_null(rlang::enquo(time)),
    FALSE,
    rlang::as_name(rlang::enquo(time))
  )

  # Check inputs that are not checked in a helper function
  validate_inputs(data = data, grid = grid, quiet = quiet)
  if (!rlang::is_false(time) & !time %in% names(data))
    cli::cli_abort(
      "{.arg time} must be NULL or the name of a column in {.var data}."
    )
  if (!rlang::is_false(time)) {
    if (!rlang::inherits_any(data[[time]], c("Date", "POSIXt"))) {
      cli::cli_abort(paste0(
        "{.var time} must be NULL or the name of a column of type ",
        "{.cls {c('Date', 'POSIXt')}} in {.var data}."
      ))
    }
  }
  if (!rlang::is_null(period) & !rlang::is_character(period, n = 1))
    cli::cli_abort("{.arg period} must be NULL or a single character value.")
  if (
    !rlang::is_null(start) &
    !(rlang::inherits_any(start, c("Date", "POSIXt")) & length(start) == 1)
  ) {
    cli::cli_abort(paste0(
      "{.arg start} must be NULL or a single {.cls {c('Date', 'POSIXt')}} ",
      "value."
    ))
  }
  if (!rlang::is_logical(collapse, n = 1))
    cli::cli_abort("{.arg collapse} must be {.q TRUE} or {.q FALSE}")
  if (!rlang::is_bare_list(params))
    cli::cli_abort(c(
      "{.arg params} must be a list.",
      "i" = "use {.fn hotspot_classify_params()} to construct {.arg params}."
    ))
  if (!all(names(rlang::fn_fmls(hotspot_classify_params)) %in% names(params)))
    cli::cli_abort(c(
      "{.arg params} must be a list containing all the required parameters.",
      "i" = "use {.fn hotspot_classify_params()} to construct {.arg params}."
    ))

  # Replace name of geometry column in SF objects if necessary
  grid <- set_geometry_name(grid)

  # Find time column if not specified
  if (isFALSE(time)) {
    date_cols <- which(
      unlist(lapply(data, rlang::inherits_any, c("Date", "POSIXt")))
    )
    if (length(date_cols) > 1) {
      rlang::abort(c(
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

  # Error if start date is before first time in data
  if (!rlang::is_null(start)) {
    if (start > min(data[[time]]))
      cli::cli_abort(
        "{.arg start} must be before the first date/time in ","{.var data}."
      )
  }

  # Set start date if not specified
  if (rlang::is_null(start)) start <- min(data[[time]])

  # Identify time period
  time_span <- difftime(max(data[[time]]), min(data[[time]]))
  if (rlang::is_character(period)) {

    # Extract the number of units
    # Source: https://stackoverflow.com/a/19256529/8222654
    periods <- as.numeric(regmatches(
      period,
      gregexpr("[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?", period)
    )[[1]][1])

    # Extract the temporal unit
    unit <- regmatches(
      period,
      gregexpr("(second|minute|hour|day|week|month|quarter|year)", period)
    )[[1]][1]

    # If no number or unit was found in time_unit, throw an error
    if (rlang::is_na(periods) | rlang::is_na(unit))
      cli::cli_abort(c(
        paste0(
          "{.arg period} must be NULL or a character value containing a ",
          "number followed by a unit of time."
        ),
        "i" = 'Example: "12 months" or "3.5 days".',
        "i" = paste0(
          "Valid units of time are second, minute, hour, day, week, month, ",
          "quarter and year."
        )
      ))

  } else {

    # Identify time period automatically
    if (attr(time_span, "units") == "weeks") {
      time_span <- difftime(
        max(data[[time]]),
        min(data[[time]]),
        units = "days"
      )
    }

    periods <- NA

    if (attr(time_span, "units") == "days") {

      unit <- "days"

      # Try to decide on a reasonable unit
      if (time_span <= 14) {
        # 14 days or fewer -> days
        periods <- 1
      } else if (time_span <= 91) {
        # Less than 1 quarter -> weeks
        periods <- 7
      } else if (time_span <= 366 * 2) {
        # Less than 2 years -> four-week periods
        periods <- 28
      } else if (time_span <= 3653) {
        # Less than 10 years -> quarters
        periods <- 91
      } else if (time_span <= 3653 * 2) {
        # Less than 20 years -> years
        # Need to deal with fact that years have different lengths
        unit <- "years"
      }

    }

    # Just divide the time span into 10 equal units
    if (rlang::is_na(unit)) {
      periods <- floor(time_span / 10)
    }

    if (rlang::is_false(quiet)) {
      cli::cli_inform("{.arg period} set to {periods} {unit} automatically.")
    }

  }

  # Create sequence of start dates for each period
  if (unit %in% c(
    "day", "week", "month", "quarter", "year", "days", "weeks", "months",
    "quarters", "years"
  )) {
    dates <- seq(
      from = as.Date(start),
      to = as.Date(max(data[[time]])),
      by = paste(periods, unit)
    )
  } else {
    dates <- seq(
      from = start,
      to = max(data[[time]]),
      by = paste(periods, unit)
    )
  }

  # Check for remainders and (possibly) add that time to the final period
  period_remainder <- as.numeric(time_span) %% periods
  if (period_remainder != 0) {
    if (rlang::is_true(collapse)) {

      dates <- dates[1:(length(dates) - 1)]

      if (rlang::is_false(quiet)) {
        cli::cli_inform(c(
          "Date range in data is not a multiple of chosen period.",
          "i" = paste0(
            "Final period of {format(period_remainder, digits = 1)} {unit} ",
            "will be collapsed into penultimate period."
          )
        ))
      }

    } else if (rlang::is_false(quiet)) {

      cli::cli_inform(c(
        "Date range data is not a multiple of chosen period.",
        "i" = paste0(
          "Final period contains {format(period_remainder, digits = 1)} {unit}."
        ),
        "i" = paste0(
          "Set {.code collapse = TRUE} to collapse that period into ",
          "penultimate period."
        )
      ))

    }
  }

  # Add a final date in the future so that splitting data with `cut()` includes
  # all values
  if (lubridate::is.Date(dates)) {
    dates <- c(dates, as.Date(max(data[[time]])) + lubridate::years(1))
  } else {
    dates <- c(dates, max(data[[time]]) + lubridate::years(1))
  }

  # Set cell size if not specified (do this here because it is needed by both
  # `create_grid()` and `gistar()`)
  if (rlang::is_null(cell_size))
    cell_size <- set_cell_size(data, round = TRUE, quiet = quiet)

  # Create grid
  if (rlang::is_null(grid)) {
    grid <- create_grid(
      data,
      cell_size = cell_size,
      grid_type = grid_type,
      quiet = quiet
    )
  }

  # Categorise data by period
  if (unit == "year") {
    data$period <- lubridate::year(data[[time]])
  } else if (lubridate::is.Date(dates)) {
    data$period <- cut(
      as.Date(data[[time]]),
      breaks = dates,
      include.lowest = TRUE
    )
  } else {
    data$period <- cut(
      data[[time]],
      breaks = dates,
      include.lowest = TRUE
    )
  }

  # Split data by period
  period_data <- split(data, data$period)

  # Count cells for each period
  period_counts <- mapply(
    function(x, y) {

      # If there are no rows in the data for a particular period, `aggregate()`
      # (used in `count_points_in_polygons()`) will throw an error, so in that
      # case just return a copy of the grid with a column of zero counts
      if (nrow(x) == 0) {
        counted <- grid
        counted$n <- 0

        cli::cli_warn(c(
          "Zero points relate to the period beginning {y}.",
          "i" = "This period will be excluded from classification.",
          "i" = "Consider changing {.arg period} or {.arg start}."
        ))
      } else {
        counted <- count_points_in_polygons(x, polygons = grid)
      }

      # Return result
      # This must be returned as a list because otherwise `mapply()` collapses
      # all the data frames returned into a single flat list, from which it is
      # not possible to extract the data for each period
      list(y = counted)

    },
    period_data,
    names(period_data)
  )

  # Calculate Gi* for each period
  period_gistar <- mapply(
    function(x, y) {

      # Calculate Gi*
      period_count <- gistar(
        x,
        n = "n",
        nb_dist = params$nb_dist,
        cell_size = cell_size,
        include_self = params$include_self,
        p_adjust_method = params$p_adjust_method,
        quiet = quiet
      )

      # Add columns for cell ID and period name
      period_count$id <- seq_len(nrow(period_count))
      period_count$period <- y

      # Return result
      # This must be returned as a list because otherwise `mapply()` collapses
      # all the data frames returned into a single flat list, from which it is
      # not possible to extract the data for each period
      list(y = period_count)

    },
    period_counts,
    seq_len(length(period_counts))
  )

  period_gistar <- rlang::exec(
    rbind,
    !!!lapply(period_gistar, sf::st_drop_geometry)
  )

  # Adjust p-values based on number of periods
  cell_gistar <- split(period_gistar, period_gistar$id)
  cell_gistar <- rlang::exec(
    rbind,
    !!!lapply(cell_gistar, function(x) {
      x$pvalue <- stats::p.adjust(x$pvalue, method = params$p_adjust_method)
      x
    }))

  # Compare p-value to threshold value
  cell_gistar$hot <- cell_gistar$gistar > 0
  cell_gistar$sig <- cell_gistar$pvalue < params$critical_p
  cell_gistar$sig_hot <- cell_gistar$sig & cell_gistar$hot
  cell_gistar$sig_cold <- cell_gistar$sig & !cell_gistar$hot

  # Reshape data to wide format
  # `reshape()` doesn't work on tibbles, so first convert to data frame and then
  # back again
  ch <- tibble::as_tibble(stats::reshape(
    as.data.frame(cell_gistar[, c("id", "period", "sig_hot")]),
    direction = "wide",
    idvar = "id",
    timevar = "period"
  ))
  cc <- tibble::as_tibble(stats::reshape(
    as.data.frame(cell_gistar[, c("id", "period", "sig_cold")]),
    direction = "wide",
    idvar = "id",
    timevar = "period"
  ))
  ch$id <- NULL
  cc$id <- NULL

  # Set number of recent periods
  recent_periods <- floor(params$recent_prop * ncol(ch))
  recent_warn <- FALSE
  if (recent_periods < 1 | ncol(ch) - recent_periods < 1) {
    recent_periods <- 1
    recent_warn <- TRUE
  }
  if (ncol(ch) - recent_periods < 1) {
    cli::cli_abort(c(
      "Zero periods identified as being non-recent.",
      "i" = "There must be at least one recent and one non-recent period.",
      "i" = paste0(
        "Specify a shorter period to generate at least one non-recent period ",
        "or specify smaller proportion of periods to be treated as recent ",
        "using {.fn hotspot_classify_params()}."
      )
    ))
  }
  if (rlang::is_true(recent_warn)) {
    # Warn here, rather than above, so that if both recent periods and
    # non-recent periods are zero then the above error will happen before this
    # warning
    cli::cli_warn(c(
      "Zero periods identified as being recent.",
      "i" = "The final period will be treated as being recent.",
      "i" = paste0(
        "Specify larger proportion of periods to be treated as recent using ",
        "{.fn hotspot_classify_params()}."
      )
    ))
  }

  # Count number of significant values
  cs <- tibble::tibble(
    # Proportion of all periods that are significant
    prop_h = rowSums(ch, na.rm = TRUE) / ncol(ch),
    prop_c = rowSums(cc, na.rm = TRUE) / ncol(cc),
    # Proportion of recent periods that are significant
    recent_h = rowSums(
      ch[, (ncol(ch) - recent_periods):ncol(ch)],
      na.rm = TRUE
    ) / recent_periods,
    recent_c = rowSums(
      cc[, (ncol(cc) - recent_periods):ncol(cc)],
      na.rm = TRUE
    ) / recent_periods,
    # Proportion of non-recent periods that are significant
    older_h = rowSums(
      ch[, 1:(ncol(ch) - recent_periods)],
      na.rm = TRUE
    ) / (ncol(ch) - recent_periods),
    older_c = rowSums(
      cc[, 1:(ncol(cc) - recent_periods)],
      na.rm = TRUE
    ) / (ncol(cc) - recent_periods)
  )

  # Test for hot/coldspot categories
  # Persistent
  cs$persistent_h <- cs$prop_h >= params$persistent_prop
  cs$persistent_c <- cs$prop_c >= params$persistent_prop
  # Emerging
  cs$emerging_h <- cs$older_h < params$hotspot_prop &
    cs$recent_h >= params$recent_prop
  cs$emerging_c <- cs$older_c < params$hotspot_prop &
    cs$recent_c >= params$recent_prop
  # Former
  cs$former_h <- cs$older_h >= params$hotspot_prop &
    cs$recent_h < params$hotspot_prop
  cs$former_c <- cs$older_c >= params$hotspot_prop &
    cs$recent_c < params$hotspot_prop
  # Intermittent
  cs$intermittent_h <- cs$prop_h >= params$hotspot_prop
  cs$intermittent_c <- cs$prop_c >= params$hotspot_prop

  # Categorise hot/coldspots
  cs$cat_h <- ifelse(
    cs$persistent_h,
    "persistent hotspot",
    ifelse(
      cs$emerging_h,
      "emerging hotspot",
      ifelse(
        cs$former_h,
        "former hotspot",
        ifelse(cs$intermittent_h, "intermittent hotspot", "no pattern")
      )
    )
  )
  cs$cat_c <- ifelse(
    cs$persistent_c,
    "persistent coldspot",
    ifelse(
      cs$emerging_c,
      "emerging coldspot",
      ifelse(
        cs$former_c,
        "former coldspot",
        ifelse(cs$intermittent_c, "intermittent coldspot", "no pattern")
      )
    )
  )
  grid$hotspot_category <- ifelse(
    cs$cat_h != "no pattern" & cs$cat_c != "no pattern",
    "mixed hot/coldspot",
    ifelse(cs$cat_h != "no pattern", cs$cat_h, cs$cat_c)
  )

  # Return result
  result <- grid[, c("hotspot_category", "geometry")]
  structure(result, class = c("hspt_c", class(result)))

}
