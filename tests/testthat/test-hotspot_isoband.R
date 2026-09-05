make_isoband_grid <- function(nx = 4, ny = 4, crs = 3857) {
  grid <- sf::st_make_grid(
    sf::st_as_sfc(sf::st_bbox(c(xmin = 0, ymin = 0, xmax = nx, ymax = ny))),
    cellsize = 1
  )
  sf::st_crs(grid) <- crs
  sf::st_as_sf(tibble::tibble(value = seq_along(grid), geometry = grid))
}

test_that("value is inferred from sfhotspot classes", {
  grid <- make_isoband_grid()
  grid$n <- grid$value
  class(grid) <- c("hspt_n", class(grid))

  result <- hotspot_isoband(grid, breaks = 3, quiet = TRUE)

  expect_s3_class(result, "hspt_ib")
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_false(inherits(result, "hspt_n"))
  expect_named(result, c("lower", "upper", "band", "label", "geometry"))
  expect_true(is.ordered(result$band))
  expect_true(is.ordered(result$label))
  expect_true(all(sf::st_is_valid(result)))
  expect_identical(sf::st_crs(result), sf::st_crs(grid))
  expect_identical(attr(result, "isoband")$source_value, "n")

  grid$sum <- grid$n * 2
  weighted <- hotspot_isoband(grid, breaks = 3, quiet = TRUE)
  expect_identical(attr(weighted, "isoband")$source_value, "sum")
})

test_that("explicit value overrides class defaults using a column name", {
  grid <- make_isoband_grid()
  grid$n <- grid$value
  grid$other <- rev(grid$value)
  grid$other_two <- grid$value * 2
  class(grid) <- c("hspt_n", class(grid))

  result <- hotspot_isoband(grid, value = other, breaks = 3, quiet = TRUE)
  expect_identical(attr(result, "isoband")$source_value, "other")
  expect_error(
    hotspot_isoband(grid, value = starts_with("other"), quiet = TRUE),
    "unquoted name"
  )
  expect_error(hotspot_isoband(grid, value = missing, quiet = TRUE), "does not exist")
})

test_that("arbitrary SF inputs use a sole numeric column", {
  grid <- make_isoband_grid()
  expect_message(
    result <- hotspot_isoband(grid, breaks = 3),
    "only numeric column"
  )
  expect_identical(attr(result, "isoband")$source_class, "sf")

  grid$second <- grid$value
  expect_error(hotspot_isoband(grid, quiet = TRUE), "Numeric columns")
})

test_that("invalid values and break specifications give useful errors", {
  grid <- make_isoband_grid()
  grid$character <- letters[seq_len(nrow(grid))]
  expect_error(
    hotspot_isoband(grid, value = character, quiet = TRUE),
    "must be numeric"
  )
  grid$value <- 1
  expect_error(hotspot_isoband(grid, quiet = TRUE), "two distinct")
  grid$value <- NA_real_
  expect_error(hotspot_isoband(grid, quiet = TRUE), "No finite values")

  grid <- make_isoband_grid()
  expect_error(hotspot_isoband(grid, breaks = 0, quiet = TRUE), "positive")
  expect_error(hotspot_isoband(grid, breaks = 2.5, quiet = TRUE), "whole")
  expect_error(
    hotspot_isoband(grid, breaks = c(0, 2, 1, 16), quiet = TRUE),
    "strictly increasing"
  )
  expect_error(
    hotspot_isoband(grid, breaks = c(2, 20), quiet = TRUE),
    "complete range"
  )
  expect_error(
    hotspot_isoband(grid, style = "not a style", quiet = TRUE),
    "Could not calculate isoband boundaries"
  )
})

test_that("explicit boundaries silently ignore style and include the maximum", {
  grid <- make_isoband_grid()
  expect_no_error(
    result <- hotspot_isoband(
      grid, breaks = c(1, 6, 11, 16), style = "not a style", quiet = TRUE
    )
  )
  expect_identical(attr(result, "isoband")$style, "fixed")
  expect_true(any(result$upper == 16))
  expect_true(any(grepl("\\]$", levels(result$band))))
  expect_true(all(!sf::st_is_empty(result)))
})

test_that("display labels use the minimum unambiguous precision", {
  expect_identical(
    isoband_display_labels(c(0, 0.0189), c(0.0189, 0.07981)),
    c("0.000\u20130.019", "0.019\u20130.080")
  )
  expect_identical(
    isoband_display_labels(c(0.121, 0.124), c(0.124, 0.129)),
    c("0.121\u20130.124", "0.124\u20130.129")
  )
  expect_identical(
    isoband_display_labels(c(-Inf, -0.000012), c(-0.000012, Inf)),
    c("< \u22121.2e-05", "\u2265 \u22121.2e-05")
  )
  expect_identical(
    isoband_display_labels(c(2.6087, 11.9521), c(11.9521, 21.2955)),
    c("2.6\u201312.0", "12.0\u201321.3")
  )
})

test_that("meaningful zero is added without making the range symmetric", {
  changed <- make_isoband_grid()
  changed$change <- seq(-2, 13, length.out = nrow(changed))
  class(changed) <- c("hspt_d", class(changed))
  result <- hotspot_isoband(changed, breaks = 4, quiet = TRUE)
  boundaries <- unique(c(result$lower, result$upper))

  expect_true(0 %in% boundaries || any(result$lower + result$upper == 0))
  expect_false(isTRUE(all.equal(abs(min(boundaries)), max(boundaries))))
  expect_identical(attr(result, "isoband")$plot_type, "diverging_zero")
})

test_that("dual KDE metadata determines zero and one semantics", {
  dual <- make_isoband_grid()
  dual$kde <- seq(-2, 3, length.out = nrow(dual))
  class(dual) <- c("hspt_dk", "hspt_k", class(dual))
  attr(dual, "method") <- "diff"
  difference <- hotspot_isoband(dual, breaks = 4, quiet = TRUE)
  expect_identical(attr(difference, "isoband")$plot_type, "diverging_zero")

  dual$kde <- seq(0.25, 4, length.out = nrow(dual))
  attr(dual, "method") <- "ratio"
  ratio <- hotspot_isoband(dual, breaks = 4, quiet = TRUE)
  expect_identical(attr(ratio, "isoband")$plot_type, "diverging_one")
})

test_that("Gi-star KDE values are filtered before breaks are calculated", {
  gistar <- make_isoband_grid()
  gistar$kde <- seq_len(nrow(gistar))
  gistar$gistar <- seq(-2, 2, length.out = nrow(gistar))
  gistar$pvalue <- c(rep(0.01, 8), rep(0.1, 8))
  class(gistar) <- c("hspt_g", class(gistar))

  result <- hotspot_isoband(gistar, breaks = 2, quiet = TRUE)
  expect_identical(attr(result, "isoband")$source_value, "kde")
  expect_identical(attr(result, "isoband")$critical_p, 0.05)
  expect_equal(max(result$upper), 8)
  expect_identical(attr(result, "isoband")$plot_type, "sequential")

  expect_error(
    hotspot_isoband(gistar, critical_p = 0.001, quiet = TRUE),
    "No cells"
  )
})

test_that("Gi-star output without KDE supports conventional p-value bands", {
  gistar <- make_isoband_grid()
  gistar$gistar <- seq(-4, 4, length.out = nrow(gistar))
  class(gistar) <- c("hspt_g", class(gistar))

  result <- hotspot_isoband(
    gistar,
    breaks = 99,
    style = "pconventions",
    quiet = TRUE
  )
  expected <- stats::qnorm(1 - c(0.1, 0.05, 0.01, 0.001) / 2)
  boundaries <- sort(unique(c(result$lower, result$upper)))
  expect_true(all(round(c(-rev(expected), expected), 6) %in%
                    round(boundaries, 6)))
  expect_true(any(result$lower + result$upper == 0))
  expect_identical(attr(result, "isoband")$source_value, "gistar")
  expect_identical(attr(result, "isoband")$style, "pconventions")
})

test_that("missing lattice positions are accepted", {
  grid <- make_isoband_grid()
  grid <- grid[-c(1, 6, 16), ]
  expect_no_error(hotspot_isoband(grid, breaks = 3, quiet = TRUE))
})

test_that("hexagonal and irregular grids are rejected", {
  square <- make_isoband_grid()
  hex <- sf::st_make_grid(square, cellsize = 1, square = FALSE)
  hex <- sf::st_as_sf(tibble::tibble(value = seq_along(hex), geometry = hex))
  expect_error(hotspot_isoband(hex, quiet = TRUE), "regular rectangular lattice")

  irregular <- square
  sf::st_geometry(irregular)[[1]] <- sf::st_buffer(
    sf::st_geometry(irregular)[[1]],
    0.1
  )
  expect_error(hotspot_isoband(irregular, quiet = TRUE), "square")
})

test_that("potentially unhelpful equal bands produce informative messages", {
  grid <- make_isoband_grid()
  grid$value <- c(rep(0, 15), 100)
  expect_message(
    hotspot_isoband(grid, breaks = 5),
    "may be uninformative"
  )
  expect_no_message(hotspot_isoband(grid, breaks = 5, quiet = TRUE))
})

test_that("isoband plotting uses stored semantics", {
  grid <- make_isoband_grid()
  grid$change <- seq(-2, 3, length.out = nrow(grid))
  class(grid) <- c("hspt_d", class(grid))
  result <- hotspot_isoband(grid, breaks = 3, quiet = TRUE)

  expect_s3_class(autoplot(result), "ggplot")
  expect_no_condition(autolayer(result))
  expect_identical(autoplot(result)$labels$fill, "change\n(after \u2212 before)")
  expect_equal(autolayer(result)[[1]]$data$label, result$label)
  expect_equal(
    autoplot(result)$scales$scales[[1]]$breaks,
    as.character(result$label)
  )
})

test_that("isoband legend titles use lower-case terminology", {
  expect_identical(isoband_plot_title("hspt_n", "n"), "count")
  expect_identical(
    isoband_plot_title("hspt_n", "sum", weighted = TRUE),
    "weighted count"
  )
  expect_identical(
    isoband_plot_title("hspt_g", "kde"),
    "density in statistically significant cells"
  )
  expect_identical(
    isoband_plot_title("hspt_dk", "kde", method = "sum"),
    "combined density"
  )
  expect_identical(isoband_plot_title("hspt_k", "kde"), "density")
})

test_that("isoband metadata survives common SF operations", {
  result <- hotspot_isoband(make_isoband_grid(), breaks = 3, quiet = TRUE)
  expect_s3_class(result[1:2, ], "hspt_ib")
  expect_true(is.list(attr(result[1:2, ], "isoband")))
  transformed <- sf::st_transform(result, 4326)
  expect_s3_class(transformed, "hspt_ib")
  expect_true(is.list(attr(transformed, "isoband")))
  expect_no_condition(ggplot2::ggplot_build(autoplot(result[1:2, ])))

  boundary <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(result)))
  clipped <- hotspot_clip(result, boundary, quiet = TRUE)
  expect_s3_class(clipped, "hspt_ib")
  expect_identical(attr(clipped, "isoband"), attr(result, "isoband"))
  expect_no_condition(ggplot2::ggplot_build(autoplot(clipped)))
})
