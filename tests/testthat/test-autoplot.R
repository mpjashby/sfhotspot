# Plot methods need representative columns, classes and geometry, not repeated
# executions of the analysis algorithms. A 4-by-3 toy lattice supplies enough
# rows for every classification category while keeping plotting tests focused.
toy_extent <- sf::st_as_sfc(sf::st_bbox(
  c(xmin = 0, ymin = 0, xmax = 4000, ymax = 3000),
  crs = sf::st_crs(3857)
))
toy_geometry <- sf::st_make_grid(toy_extent, n = c(4, 3))
toy_n <- as.double(seq_along(toy_geometry) - 1L)

result_count <- new_hotspot_results(
  sf::st_as_sf(tibble::tibble(n = toy_n, geometry = toy_geometry)),
  class = "hspt_n"
)
result_kde <- new_hotspot_results(
  sf::st_as_sf(tibble::tibble(
    n = toy_n,
    kde = as.double(seq_along(toy_geometry)) / 100,
    geometry = toy_geometry
  )),
  class = "hspt_k"
)
result_classify <- new_hotspot_results(
  sf::st_as_sf(tibble::tibble(
    hotspot_category = rep(
      names(hotspot_category_colours),
      length.out = length(toy_geometry)
    ),
    geometry = toy_geometry
  )),
  class = "hspt_c"
)
result_change <- new_hotspot_results(
  sf::st_as_sf(tibble::tibble(
    n_before = toy_n,
    n_after = rev(toy_n),
    change = toy_n - rev(toy_n),
    geometry = toy_geometry
  )),
  class = "hspt_d"
)

result_gistar <- result_kde
result_gistar$gistar <- rep(c(-2, -1, 1, 2), length.out = nrow(result_gistar))
result_gistar$pvalue <- rep(c(0.01, 0.1), length.out = nrow(result_gistar))
class(result_gistar) <- c(
  "hspt_g",
  setdiff(class(result_gistar), "hspt_k")
)
result_gistar_no_kde <- result_gistar[
  , setdiff(names(result_gistar), "kde")
]

result_dual_kde <- result_kde
class(result_dual_kde) <- c("hspt_dk", class(result_dual_kde))
attr(result_dual_kde, "method") <- "ratio"

layer_data <- function(layer) layer[[1]]$data


test_that("hotspot_layer delegates to the autolayer methods", {
  wrapped <- hotspot_layer(result_count, alpha = 0.5)
  direct <- ggplot2::autolayer(result_count, alpha = 0.5)

  expect_equal(layer_data(wrapped), layer_data(direct))
  expect_identical(wrapped$aes_params$alpha, direct$aes_params$alpha)

  expect_equal(
    layer_data(hotspot_layer(result_gistar, sign = "hot")),
    layer_data(ggplot2::autolayer(result_gistar, sign = "hot"))
  )
})



# TEST INPUTS ------------------------------------------------------------------


# Errors ----

test_that("error if `object` is not an SF object", {
  expect_error(autoplot(sf::st_drop_geometry(result_count)))
  expect_error(autolayer(sf::st_drop_geometry(result_count)))
  expect_error(autoplot(sf::st_drop_geometry(result_kde)))
  expect_error(autolayer(sf::st_drop_geometry(result_kde)))
  expect_error(autoplot(sf::st_drop_geometry(result_classify)))
  expect_error(autolayer(sf::st_drop_geometry(result_classify)))
  expect_error(autoplot(sf::st_drop_geometry(result_change)))
  expect_error(autolayer(sf::st_drop_geometry(result_change)))
  expect_error(autoplot(sf::st_drop_geometry(result_gistar)))
  expect_error(autolayer(sf::st_drop_geometry(result_gistar)))
  expect_error(autoplot(sf::st_drop_geometry(result_dual_kde)))
  expect_error(autolayer(sf::st_drop_geometry(result_dual_kde)))
})

test_that("error if `object` does not contain the required columns", {
  expect_error(autoplot(result_count[, "geometry"]))
  expect_error(autolayer(result_count[, "geometry"]))
  expect_error(autoplot(result_kde[, "geometry"]))
  expect_error(autolayer(result_kde[, "geometry"]))
  expect_error(autoplot(result_classify[, "geometry"]))
  expect_error(autolayer(result_classify[, "geometry"]))
  expect_error(autoplot(result_change[, "geometry"]))
  expect_error(autolayer(result_change[, "geometry"]))
  expect_error(autoplot(result_gistar[, "geometry"]))
  expect_error(autolayer(result_gistar[, "geometry"]))
  expect_error(autoplot(result_dual_kde[, "geometry"]))
  expect_error(autolayer(result_dual_kde[, "geometry"]))
})

test_that("error if required column does not have correct type", {
  result_count$n <- as.character(result_count$n)
  expect_error(autoplot(result_count))
  expect_error(autolayer(result_count))
  result_kde$kde <- as.character(result_kde$kde)
  expect_error(autoplot(result_kde))
  expect_error(autolayer(result_kde))
  result_change$change <- as.character(result_change$change)
  expect_error(autoplot(result_change))
  expect_error(autolayer(result_change))
  result_gistar$gistar <- as.character(result_gistar$gistar)
  expect_error(autoplot(result_gistar))
  expect_error(autolayer(result_gistar))
})

test_that("Gi* plotting arguments are validated", {
  expect_error(autoplot(result_gistar, critical_p = character()))
  expect_error(autoplot(result_gistar, critical_p = c(0.01, 0.05)))
  expect_error(autoplot(result_gistar, critical_p = 0))
  expect_error(autoplot(result_gistar, critical_p = Inf))
  expect_error(autoplot(result_gistar, sign = TRUE))
  expect_error(autoplot(result_gistar, sign = "positive"))

  missing_pvalue <- result_gistar[
    , setdiff(names(result_gistar), "pvalue")
  ]
  expect_error(autoplot(missing_pvalue), "pvalue")
})

test_that("dual-KDE method metadata is validated", {
  missing_method <- result_dual_kde
  attr(missing_method, "method") <- NULL
  expect_error(autoplot(missing_method), "method")
  expect_error(autolayer(missing_method), "method")

  invalid_method <- result_dual_kde
  attr(invalid_method, "method") <- "invalid"
  expect_error(autoplot(invalid_method), "method")
})

test_that("classification categories are validated", {
  invalid_category <- result_classify
  invalid_category$hotspot_category[[1]] <- "unknown category"
  expect_error(autoplot(invalid_category), "Unknown value")

  invalid_type <- result_classify
  invalid_type$hotspot_category <- seq_len(nrow(invalid_type))
  expect_error(autolayer(invalid_type), "must be character")
})



# TEST OUTPUTS -----------------------------------------------------------------

test_that("output has correct class", {
  expect_s3_class(autoplot(result_count), "ggplot")
  expect_s3_class(autoplot(result_kde), "ggplot")
  expect_s3_class(autoplot(result_classify), "ggplot")
  expect_s3_class(autoplot(result_change), "ggplot")
  expect_s3_class(autoplot(result_gistar), "ggplot")
  expect_s3_class(autoplot(result_gistar_no_kde), "ggplot")
  expect_s3_class(autoplot(result_dual_kde), "ggplot")
})

test_that("base maps are opt-in and configured without downloading tiles", {
  expect_error(autoplot(result_count, basemap_type = NA), "single string")
  expect_length(autoplot(result_count)$layers, 1)

  skip_if_not_installed("ggspatial")
  plot <- autoplot(
    result_count,
    alpha = 0.2,
    basemap_type = "osm",
    basemap_zoom = 12
  )

  # Inspecting an unbuilt plot avoids any request to the tile provider.
  expect_s3_class(plot$layers[[1]]$geom, "GeomMapTile")
  expect_equal(plot$layers[[1]]$data$type, "osm")
  expect_equal(plot$layers[[1]]$data$zoom, 12)
  expect_equal(plot$layers[[1]]$data$zoomin, 0)
  expect_equal(plot$layers[[1]]$geom_params$progress, "none")
  expect_equal(plot$layers[[length(plot$layers)]]$aes_params$alpha, 0.75)
  expect_equal(plot$labels$caption, "\u00a9 OpenStreetMap contributors")

  expect_error(
    autoplot(result_count, basemap_type = "future_provider"),
    "basemap_attribution"
  )
  custom <- autoplot(
    result_count,
    basemap_type = "future_provider",
    basemap_attribution = "Map tiles \u00a9 Future Provider"
  )
  expect_equal(custom$labels$caption, "Map tiles \u00a9 Future Provider")

  verbose <- autoplot(result_count, basemap_type = "osm", quiet = FALSE)
  expect_equal(verbose$layers[[1]]$geom_params$progress, "text")
  expect_error(autoplot(result_count, quiet = NA), "quiet")
})

test_that("all autoplot methods are quiet by default", {
  methods <- c("n", "k", "c", "d", "dk", "g", "ib", "s")
  for (class in methods) {
    method <- getS3method("autoplot", paste0("hspt_", class))
    expect_identical(formals(method)$quiet, TRUE)
  }
})

test_that("Gi* captions retain base-map attribution", {
  skip_if_not_installed("ggspatial")
  plot <- autoplot(result_gistar, basemap_type = "osm")
  expect_match(plot$labels$caption, "more or fewer points")
  expect_match(plot$labels$caption, "OpenStreetMap contributors")

  automatic <- hotspot_map(
    result_gistar,
    sign = "hot",
    caption = "Analysis by A. Researcher"
  )
  expect_equal(
    strsplit(automatic$labels$caption, "\n", fixed = TRUE)[[1]],
    c(
      "* in areas with more points than expected by chance",
      "Analysis by A. Researcher",
      "\u00a9 OpenStreetMap contributors"
    )
  )
  expect_identical(automatic$theme$plot.caption$hjust, 0)
})

test_that("weighted count outputs map weighted values", {
  weighted <- result_count
  weighted$sum <- result_count$n * 10 + 1

  expect_equal(layer_data(autolayer(result_count))$.plot_value, result_count$n)
  expect_equal(layer_data(autolayer(weighted))$.plot_value, weighted$sum)
  expect_equal(autoplot(result_count)$labels$fill, "count")
  expect_equal(autoplot(weighted)$labels$fill, "weighted count")
  expect_equal(autoplot(weighted)$scales$get_scales("fill")$limits[[1]], 0)
})

test_that("all hotspot classification categories have stable colours", {
  categories <- names(sfhotspot:::hotspot_category_colours)
  classified <- result_classify[seq_along(categories), ]
  classified$hotspot_category <- categories

  plot <- autoplot(classified)
  scale <- plot$scales$get_scales("fill")

  expect_equal(scale$breaks, categories)
  expect_equal(unname(scale$palette(length(categories))),
               unname(sfhotspot:::hotspot_category_colours))
  expect_equal(layer_data(autolayer(classified))$hotspot_category, categories)
})

test_that("change scales are centred on zero with symmetric limits", {
  changed <- result_change
  changed$change <- rep(c(-2, 8), length.out = nrow(changed))
  limits <- autoplot(changed)$scales$get_scales("fill")$limits
  expect_equal(limits, c(-8, 8))

  changed$change <- 0
  expect_equal(
    autoplot(changed)$scales$get_scales("fill")$limits,
    c(-1, 1)
  )
})

test_that("dual-KDE methods use method-specific scales and labels", {
  specifications <- list(
    ratio = list(title = "density ratio", limits = NULL),
    log = list(title = "log density ratio", limits = NULL),
    diff = list(title = "density difference", limits = c(-4, 4)),
    sum = list(title = "combined density", limits = c(0, NA))
  )

  for (method in names(specifications)) {
    object <- result_dual_kde
    kde <- rep(
      if (method == "ratio") c(0.25, 1, 4) else c(-4, 0, 2),
      length.out = nrow(object)
    )
    if (method == "sum") kde <- abs(kde)
    object$kde <- kde
    class(object) <- c("hspt_dk", "hspt_k", setdiff(
      class(object),
      c("hspt_dk", "hspt_k")
    ))
    attr(object, "method") <- method
    plot <- autoplot(object)
    scale <- plot$scales$get_scales("fill")

    expect_equal(plot$labels$fill, specifications[[method]]$title)
    expect_equal(scale$limits, specifications[[method]]$limits)
    expect_equal(scale$get_transformation()$name, "identity")
  }
})

test_that("only difference maps use a diverging scale", {
  for (method in c("ratio", "log", "sum")) {
    object <- result_dual_kde
    object$kde <- rep(c(0.25, 1, 4), length.out = nrow(object))
    attr(object, "method") <- method
    scale <- autoplot(object)$scales$get_scales("fill")

    expect_s3_class(scale, "ScaleContinuous")
    expect_null(scale$midpoint)
    expect_equal(
      scale$palette(c(0, 0.5, 1)),
      c("#EFF3FF", "#6BAED6", "#084594")
    )
  }

  object <- result_dual_kde
  object$kde <- rep(c(-4, 0, 2), length.out = nrow(object))
  attr(object, "method") <- "diff"
  scale <- autoplot(object)$scales$get_scales("fill")
  expect_equal(scale$palette(0.5), "#FFFFFF")
})

test_that("numeric map legends use SI suffixes", {
  scale <- autoplot(result_count)$scales$get_scales("fill")
  expect_equal(scale$get_labels(c(1, 1e3, 1e6)), c("1", "1k", "1M"))
  expect_equal(label_iso(c(12, 999)), c("12", "999"))
  expect_equal(label_iso(1e33), "1,000Q")
})

test_that("density legends reliably label both endpoints", {
  density_plots <- list(
    autoplot(result_kde),
    autoplot(result_gistar, sign = "hot")
  )
  for (method in c("ratio", "log", "diff", "sum")) {
    object <- result_dual_kde
    object$kde <- rep(
      switch(method, ratio = c(0.25, 1, 4), c(-4, 0, 2)),
      length.out = nrow(object)
    )
    if (method == "sum") object$kde <- abs(object$kde)
    attr(object, "method") <- method
    density_plots[[length(density_plots) + 1L]] <- autoplot(object)
  }

  for (plot in density_plots) {
    built_scale <- ggplot2::ggplot_build(plot)$plot$scales$get_scales(
      "fill"
    )
    expect_equal(built_scale$get_labels(), c("low", "high"))
    expect_length(built_scale$get_breaks(), 2)
    expect_lt(built_scale$get_breaks()[[1]], built_scale$get_breaks()[[2]])
  }
})

test_that("dual-KDE layers explicitly handle non-finite values", {
  object <- result_dual_kde[1:5, ]
  object$kde <- c(0, -1, NA, Inf, 2)
  expect_equal(
    layer_data(autolayer(object))$.plot_value,
    c(NA_real_, NA_real_, NA_real_, NA_real_, 2)
  )
})

test_that("Gi* KDE layers apply p-value and sign conditions", {
  object <- result_gistar[1:4, ]
  object$kde <- 1:4
  object$gistar <- c(-2, -1, 1, 2)
  object$pvalue <- c(0.01, 0.1, 0.01, 0.1)

  expect_equal(
    layer_data(autolayer(object))$.plot_value,
    c(-1, NA_real_, 3, NA_real_)
  )
  expect_equal(
    layer_data(autolayer(object, sign = "hot"))$.plot_value,
    c(NA_real_, NA_real_, 3, NA_real_)
  )
  expect_equal(
    layer_data(autolayer(object, sign = "cold"))$.plot_value,
    c(1, NA_real_, NA_real_, NA_real_)
  )
  expect_equal(
    layer_data(autolayer(object, critical_p = 0.2))$.plot_value,
    c(-1, -2, 3, 4)
  )
  expect_equal(autoplot(object)$scales$get_scales("fill")$na.value,
               "transparent")

  object$pvalue <- 1
  expect_no_warning(ggplot2::ggplot_build(autoplot(object)))
})

test_that("Gi* KDE plots use sign-appropriate continuous scales", {
  both <- autoplot(result_gistar, sign = "both")$scales$get_scales("fill")
  expect_equal(both$palette(c(0, 0.5, 1)), c("#2166AC", "#F7F7F7", "#B2182B"))
  expect_equal(both$get_labels(both$limits), c("cold", "hot"))
  expect_equal(both$limits, symmetric_limits(
    gistar_density_values(result_gistar, 0.05, "both")
  ))

  for (sign in c("cold", "hot")) {
    scale <- autoplot(result_gistar, sign = sign)$scales$get_scales("fill")
    expect_equal(scale$palette(c(0, 1)), c("#6BAED6", "#084594"))
    expect_equal(scale$get_labels(c(1, 2)), c("low", "high"))
  }
})

test_that("Gi* plots use audience-appropriate, sign-specific labels", {
  captions <- c(
    hot = "* in areas with more points than expected by chance",
    cold = "* in areas with fewer points than expected by chance",
    both = "* in areas with more or fewer points than expected by chance"
  )

  for (sign in names(captions)) {
    plot <- autoplot(result_gistar, sign = sign)

    expect_equal(plot$labels$fill, "density*")
    expect_equal(plot$labels$caption, captions[[sign]])
  }
})

test_that("Gi* values are mapped directly when KDE is absent", {
  layer <- autolayer(
    result_gistar_no_kde,
    critical_p = 0.001,
    sign = "cold"
  )
  plot <- autoplot(result_gistar_no_kde)

  expect_equal(layer_data(layer)$.plot_value, result_gistar_no_kde$gistar)
  expect_equal(plot$labels$fill, "Gi* statistic")
  expect_equal(
    plot$scales$get_scales("fill")$limits,
    symmetric_limits(result_gistar_no_kde$gistar)
  )

  statistic_only <- result_gistar_no_kde[, "gistar"]
  expect_no_error(autoplot(statistic_only))
})
