test_that("hotspot_map maps ordinary sf objects", {
  object <- memphis_robberies[1:4, ]
  plot <- hotspot_map(
    object,
    mapping = ggplot2::aes(colour = offense_type),
    size = 2,
    basemap_type = "none"
  )

  expect_s3_class(plot, "ggplot")
  expect_named(plot$layers[[1]]$mapping, "colour")
  expect_equal(plot$layers[[1]]$aes_params$size, 2)
  expect_no_condition(ggplot2::ggplot_build(plot))
})

test_that("hotspot_map validates mappings for ordinary sf objects", {
  object <- memphis_robberies[1:2, ]
  expect_error(
    hotspot_map(object, mapping = "offense_type", basemap_type = "none"),
    "ggplot2::aes"
  )
})

test_that("hotspot_map defaults to an OSM base map and forces transparency", {
  plot <- hotspot_map(memphis_robberies[1:2, ], alpha = 1)

  expect_s3_class(plot$layers[[1]]$geom, "GeomMapTile")
  expect_equal(plot$layers[[length(plot$layers)]]$aes_params$alpha, 0.75)
  expect_match(plot$labels$caption, "OpenStreetMap")
})

test_that("hotspot_map combines user captions with automatic captions", {
  object <- memphis_robberies[1:2, ]

  with_basemap <- hotspot_map(object, caption = "Map by A. Researcher")
  expect_equal(
    with_basemap$labels$caption,
    paste("Map by A. Researcher", "\u00a9 OpenStreetMap contributors", sep = "\n")
  )

  without_basemap <- hotspot_map(
    object,
    basemap_type = "none",
    caption = "Map by A. Researcher"
  )
  expect_equal(without_basemap$labels$caption, "Map by A. Researcher")

  custom_attribution <- hotspot_map(
    object,
    basemap_type = "future_provider",
    basemap_attribution = "Map tiles \u00a9 Future Provider",
    caption = "Map by A. Researcher"
  )
  expect_equal(
    custom_attribution$labels$caption,
    paste("Map by A. Researcher", "Map tiles \u00a9 Future Provider", sep = "\n")
  )
})

test_that("hotspot_map captions are left aligned", {
  object <- memphis_robberies[1:2, ]

  expect_identical(
    hotspot_map(object)$theme$plot.caption$hjust,
    0
  )
  expect_identical(
    hotspot_map(object, basemap_type = "none", caption = "Map caption")$
      theme$plot.caption$hjust,
    0
  )
})

test_that("hotspot_map validates user captions", {
  object <- memphis_robberies[1:2, ]
  for (caption in list("", NA_character_, c("one", "two"), 1)) {
    expect_error(
      hotspot_map(object, basemap_type = "none", caption = caption),
      "single non-empty string"
    )
  }
})

test_that("automatic maps preserve the default legend position", {
  result <- hotspot_count(
    memphis_robberies,
    cell_size = 0.01,
    quiet = TRUE
  )

  default_position <- ggplot2::theme_get()$legend.position
  expect_identical(
    autoplot(result)$theme$legend.position,
    default_position
  )
  expect_identical(
    hotspot_map(result, basemap_type = "none")$theme$legend.position,
    default_position
  )
  expect_identical(
    hotspot_map(
      memphis_robberies,
      mapping = ggplot2::aes(colour = offense_type),
      basemap_type = "none"
    )$theme$legend.position,
    default_position
  )
})

test_that("base-map padding is ten percent of the longest side", {
  object <- memphis_robberies[1:4, ]
  bbox <- sf::st_bbox(object)
  padding <- 0.1 * max(
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

  with_basemap <- hotspot_map(object)
  without_basemap <- hotspot_map(object, basemap_type = "none")
  x_scale <- with_basemap$scales$get_scales("x")
  y_scale <- with_basemap$scales$get_scales("y")

  expect_equal(
    x_scale$limits,
    unname(basemap_bbox[c("xmin", "xmax")])
  )
  expect_equal(
    y_scale$limits,
    unname(basemap_bbox[c("ymin", "ymax")])
  )
  expect_equal(x_scale$expand, ggplot2::expansion(0))
  expect_equal(y_scale$expand, ggplot2::expansion(0))
  expect_equal(x_scale$oob(c(-Inf, 0, Inf), x_scale$limits), c(-Inf, 0, Inf))
  expect_equal(y_scale$oob(c(-Inf, 0, Inf), y_scale$limits), c(-Inf, 0, Inf))
  expect_null(with_basemap$coordinates$crs)
  expect_null(without_basemap$coordinates$limits$x)
  expect_null(without_basemap$coordinates$limits$y)
  expect_true(without_basemap$coordinates$expand)
})

test_that("base-map layers use Web Mercator for objects in another CRS", {
  object <- sf::st_transform(memphis_robberies[1:4, ], 26915)

  plot <- hotspot_map(object)
  built <- ggplot2::ggplot_build(plot)
  point_geometry <- built$data[[length(built$data)]]$geometry

  expect_equal(sf::st_crs(point_geometry), sf::st_crs(3857))
  expect_true(all(!sf::st_is_empty(point_geometry)))
  expect_true(all(sf::st_bbox(point_geometry)[c("xmin", "xmax")] >=
    min(built$layout$panel_params[[1]]$x_range)))
  expect_true(all(sf::st_bbox(point_geometry)[c("xmin", "xmax")] <=
    max(built$layout$panel_params[[1]]$x_range)))
  expect_true(all(sf::st_bbox(point_geometry)[c("ymin", "ymax")] >=
    min(built$layout$panel_params[[1]]$y_range)))
  expect_true(all(sf::st_bbox(point_geometry)[c("ymin", "ymax")] <=
    max(built$layout$panel_params[[1]]$y_range)))
})

test_that("sf layers can be added without replacing the map coordinates", {
  object <- sf::st_transform(memphis_robberies[1:4, ], 26915)
  overlay <- memphis_robberies[5:8, ]
  plot <- hotspot_map(object)

  expect_no_message(
    combined <- plot + ggplot2::geom_sf(data = overlay)
  )
  built <- ggplot2::ggplot_build(combined)

  expect_equal(
    sf::st_crs(built$data[[length(built$data)]]$geometry),
    sf::st_crs(3857)
  )
  expect_equal(
    built$layout$panel_params[[1]]$x_range,
    plot$scales$get_scales("x")$limits
  )
  expect_equal(
    built$layout$panel_params[[1]]$y_range,
    plot$scales$get_scales("y")$limits
  )
})

test_that("ordinary sf maps without a base map do not require a CRS", {
  object <- memphis_robberies[1:2, ]
  sf::st_crs(object) <- NA

  expect_error(hotspot_map(object), "reference system.*missing")
  expect_s3_class(hotspot_map(object, basemap_type = "none"), "ggplot")
})

test_that("hotspot_map reports unusable base-map extents", {
  zero_rows <- memphis_robberies[0, ]
  empty <- memphis_robberies[1, ]
  sf::st_geometry(empty) <- sf::st_sfc(sf::st_point(), crs = sf::st_crs(empty))

  expect_error(hotspot_map(zero_rows), "zero rows")
  expect_error(hotspot_map(empty), "missing geometry")
})

test_that("hotspot result methods reject mapping with the producer name", {
  producers <- c(
    hspt_n = "hotspot_count",
    hspt_k = "hotspot_kde",
    hspt_c = "hotspot_classify",
    hspt_d = "hotspot_change",
    hspt_dk = "hotspot_dual_kde",
    hspt_g = "hotspot_gistar",
    hspt_ib = "hotspot_isoband",
    hspt_s = "hotspot_dbscan"
  )

  for (class_name in names(producers)) {
    object <- memphis_robberies[1, ]
    class(object) <- c(class_name, setdiff(class(object), class_name))
    expect_error(
      hotspot_map(
        object,
        mapping = ggplot2::aes(colour = offense_type),
        basemap_type = "none"
      ),
      producers[[class_name]],
      fixed = TRUE
    )
  }
})

test_that("all hotspot_map methods are registered", {
  classes <- c("n", "k", "c", "d", "dk", "g", "ib", "s")
  for (class in classes) {
    expect_true(is.function(getS3method("hotspot_map", paste0("hspt_", class))))
  }
})

test_that("hotspot_map delegates hotspot rendering to autoplot", {
  result <- hotspot_count(
    memphis_robberies,
    cell_size = 0.01,
    quiet = TRUE
  )
  automatic <- hotspot_map(result, basemap_type = "none")
  conventional <- autoplot(result)

  expect_equal(length(automatic$layers), length(conventional$layers))
  expect_equal(automatic$labels, conventional$labels)
  expect_equal(
    automatic$scales$get_scales("fill")$limits,
    conventional$scales$get_scales("fill")$limits
  )
})
