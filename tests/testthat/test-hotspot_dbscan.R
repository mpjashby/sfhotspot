make_dbscan_points <- function() {
  # Two four-point clusters with equal counts but different areas, surrounded
  # by four noise points that keep buffered cluster hulls inside the study hull
  coordinates <- rbind(
    c(-2.3, -0.3), c(-2.3, 0.3), c(-1.7, -0.3), c(-1.7, 0.3),
    c(1.9, -0.1), c(1.9, 0.1), c(2.1, -0.1), c(2.1, 0.1),
    c(-6, -6), c(-6, 6), c(6, -6), c(6, 6)
  )
  sf::st_as_sf(
    data.frame(id = seq_len(nrow(coordinates)), coordinates),
    coords = c("X1", "X2"),
    crs = 3857
  )
}

test_that("hotspot_dbscan returns ranked tidy polygon output", {
  result <- hotspot_dbscan(
    make_dbscan_points(), eps = 1, min_pts = 3, quiet = TRUE
  )

  expect_s3_class(result, "hspt_s")
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
  expect_named(result, c("cluster", "rank", "n", "prop", "geometry"))
  expect_type(result$cluster, "integer")
  expect_type(result$rank, "integer")
  expect_type(result$n, "integer")
  expect_type(result$prop, "double")
  expect_identical(result$rank, 1:2)
  expect_identical(result$n, c(4L, 4L))
  expect_equal(result$prop, c(4 / 12, 4 / 12))
  expect_identical(result$cluster, c(2L, 1L))
  expect_true(all(sf::st_is(result, c("POLYGON", "MULTIPOLYGON"))))
  expect_true(all(sf::st_is_valid(result)))
})

test_that("MULTIPOINT coordinates are counted individually", {
  multipoint <- sf::st_sf(
    source = "one row",
    geometry = sf::st_sfc(sf::st_multipoint(matrix(
      c(0, 0, 0.1, 0, 0, 0.1, 0.1, 0.1, 0.05, 0.05),
      ncol = 2,
      byrow = TRUE
    )), crs = 3857)
  )

  result <- hotspot_dbscan(multipoint, eps = 0.2, min_pts = 3, quiet = TRUE)
  expect_identical(result$n, 5L)
  expect_identical(result$prop, 1)
})

test_that("polygon counts include noise points inside the final hull", {
  # The points on the circle form one DBSCAN cluster, while the centre point is
  # noise because it is farther than eps from every point on the circle. The
  # convex cluster polygon nevertheless contains all 25 input coordinates.
  angle <- seq(0, 2 * pi, length.out = 25)[-25]
  coordinates <- rbind(
    cbind(x = 3 * cos(angle), y = 3 * sin(angle)),
    c(x = 0, y = 0)
  )
  data <- sf::st_as_sf(
    data.frame(id = seq_len(nrow(coordinates)), coordinates),
    coords = c("x", "y"),
    crs = 3857
  )

  result <- hotspot_dbscan(data, eps = 1, min_pts = 3, quiet = TRUE)

  expect_identical(result$n, 25L)
  expect_identical(result$prop, 1)
})

test_that("automatic eps uses global convex-hull density", {
  data <- make_dbscan_points()
  area <- as.numeric(sf::st_area(sf::st_convex_hull(sf::st_union(data))))
  expected <- sqrt(((3 - 1) * area) / (pi * nrow(data) * 2))

  expect_message(
    value <- set_dbscan_eps(
      area,
      n = nrow(data),
      min_pts = 3,
      density_adjust = 2,
      data = data,
      quiet = FALSE
    ),
    "Neighbourhood distance set automatically"
  )
  expect_equal(value, expected)
  expect_no_message(set_dbscan_eps(
    area,
    n = nrow(data),
    min_pts = 3,
    density_adjust = 2,
    data = data,
    quiet = TRUE
  ))
})

test_that("automatic eps is recorded in metadata", {
  result <- hotspot_dbscan(memphis_robberies_jan, quiet = TRUE)
  metadata <- attr(result, "dbscan")

  expect_true(metadata$eps_auto)
  expect_true(is.finite(metadata$eps) && metadata$eps > 0)
  expect_identical(metadata$min_pts, 5L)
  expect_identical(metadata$density_adjust, 2)
  expect_identical(metadata$hull, "concave")
  expect_identical(metadata$hull_ratio, 0.75)
})

test_that("geographic data are transformed and restored", {
  expect_message(
    result <- hotspot_dbscan(memphis_robberies_jan, quiet = FALSE),
    "Data transformed to"
  )
  expect_identical(sf::st_crs(result), sf::st_crs(memphis_robberies_jan))
  expect_error(
    hotspot_dbscan(memphis_robberies_jan, transform = FALSE, quiet = TRUE),
    "Cannot calculate DBSCAN clusters for lon/lat data"
  )
})

test_that("convex and concave hulls are supported", {
  data <- make_dbscan_points()
  convex <- hotspot_dbscan(
    data, eps = 1, min_pts = 3, hull = "convex", quiet = TRUE
  )
  expect_identical(attr(convex, "dbscan")$hull, "convex")

  skip_if(
    utils::compareVersion(sf::sf_extSoftVersion()[["GEOS"]], "3.11.0") < 0,
    "concave hulls require GEOS 3.11"
  )
  concave <- hotspot_dbscan(
    data,
    eps = 1,
    min_pts = 3,
    hull = "concave",
    hull_ratio = 0.25,
    quiet = TRUE
  )
  expect_identical(attr(concave, "dbscan")$hull, "concave")
  expect_identical(attr(concave, "dbscan")$hull_ratio, 0.25)
  expect_true(all(sf::st_is_valid(concave)))
})

test_that("DBSCAN parameters are validated", {
  data <- make_dbscan_points()
  expect_error(hotspot_dbscan(data, eps = 0), "eps.*greater than zero")
  expect_error(hotspot_dbscan(data, eps = Inf), "eps.*finite")
  expect_error(hotspot_dbscan(data, eps = 1, min_pts = 1), "min_pts")
  expect_error(hotspot_dbscan(data, eps = 1, min_pts = 2.5), "min_pts")
  expect_error(hotspot_dbscan(data, eps = 1, min_pts = 20), "cannot exceed")
  expect_error(
    hotspot_dbscan(data, eps = 1, density_adjust = 0),
    "density_adjust"
  )
  expect_error(hotspot_dbscan(data, eps = 1, hull = "triangle"), "hull")
  expect_error(hotspot_dbscan(data, eps = 1, hull_ratio = -0.1), "hull_ratio")
  expect_error(hotspot_dbscan(data, eps = 1, transform = 1), "transform")
  expect_error(hotspot_dbscan(data, eps = 1, quiet = 1), "quiet")
})

test_that("reserved and supported dot arguments are handled", {
  data <- make_dbscan_points()
  expect_no_error(hotspot_dbscan(
    data, eps = 1, min_pts = 3, search = "linear", quiet = TRUE
  ))
  expect_no_error(hotspot_dbscan(
    data, eps = 1, min_pts = 3, borderPoints = FALSE, quiet = TRUE
  ))
  expect_error(hotspot_dbscan(data, eps = 1, weights = 1), "not supported")
  expect_error(hotspot_dbscan(data, eps = 1, minPts = 3), "reserved")
  expect_error(
    hotspot_dbscan(
      data = data,
      eps = 1,
      min_pts = 3,
      density_adjust = 1,
      hull = "convex",
      hull_ratio = 0.5,
      transform = TRUE,
      quiet = TRUE,
      10
    ),
    "must be named"
  )
})

test_that("all-noise and degenerate automatic inputs give useful errors", {
  data <- make_dbscan_points()
  expect_error(
    hotspot_dbscan(data, eps = 0.01, min_pts = 3, quiet = TRUE),
    "No DBSCAN hotspots"
  )

  collinear <- sf::st_as_sf(
    data.frame(x = 1:5, y = 1:5), coords = c("x", "y"), crs = 3857
  )
  expect_error(
    hotspot_dbscan(collinear, quiet = TRUE),
    "convex hull.*zero area"
  )
  expect_s3_class(
    hotspot_dbscan(collinear, eps = 2, min_pts = 2, quiet = TRUE),
    "hspt_s"
  )
})

test_that("hspt_s class and metadata survive hotspot_clip", {
  result <- hotspot_dbscan(
    make_dbscan_points(), eps = 1, min_pts = 3, quiet = TRUE
  )
  boundary <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(-5, -5, 5, -5, 5, 5, -5, 5, -5, -5),
      ncol = 2,
      byrow = TRUE
    ))),
    crs = 3857
  ))
  clipped <- hotspot_clip(result, boundary, quiet = TRUE)

  expect_s3_class(clipped, "hspt_s")
  expect_identical(attr(clipped, "dbscan"), attr(result, "dbscan"))
})

test_that("DBSCAN results cannot be converted to isobands", {
  result <- hotspot_dbscan(
    make_dbscan_points(), eps = 1, min_pts = 3, quiet = TRUE
  )
  expect_error(hotspot_isoband(result, quiet = TRUE), "cannot be converted")
})

test_that("DBSCAN results have configurable plot fills and labels", {
  result <- hotspot_dbscan(
    make_dbscan_points(), eps = 1, min_pts = 3, quiet = TRUE
  )

  expect_s3_class(autoplot(result), "ggplot")
  expect_equal(autolayer(result)[[1]]$data$.plot_value, result$n)
  expect_equal(autolayer(result, fill = "prop")[[1]]$data$.plot_value,
               result$prop)
  expect_equal(autolayer(result, fill = "rank")[[1]]$data$.plot_value,
               result$rank)
  expect_equal(
    autoplot(result)$scales$get_scales("fill")$palette(c(0, 1)),
    c("#EFF3FF", "#084594")
  )

  expect_length(autoplot(result)$layers, 1)
  expect_equal(autolayer(result, label = "n")[[2]]$data$.plot_label,
               as.character(result$n))
  expect_equal(autolayer(result, label = "prop")[[2]]$data$.plot_label,
               c("33.3%", "33.3%"))
  expect_equal(autolayer(result, label = "rank")[[2]]$data$.plot_label,
               c("1st", "2nd"))
  expect_equal(
    autolayer(result, label = c("n", "prop"))[[2]]$data$.plot_label,
    c("n = 4\n33.3%", "n = 4\n33.3%")
  )
  expect_equal(
    autolayer(result, label = c("rank", "n"))[[2]]$data$.plot_label,
    c("1st\nn = 4", "2nd\nn = 4")
  )
  expect_length(autoplot(result, label = "rank")$layers, 2)
  expect_no_condition(ggplot2::ggplot_build(autoplot(result, label = "prop")))

  outline <- autolayer(result, fill = "none")
  expect_equal(outline[[1]]$data$.plot_value, result$n)
  expect_true(is.na(outline[[1]]$aes_params$fill))
  expect_equal(outline[[1]]$aes_params$linewidth, 0.8)
  expect_true("colour" %in% names(outline[[1]]$mapping))
  outline_plot <- autoplot(result, fill = "none", label = c("rank", "n"))
  expect_null(outline_plot$scales$get_scales("fill"))
  expect_equal(outline_plot$labels$colour, "rank")
  expect_equal(
    outline_plot$scales$get_scales("colour")$palette(c(0, 1)),
    c("#EFF3FF", "#084594")
  )

  geographic <- sf::st_transform(result, 4326)
  expect_no_warning(
    ggplot2::ggplot_build(autoplot(geographic, label = "n"))
  )
})

test_that("DBSCAN plotting arguments and columns are validated", {
  result <- hotspot_dbscan(
    make_dbscan_points(), eps = 1, min_pts = 3, quiet = TRUE
  )

  expect_error(autoplot(result, fill = "density"), "fill")
  expect_error(autolayer(result, label = "cluster"), "label")
  expect_error(autolayer(result, label = c("none", "n")), "cannot be combined")
  expect_error(autolayer(result, label = c("n", "n")), "duplicated")
  expect_error(autolayer(result[, "geometry"]), "n")
  result$n <- as.character(result$n)
  expect_error(autoplot(result), "must be numeric")
})
