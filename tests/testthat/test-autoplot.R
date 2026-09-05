set.seed(123)

data_sf <- memphis_robberies
data_df <- as.data.frame(sf::st_drop_geometry(data_sf))
result_count <- hotspot_count(data_sf, cell_size = 0.01, quiet = TRUE)
result_kde <- hotspot_kde(
  sf::st_transform(data_sf, 32616),
  cell_size = 1000,
  quiet = TRUE
)
result_classify <- hotspot_classify(
  memphis_robberies,
  cell_size = 0.01,
  quiet = TRUE
)
result_change <- hotspot_change(data_sf, quiet = TRUE)

result_gistar <- result_kde
result_gistar$gistar <- rep(c(-2, -1, 1, 2), length.out = nrow(result_gistar))
result_gistar$pvalue <- rep(c(0.01, 0.1), length.out = nrow(result_gistar))
class(result_gistar) <- c(
  "hspt_g",
  setdiff(class(result_gistar), "hspt_k")
)
result_gistar_no_kde <- dplyr::select(result_gistar, -kde)

result_dual_kde <- result_kde
class(result_dual_kde) <- c("hspt_dk", class(result_dual_kde))
attr(result_dual_kde, "method") <- "ratio"

layer_data <- function(layer) layer[[1]]$data



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

  missing_pvalue <- dplyr::select(result_gistar, -pvalue)
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

test_that("weighted count outputs map weighted values", {
  weighted <- result_count
  weighted$sum <- result_count$n * 10 + 1

  expect_equal(layer_data(autolayer(result_count))$.plot_value, result_count$n)
  expect_equal(layer_data(autolayer(weighted))$.plot_value, weighted$sum)
  expect_equal(autoplot(result_count)$labels$fill, "Count")
  expect_equal(autoplot(weighted)$labels$fill, "Weighted count")
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
    ratio = list(title = "Density ratio", limits = log10(c(0.25, 4))),
    log = list(title = "Log density ratio", limits = c(-4, 4)),
    diff = list(title = "Density difference", limits = c(-4, 4)),
    sum = list(title = "Combined density", limits = c(0, NA))
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
    expect_equal(
      scale$get_transformation()$name,
      if (method == "ratio") "log-10" else "identity"
    )
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
    c(1, NA_real_, 3, NA_real_)
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
    object$kde
  )
  expect_equal(autoplot(object)$scales$get_scales("fill")$na.value,
               "transparent")

  object$pvalue <- 1
  expect_no_warning(ggplot2::ggplot_build(autoplot(object)))
})

test_that("Gi* plots use audience-appropriate, sign-specific labels", {
  expect_equal(
    autoplot(result_gistar, sign = "hot")$labels$fill,
    "Density in areas with more points than expected by chance"
  )
  expect_equal(
    autoplot(result_gistar, sign = "cold")$labels$fill,
    "Density in areas with fewer points than expected by chance"
  )
  expect_equal(
    autoplot(result_gistar)$labels$fill,
    "Density in areas with more or fewer points than expected by chance"
  )
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

  statistic_only <- dplyr::select(result_gistar_no_kde, gistar)
  expect_no_error(autoplot(statistic_only))
})
