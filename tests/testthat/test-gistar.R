# Gi* runtime grows rapidly with the number of grid cells. This deliberately
# small fixture retains non-zero and zero-count cells, so it exercises the same
# statistical branches without turning a correctness test into a scale test.
data_sf <- head(memphis_robberies, 100)
cell_size <- 0.02
counts <- count_points_in_polygons(
  points = data_sf,
  polygons = sf::st_set_geometry(
    create_grid(data_sf, cell_size = cell_size),
    "random_geom_column"
  )
)

# To speed up the checking process, run the function with arguments that should
# not produce any errors or warnings
result <- gistar(counts = counts)


# CHECK INPUTS -----------------------------------------------------------------

# Note that only inputs evaluated in `hotspot_kde()` are tested here; those
# evaluated in helper functions are tested in the test files for those functions


## Errors ----

test_that("error if `data` is not an SF object", {
  expect_error(gistar(counts = sf::st_drop_geometry(counts)))
})

test_that("error if counts are missing or are not numeric", {
  expect_error(gistar(counts = counts[, setdiff(names(counts), "n")]),
               "named `n` or `sum`")
  counts_character <- counts
  counts_character$n <- as.character(counts_character$n)
  expect_error(gistar(counts = counts_character), "must be numeric")
})

test_that("error if inputs don't have correct types", {
  expect_error(gistar(counts = counts, nb_dist = character()))
  expect_error(gistar(counts = counts, cell_size = character()))
  expect_error(gistar(counts = counts, include_self = character()))
  expect_error(gistar(counts = counts, p_adjust_method = 1))
  expect_error(gistar(counts = counts, quiet = character()))
})

test_that("error if inputs aren't of length 1", {
  expect_error(gistar(counts = counts, nb_dist = 1:2))
  expect_error(gistar(counts = counts, cell_size = 1:2))
  expect_error(gistar(counts = counts, include_self = c(TRUE, FALSE)))
  expect_error(gistar(
    counts = counts,
    p_adjust_method = stats::p.adjust.methods[1:2])
  )
  expect_error(gistar(counts = counts, quiet = c(TRUE, FALSE)))
})

test_that("error if values are of the correct type/length but are invalid", {
  expect_error(gistar(counts = counts, nb_dist = -1))
  expect_error(gistar(counts = counts, cell_size = -1))
  expect_error(
    gistar(counts = counts, p_adjust_method = "some other method")
  )
})



# CHECK OUTPUTS ----------------------------------------------------------------


## Correct outputs ----

test_that("function produces an SF tibble", {
  expect_s3_class(result, "sf")
  expect_s3_class(result, "tbl_df")
})

test_that("output object has the required column names", {
  expect_equal(sort(names(result)), c("geometry", "gistar", "n", "pvalue"))
})

test_that("columns in output have the required types", {
  expect_type(result$n, "double")
  expect_type(result$gistar, "double")
  expect_type(result$pvalue, "double")
  expect_true(sf::st_is(result$geometry[[1]], "POLYGON"))
})

test_that("column values are within the specified range", {
  expect_true(all(result$n >= 0))
  expect_true(all(result$pvalue >= 0))
  expect_true(all(result$pvalue <= 1))
})

test_that("NULL uses the default p-value adjustment method (#94)", {
  # Leave cell size implicit in both calls so the test compares only the default
  # adjustment method while retaining coverage of automatic grid inference.
  holm_result <- gistar(counts, p_adjust_method = "holm")

  expect_equal(result$pvalue, holm_result$pvalue)
})

test_that("weighted counts are used when present (#86)", {
  weighted_counts <- counts
  weighted_counts$sum <- seq_len(nrow(weighted_counts))^2

  # These calls test weighting, not cell-size inference, so use the known toy
  # cell size to keep both calculations focused and fast.
  weighted_result <- gistar(weighted_counts, cell_size = cell_size)
  unweighted_result <- gistar(
    weighted_counts[, setdiff(names(weighted_counts), "sum")],
    cell_size = cell_size
  )

  expect_false(isTRUE(all.equal(
    weighted_result$gistar,
    unweighted_result$gistar
  )))
  expect_false(isTRUE(all.equal(
    weighted_result$pvalue,
    unweighted_result$pvalue
  )))
})
