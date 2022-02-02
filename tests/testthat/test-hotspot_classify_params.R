# All the tests in this file are commented out until `hotspot_classify_params()`
# is included in the package, which will be when `hotspot_classify()` is ready

# CHECK INPUTS -----------------------------------------------------------------
#
#
# ## Errors ----
#
# test_that("error if inputs don't have correct types", {
#   expect_error(hotspot_classify_params(hotspot_prop = character()))
#   expect_error(hotspot_classify_params(persistent_prop = character()))
#   expect_error(hotspot_classify_params(recent_prop = character()))
#   expect_error(hotspot_classify_params(critical_p = character()))
#   expect_error(hotspot_classify_params(nb_dist = character()))
#   expect_error(hotspot_classify_params(include_self = character()))
#   expect_error(hotspot_classify_params(p_adjust_method = 1))
# })
#
# test_that("error if inputs aren't of length 1", {
#   expect_error(hotspot_classify_params(hotspot_prop = 1:2))
#   expect_error(hotspot_classify_params(persistent_prop = 1:2))
#   expect_error(hotspot_classify_params(recent_prop = 1:2))
#   expect_error(hotspot_classify_params(critical_p = 1:2))
#   expect_error(hotspot_classify_params(nb_dist = 1:2))
#   expect_error(hotspot_classify_params(include_self = c(TRUE, FALSE)))
#   expect_error(
#     hotspot_classify_params(p_adjust_method = stats::p.adjust.methods[1:2])
#   )
# })
#
# test_that("error if values are of the correct type/length but are invalid", {
#   expect_error(hotspot_classify_params(hotspot_prop = 2))
#   expect_error(hotspot_classify_params(persistent_prop = 2))
#   expect_error(hotspot_classify_params(recent_prop = 2))
#   expect_error(hotspot_classify_params(critical_p = 2))
#   expect_error(hotspot_classify_params(nb_dist = -1))
#   expect_error(hotspot_classify_params(p_adjust_method = "some other method"))
# })
#
#
#
# CHECK OUTPUTS ----------------------------------------------------------------
#
#
# ## Correct outputs ----
#
# test_that("output list contains required elements", {
#   expect_identical(
#     names(hotspot_classify_params()),
#     c(
#       "hotspot_prop", "persistent_prop", "recent_prop", "critical_p",
#       "nb_dist", "include_self", "p_adjust_method"
#     )
#   )
# })
