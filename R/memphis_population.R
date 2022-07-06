#' Populations of census blocks in Memphis in 2020
#'
#' A dataset containing records of populations associated with the centroids of
#' census blocks in Memphis, Tennessee, in 2020.
#'
#' @format A simple-features tibble with 10,393 rows and three variables:
#' \describe{
#'   \item{geoid}{the census GEOID for each block}
#'   \item{population}{the number of people residing in each block}
#'   \item{geometry}{the co-ordinates of the centroid of each block, stored in
#'     simple-features point format}
#' }
#'
#' @source US Census Bureau. Census 2020, Redistricting Data summary file.
#'   <https://www.census.gov/programs-surveys/decennial-census/about/rdo/summary-files.html>
"memphis_population"
