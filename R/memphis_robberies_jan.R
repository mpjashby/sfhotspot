#' Personal robberies in Memphis in January 2019
#'
#' A dataset containing records of personal robberies recorded by police in
#' Memphis, Tennessee, in January 2019. This dataset is too small for some types
#' of analysis but is included for testing purposes.
#'
#' @format A simple-features tibble with 206 rows and four variables:
#' \describe{
#'   \item{uid}{a unique identifier for each robbery}
#'   \item{offense_type}{the type of crime (always 'personal robbery')}
#'   \item{date}{the date and time at which the crime occurred}
#'   \item{geometry}{the co-ordinates at which the crime occurred, stored in
#'     simple-features point format}
#' }
#'
#' @source Crime Open Database, \url{https://osf.io/zyaqn/}
"memphis_robberies_jan"
