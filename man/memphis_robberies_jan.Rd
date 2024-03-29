% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/memphis_robberies_jan.R
\docType{data}
\name{memphis_robberies_jan}
\alias{memphis_robberies_jan}
\title{Personal robberies in Memphis in January 2019}
\format{
A simple-features tibble with 206 rows and four variables:
\describe{
\item{uid}{a unique identifier for each robbery}
\item{offense_type}{the type of crime (always 'personal robbery')}
\item{date}{the date and time at which the crime occurred}
\item{geometry}{the co-ordinates at which the crime occurred, stored in
simple-features point format}
}
}
\source{
Crime Open Database, \url{https://osf.io/zyaqn/}
}
\usage{
memphis_robberies_jan
}
\description{
A dataset containing records of personal robberies recorded by police in
Memphis, Tennessee, in January 2019. This dataset is too small for some types
of analysis but is included for testing purposes.
}
\keyword{datasets}
