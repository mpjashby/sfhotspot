% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hotspot_classify_params.R
\name{hotspot_classify_params}
\alias{hotspot_classify_params}
\title{Control the parameters used to classify hotspots}
\usage{
hotspot_classify_params(
  hotspot_prop = 0.1,
  persistent_prop = 0.8,
  recent_prop = 0.2,
  critical_p = 0.05,
  nb_dist = NULL,
  include_self = TRUE,
  p_adjust_method = NULL
)
}
\arguments{
\item{hotspot_prop}{A single numeric value specifying the minimum proportion
of periods for which a cell must contain significant clusters of points
before the cell can be classified as a hot or cold spot of any type.}

\item{persistent_prop}{A single numeric value specifying the minimum
proportion of periods for which a cell must contain significant clusters of
points before the cell can be classified as a persistent hot or cold spot.}

\item{recent_prop}{A single numeric value specifying the proportion of
periods that should be treated as being recent in the classification of
emerging and former hotspots.}

\item{critical_p}{A threshold \emph{p}-value below which values should be
treated as being statistically significant.}

\item{nb_dist}{The distance around a cell that contains the neighbours of
that cell, which are used in calculating the statistic. If this argument is
\code{NULL} (the default), \code{nb_dist} is set as \code{cell_size *
  sqrt(2)} so that only the cells immediately adjacent to each cell are
treated as being its neighbours.}

\item{include_self}{Should points in a given cell be counted as well as
counts in neighbouring cells when calculating the values of
\ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G^*_i}}
(if \code{include_self = TRUE}, the default) or
\ifelse{html}{\out{<i>G</i><sub><i>i</i></sub><sup>*</sup>}}{\eqn{G_i}}
(if \code{include_self = FALSE}) values? You are unlikely to want to change
the default value.}

\item{p_adjust_method}{The method to be used to adjust \emph{p}-values for
multiple comparisons. \code{NULL} (the default) uses the default method
used by \code{\link[stats]{p.adjust}}, but any of the character values in
\code{stats::p.adjust.methods} may be specified.}
}
\value{
A list that can be used as the input to the \code{params} argument to
\code{\link{hotspot_classify}}.
}
\description{
This function allows specification of parameters that affect the output from
\code{\link{hotspot_classify}}.
}
