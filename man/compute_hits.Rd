% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compute_hits.R
\name{compute_hits}
\alias{compute_hits}
\title{For multilibrary enrichment, identify libraries, and identify values above threshold.
for each.}
\usage{
compute_hits(data, threshold = 5, parallel = FALSE, verbose = FALSE)
}
\arguments{
\item{data}{List with first element a character vector of library names and
subsequent elements data frames of data for corresponding libraries.}

\item{threshold}{Minimum enrichment value to be considered a hit.}

\item{parallel}{Logical whether or not to use future and future.apply to parallelize individual library computations.}

\item{verbose}{Logical whether to print additional info}
}
\description{
For multilibrary enrichment, identify libraries, and identify values above threshold.
for each.
}
