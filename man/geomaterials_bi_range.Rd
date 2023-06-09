% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mindat_geomaterials.R
\name{geomaterials_bi_range}
\alias{geomaterials_bi_range}
\title{retrieve the geomaterials whose birifrigence are higher and lower than the given value.}
\usage{
geomaterials_bi_range(gt,lt, ...)
}
\arguments{
\item{gt}{float value}

\item{lt}{float value}

\item{..., }{Further named parameters.Other optional arguments.}
}
\value{
df, a data frame of geomaterials
}
\description{
: Queries the list of geomaterials that have lower birifrigence than lt.
}
\details{
retrieve all the geomaterials that has the birifrigence within the range of (gt,lt).
}
\examples{
geomaterials_bi_range(0.6,0.7)
}
