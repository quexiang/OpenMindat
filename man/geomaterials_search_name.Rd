% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mindat_geomaterials.R
\name{geomaterials_search_name}
\alias{geomaterials_search_name}
\title{retrieve the geomaterials by a given name.}
\usage{
geomaterials_search_name(name,...)
}
\arguments{
\item{name}{string,}

\item{..., }{Further named parameters.Other optional arguments.}
}
\value{
df, a list of geomaterials
}
\description{
: Queries the list of geomaterials by a given name.
}
\details{
retrieve the geomaterial list that match the given name.
}
\examples{
geomaterials_search_name("Quartz")
}
