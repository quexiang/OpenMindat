% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mindat_geomaterials.R
\name{geomaterials_fracturetype}
\alias{geomaterials_fracturetype}
\title{retrieve the geomaterials that have the given fracturetype.}
\usage{
geomaterials_fracturetype(types, ...)
}
\arguments{
\item{types}{list of types.}

\item{..., }{Further named parameters.Other optional arguments.}
}
\value{
df, a data frame of geomaterials
}
\description{
: Queries the list of geomaterials that have the given types.
}
\details{
Items Enum: "Conchoidal" "Fibrous" "Hackly" "Irregular/Uneven" "Micaceous" "None observed" "Splintery" "Step-Like" "Sub-Conchoidal"
}
\examples{
geomaterials_fracturetype(c('Conchoidal'))
geomaterials_fracturetype(c('Conchoidal','Fibrous'))
}
