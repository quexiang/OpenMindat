% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mindat_querys.R
\name{mindat_localitiy}
\alias{mindat_localitiy}
\title{mindat_localitiy}
\usage{
mindat_localitiy (id)
}
\arguments{
\item{id}{the mindat localitiy id}

\item{..., }{Further named parameters.}
}
\value{
df, data frame of locality
}
\description{
retrieve locality by its id
}
\examples{
 mindat_localitiy(id = 222)
 mindat_localitiy(id = 222, fields = "id,name")
}
