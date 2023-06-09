% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mindat_localities.R
\name{localities_status_retrieve}
\alias{localities_status_retrieve}
\title{localities_status_retrieve}
\usage{
localities_status_retrieve (...)
}
\arguments{
\item{id}{the mindat localitiy status id}

\item{..., }{Further named parameters.}
}
\value{
df, data frame of locality status.
}
\description{
retrieve locality status by its id.
}
\examples{
 localities_status_retrieve(2)
 localities_status_retrieve(id = 2)
}
