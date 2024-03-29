% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mindat_localities.R
\name{localities_list_updated_at}
\alias{localities_list_updated_at}
\title{retrieve the localities list updated at the given time.}
\usage{
localities_list_updated_at(updateDate,...)
}
\arguments{
\item{updateDate}{string\if{html}{\out{<date-time>}}, Last updated datetime in format \%Y-\%m-\%d \%H:\%M:\%S}

\item{..., }{Further named parameters.Other optional arguments.}
}
\value{
df, a data frame of localities
}
\description{
: Queries the list of localities that have the given time
}
\details{
This function releated to all the fields "updated_at" of localities.
retrieve the localities that have the latest updated at the given time.
}
\examples{
\dontrun{
df <-localities_list_updated_at("lava")
}
}
