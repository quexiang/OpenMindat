% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mindat_geomaterials.R
\name{geomaterials_contain_only_elems}
\alias{geomaterials_contain_only_elems}
\title{geomaterials_contain_only_elems}
\usage{
geomaterials_contain_only_elems (icl_only_elms_vector)
}
\arguments{
\item{icl_only_elms_vector, }{vector of elements.}

\item{..., }{Further named parameters.Other optional arguments-Additional arguments that can be
passed to the mindat_geomaterial_list function.}
}
\value{
df, a data frame of geomaterials
}
\description{
retrieve the geomaterials that contain only the elements users input.This function queries
the list of geological materials that only contain the specified elements.
It performs the query operation by calling the mindat_geomaterial_list function
}
\examples{
geomaterials_contain_only_elems(c('Si','O'))
}
