% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map.R
\name{leafletTiles}
\alias{leafletTiles}
\title{Extract leaflet tile paths from a \code{map} obj}
\usage{
leafletTiles(map)
}
\arguments{
\item{map}{A \code{map} class obj}
}
\value{
A vector of paths indicating the relative paths. Any layers
that don't have leaflet tiles will return NA.
}
\description{
Extract leaflet tile paths from a \code{map} obj
}
