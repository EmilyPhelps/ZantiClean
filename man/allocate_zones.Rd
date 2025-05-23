% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/allocate_zones.R
\name{allocate_zones}
\alias{allocate_zones}
\title{allocate_zones}
\usage{
allocate_zones(XY, zones, arena, start, end, file, nested)
}
\arguments{
\item{XY}{Transformed coord data}

\item{zones}{A dataframe with the number of zones and the coordinates of the zones.
e.g. zone, xmin, xmax, ymin, ymax
Zone name must have a Z in it. e.g. Z1, Z2, Z3 or Z_inner, Z_outer
Also the zones cannot overlap. Make them 0.1 apart at least.}

\item{arena}{A dataframe with the coordinates of the arenas.
e.g. arena, xmin, xmax, ymin, ymax}

\item{start}{The start time of the assay, after acclimatization (in seconds)}

\item{end}{The end time of the assay (in seconds)}

\item{file}{The name of the file to be included. If not included output will
have NA in this column.}

\item{nested}{Logical vector, is Zone1 within Zone2. Only applicable for two Zones currently.}
}
\value{
An object containing a data frame reflective of the transformed csv
}
\description{
This function is used to change the zone allocation and calculate the distance, time and counts data for the new zones
}
