% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/behavioural_traits.R
\name{split_behaviour}
\alias{split_behaviour}
\title{split_behaviour()}
\usage{
split_behaviour(data, time, ID, frz)
}
\arguments{
\item{data}{Zantiks transformed csv.}

\item{time}{Time in seconds, in which to divide data by.}
}
\value{
A dataframe containing summary behavioural variables
}
\description{
This function estimates the behaviour variables accounting
for differences in assay that can be divided by time. E.g.
light for the first X seconds, dark for next X seconds.
}
