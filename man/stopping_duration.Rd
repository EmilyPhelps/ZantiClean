% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/behavioural_traits.R
\name{stopping_duration}
\alias{stopping_duration}
\title{stopping_duration()}
\usage{
stopping_duration(data, ID)
}
\arguments{
\item{data}{Zantiks transformed csv}

\item{ID}{(optional) A logical vector. If true, the ID will be carried forward.}
}
\value{
A dataframe containing duration of each stopping event, e.g. when total distance is 0.
}
\description{
This function estimates the time stopped from the Zantiks csv files.
}
