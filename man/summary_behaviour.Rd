% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/behavioural_traits.R
\name{summary_behaviour}
\alias{summary_behaviour}
\title{summary_behaviour()}
\usage{
summary_behaviour(data, ID, frz)
}
\arguments{
\item{data}{Zantiks transformed csv}

\item{ID}{a logical vector. If true, an ID will be assigned from the Service part of the file.}
}
\value{
A dataframe containing summary behavioural variables
}
\description{
This function estimates the summary behaviour data from the
zantiks csv. The variables included are time in each zone,
overall velocity, track length, freezings (calculated as no
movement for three seconds)
}
