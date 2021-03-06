% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{read FARS data from a vector containing the years}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{integer vector containing the years of FARS data to read}
}
\value{
a dataframe containing month and year of data
}
\description{
read FARS data from a vector containing the years
}
\details{
This function fails if any single year provided is missing from the data.
}
\examples{
fars_read_years(c(2012, 2013))
}
