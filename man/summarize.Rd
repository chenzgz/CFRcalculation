% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize.R
\docType{data}
\name{summarize}
\alias{summarize}
\title{Data from simulation}
\format{
A data frame with 151 observations on the following 8 variables.
\describe{
\item{id}{patient id number}
\item{date}{the cout time}
\item{n}{number of new admissions}
\item{d}{number of new deaths}
\item{c}{number of new discharged patients}
\item{N}{cumulative number of cases}
\item{D}{cumulative number of deaths}
\item{C}{cumulative number of discharged patients}
}
}
\usage{
data(summarize)
}
\description{
Summarized survival data transformed by individual survival data from simulation.
}
\examples{
library(CFRcalculation)
data(summarize)
}
