% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lamtest.R
\name{lamtest}
\alias{lamtest}
\title{Lam test for consistency of CFR}
\usage{
(data,T)
}
\arguments{
\item{data}{summarized survival data}
}
\value{
A list containing the following components:
\item{Z}{Z statistic}
\item{P}{P value}
}
\description{
Lam test for consistency of CFR
}
\examples{
data(individual)
data1<-sumdata(individual)
lamtest(data1,60)
}
