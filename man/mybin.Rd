% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mybin.R
\name{mybin}
\alias{mybin}
\title{My Binomial Function}
\usage{
mybin(iter = 100, n = 10, p = 0.5)
}
\arguments{
\item{iter}{Number of iterations}

\item{n}{Number of trials in each iteration}

\item{p}{Probability of individual success per trial (between 0 and 1)}
}
\value{
A distribution plot of the frequency of success and a printed table containing the density values
}
\description{
My Binomial Function
}
\examples{
mybin(iter=100,n=10, p=0.7)
mybin(iter=10000,n=15, p=0.3)
}
