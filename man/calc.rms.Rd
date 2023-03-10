% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1. calculators.R
\name{calc.rms}
\alias{calc.rms}
\title{Calculate the Root-Mean-Square}
\usage{
calc.rms(a, post.op = eval)
}
\arguments{
\item{a}{(vector) A vector of numeric values}

\item{post.op}{See \code{\link{calc.means}}}
}
\description{
This is an alias that calls \code{\link{calc.means}(a, "rms")}
}
\seealso{
Other Central-tendency calculations: 
\code{\link{calc.geo_mean}()},
\code{\link{calc.harmonic_mean}()},
\code{\link{calc.means}()},
\code{\link{calc.zero_mean}()}
}
\concept{Central-tendency calculations}
