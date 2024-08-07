% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculators.R
\name{calc.zero_mean}
\alias{calc.zero_mean}
\title{Calculate the Zero-Mean}
\usage{
calc.zero_mean(a, post.op = eval, as.zscore = FALSE, use.population = FALSE)
}
\arguments{
\item{a}{(vector) A vector of numeric values}

\item{post.op}{See \code{\link{calc.means}}}

\item{as.zscore}{(logical | \code{FALSE}) Should the output be transformed to Z-scores?}

\item{use.population}{(logical,numeric| \code{FALSE}) Should the population standard deviation be used (ignored when \code{as.zscore==FALSE}): defaults to a sampling distribution standard deviation. Providing a numeric value assumes \code{use.population=TRUE} and the value provided is used as the standard deviation.}
}
\description{
\code{calc.zero_mean} subtracts the mean from the input
}
\details{
This is an alias that calls \code{\link{calc.means}(a, "hm")}
}
\seealso{
Other Calculators: 
\code{\link{calc.geo_mean}()},
\code{\link{calc.harmonic_mean}()},
\code{\link{calc.means}()},
\code{\link{calc.rms}()},
\code{\link{odds2probs}()},
\code{\link{radix}()},
\code{\link{range_diff}()},
\code{\link{ranking.algorithm}()},
\code{\link{ratio}()}
}
\concept{Calculators}
