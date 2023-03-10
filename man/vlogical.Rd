% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/5. miscellaneous.R
\name{vlogical}
\alias{vlogical}
\title{Vectorized Logical Tests}
\usage{
vlogical(vector, vpattern, test, simplify_with = NULL, ...)
}
\arguments{
\item{vector}{A vector or dimensional object to be processed (e.g., matrix, data.frame, etc.)}

\item{vpattern}{A vector of patterns to be matched}

\item{test}{(function) The function to use for logical testing: the function should be appropriate for the values of \code{vpattern} and have a named argument of \code{vector}, an additional argument to accept the pattern fed atomically, and '...' (even if not used).}

\item{simplify_with}{(function) When provided, the function operates over rows, thereby simplifying the result}

\item{...}{Additional arguments to be sent to the function held by argument \code{test}}
}
\value{
A logical matrix, with rows of the same length as \code{vector} and columns the length of \code{vpattern} TRUE for items that match pattern. If vector is dimensional, multiple \code{TRUE} values may be found for each row.
}
\description{
\code{vlike} is a vectorized version of \code{\link[data.table]{like}} allowing a pattern vector to be supplied
}
