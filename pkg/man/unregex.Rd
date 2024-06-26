% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscellaneous.R
\name{unregex}
\alias{unregex}
\title{Convert REGEX Pattern to Object Names}
\usage{
unregex(i, x)
}
\arguments{
\item{i}{(string[]) A string or string vector.  Pattern matching is executed if the string is of class regex" (see \code{\link{as.regex}}, \code{\link{is.regex}})}

\item{x}{(object) The names to search OR an object with column names}
}
\value{
Matching values in \code{x} based on values of \code{i}
}
\description{
Convert REGEX Pattern to Object Names
}
\seealso{
Other Miscellaneous Functions: 
\code{\link{as.regex}()},
\code{\link{call.recursion}()},
\code{\link{checksum}()},
\code{\link{gen.primes}()},
\code{\link{is.regex}()},
\code{\link{log_note}()},
\code{\link{polyname2orig}()},
\code{\link{vlogical}()}
}
\concept{Miscellaneous Functions}
