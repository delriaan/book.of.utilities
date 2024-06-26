% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/miscellaneous.R
\name{checksum}
\alias{checksum}
\title{Checksum Validation}
\usage{
checksum(object, hash, ...)
}
\arguments{
\item{object, ...}{See \code{\link[digest]{digest}}}

\item{hash}{(string) The hash to compare}
}
\value{
A logical scalar
}
\description{
\code{checksum} provides a wrapper to \code{\link[digest]{digest}} providing the hash to use for comparison
}
\seealso{
Other Miscellaneous Functions: 
\code{\link{as.regex}()},
\code{\link{call.recursion}()},
\code{\link{gen.primes}()},
\code{\link{is.regex}()},
\code{\link{log_note}()},
\code{\link{polyname2orig}()},
\code{\link{unregex}()},
\code{\link{vlogical}()}
}
\concept{Miscellaneous Functions}
