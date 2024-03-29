% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/object_management.R
\name{distinct.list}
\alias{distinct.list}
\title{Unique List Selection}
\usage{
distinct.list(i, logical.only = FALSE)
}
\arguments{
\item{i}{(object) The input list object (or coercible)}

\item{logical.only}{(logical) When \code{TRUE}, a logical vector is returned}
}
\value{
The list with unique, first-level child elements
}
\description{
Return the distinct set of first-level list elements based on their respective base64-encoded signatures.  This is an inefficient implementation used for nested lists
}
\seealso{
Other Object Management: 
\code{\link{enlist}()},
\code{\link{get.object_sizes}()},
\code{\link{scrub.data}()}
}
\concept{List management}
\concept{Object Management}
