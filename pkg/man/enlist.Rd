% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/object_management.R
\name{enlist}
\alias{enlist}
\title{Create a Named List}
\usage{
enlist(x, ...)
}
\arguments{
\item{x}{A vector of values}

\item{...}{(\code{\link[rlang]{dots_list}}):: Names to use for the list members: must be the same length as \code{x}.  Can be a vector or an atomic listing.}
}
\value{
A list, the names of which being the values of \code{x}
}
\description{
\code{enlist} creates a list from an atomic vector.  When names having the length of the input are provided to \code{...}, a list with members named accordingly is produced.  Otherwise, the members are named by their values (eg. \code{list(this="this", that="that")} ): this is the default behavior.
}
\examples{
# :: Test Objects
test_x = list(
	test_1 = c(1:5)
	, test_2 = list(c(1:5), 3, 4, c(letters[1:5]))
);
nms = c("up", "down", "left", "right", "center");

# :: Unnamed Vector -> Elements become names
enlist(test_x$test_1);
enlist(test_x$test_2);

# :: Unnamed Vector & Provided Names (Full) -> All elements have names in 'nms'
enlist(test_x$test_1, nms);
enlist(test_x$test_2, !!!nms);
enlist(test_x$test_2, up, down, left, right, center);

# \%>\% Unnamed Vector & Provided Names (Partial) -> First two names are 'nms[1:2]' with the balance as default
enlist(test_x$test_1, !!!nms[1:2]);
enlist(test_x$test_2, !!!nms[1:2])

}
\seealso{
Other Object Management: 
\code{\link{distinct.list}()},
\code{\link{get.object_sizes}()},
\code{\link{scrub.data}()}
}
\concept{Object Management}
