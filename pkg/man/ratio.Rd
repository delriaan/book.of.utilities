% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculators.R
\name{ratio}
\alias{ratio}
\title{Ratio Calculator}
\usage{
ratio(
  i,
  type = "of.sum",
  as_density = FALSE,
  sort.type = c("none", "value", "label", "num_label"),
  ...
)
}
\arguments{
\item{i}{(vector) numeric vector}

\item{type}{(string[]) One or more types of ratio methods to use (see Details): a vector of supported values is supported}

\item{as_density}{(logical) \code{TRUE} returns \code{x * p(1 - p)}, where \code{p} is a vector of cumulative proportions of \code{x}}

\item{sort.type}{(string,function) One of the following:\cr
\enumerate{
\item{\code{"none"} to return along the original order (default)}
\item{\code{"value"} to sort by value}
\item{\code{"label"} to sort by name: all elements must be named}
\item{\code{"num_label"} to sort by numeral names: all elements must be named}
}\cr}

\item{...}{\code{\link[rlang]{dots_list}}: provided for backwards-compatibility as the function signature changes.
Sorting occurs \emph{before} the input is further processed.}
}
\value{
A numeric vector
}
\description{
\code{ratio} calculates one of the following ratio types:\cr
\enumerate{
\item{\code{"of.sum"} (relative to the sum of \code{i})}
\item{\code{"of.max"} (relative to maximum value)}
\item{\code{"cumulative"} (cumulative total vs. total)}
}.\cr Using the related operator \code{\%ratio\%} assumes simple division by the total of \code{i}.
}
\note{
Because this function produces values on a \emph{ratio} scale, all values are internally shifted such that all values are >= 0.
}
\examples{
# Argument "sort.type" = "none" (the default):
y <- ratio(print(x), type = pareto, as_density = FALSE)
plot(x = x, y = y, col = "blue", main = "cumulative")
y <- ratio(x, type = pareto, as_density = TRUE) 
plot(x = x, y = y, col = "red", main = "cumulative w/ density")
y <- ratio(x, type = of.max, as_density = FALSE) 
plot(x = x, y = y, col = "green", main = "of.max")
y <- ratio(x, type = of.max, as_density = TRUE)
plot(x = x, y = y, col = "orange", main = "of.max w/density")
# Argument "sort.type" (type = "of.sum", the default"):
(\(x2) list(
	orig = x2
	, none = ratio(x2, decimals = 8, sort.type = "none")
	, value = ratio(x2, decimals = 8, sort.type = "value")
	, label = ratio(x2, decimals = 8, sort.type = "label")
	, num_label = ratio(x2, decimals = 8, sort.type = "num_label")
	))(sample(100, 5, TRUE) |> (\(m) rlang::set_names(m, m + 50))())

}
\seealso{
Other Calculators: 
\code{\link{calc.geo_mean}()},
\code{\link{calc.harmonic_mean}()},
\code{\link{calc.means}()},
\code{\link{calc.rms}()},
\code{\link{calc.zero_mean}()},
\code{\link{odds2probs}()},
\code{\link{radix}()},
\code{\link{range_diff}()},
\code{\link{ranking.algorithm}()}
}
\concept{Calculators}
