range_diff <- function(...){
#' Magnitude of a Range of Values
#'
#' \code{calc.range_diff} calculates the difference in the range of values given by \code{input}
#'
#' @param ... (\code{\link[rlang]{dots_list}}): A numeric vector for which the range and corresponding difference is calculated. If a list is provided, the calculation is executed over each element of the list.
#' @return A scalar value of the inclusive difference of upper and lower boundaries of the range of the input vector
#' @family Calculators
#'
#' @export

	action = function(i){ i = as.numeric(unlist(i)); max(i, na.rm = TRUE) - min(i, na.rm = TRUE); }

	i = rlang::list2(...);

	if (any(purrr::map_lgl(i, ~length(.x) > 1))){ purrr::map(i, action) } else { action(i = i) }
}
#' @family Deprecated
#' @export
calc.range_diff <- range_diff;
#
calc.means <- function(a, mean.type = "am", post.op = eval) {
#' Calculate Means
#'
#' \code{calc.means} calculates various types of central-tendency measures for the vector passed to argument \code{a}
#'
#' If `mean.type` contains multiple values, a list object is returned containing one member for each output; otherwise, a single value is returned with a class dependent on the value of `mean.type`.
#' Valid `mean.type` values are as follows:
#' \enumerate{
#' \item "am" (Arithmetic-Mean \url{https://en.wikipedia.org/wiki/Arithmetic_mean}): Return the sum of vector values divided by the magnitude of the vector
#' \item "zm" (Zero-Mean): Return the vector with the mean of the vector subtracted from all values
#' \item "gm" (Geometric Mean \url{https://en.wikipedia.org/wiki/Geometric_mean}): Return the \emph{n-th} root of the product of an \emph{n}-element input vector.  Negative input values are allowed, resulting in a complex result
#' \item "hm" (Harmonic Mean \url{https://en.wikipedia.org/wiki/Harmonic_mean}): Return the reciprocal of the arithmetic mean of the reciprocal of the elements of the input vector
#' \item "rms"(Root Mean Squared \url{https://en.wikipedia.org/wiki/Root_mean_square}): Return the square-root of the arithmetic mean of the squares of the elements of the input vector
#' }
#'
#' @param a (numeric): A numeric vector
#' @param mean.type (string): A character vector of the types of mean value operations to execute (see "Details")
#' @param post.op (call | \code{\link[base]{eval}}): A function that will process the output before returning
#'
#' @return Arguments \code{a}, \code{mean.type}, and \code{post.op} determine the return type.
#'
#' @family Central-tendency calculations
#'
#' @export

	a = as.vector(a);

  func.list <- { list(
      am	= function(i){ if (length(unique(i)) == 1) { 0 } else { mean(i, na.rm = TRUE) }}
		, zm	= function(i){ if (length(unique(i)) == 1) { 0 } else { i - mean(i, na.rm = TRUE) }}
		, gm	= function(i){
				i <- i[!i==0]
				if (any(sign(i) == -1)) { i <- as.complex(i) }
				prod(i, na.rm = TRUE)^(length(i)^-1)
			}
		, hm	= function(i){ i <- i[!i==0]; length(i) / sum(i^-1, na.rm = TRUE) }
		, rms = function(i){ ifelse(rlang::has_length(unique(i), 1), i, sapply(i, `^`, 2) %>% mean(na.rm = TRUE) |> sqrt()) }
    )}

  if ("*" %in% mean.type) { mean.type <- names(func.list) }

	output = if ("list" %in% class(a)){
		purrr::map(func.list[mean.type], ~{ fn = .x; purrr::map(a, fn) })
		} else if (any(c("data.frame", "data.table", "matrix") %in% class(a))){
			purrr::map(func.list[mean.type], ~{ fn = .x; apply(X = a, FUN = fn, ...) })
		} else { purrr::map(func.list[mean.type], ~.x(a) ) }

	post.op(if (rlang::has_length(output, 1)){ output[[1]] } else { output })
}
#
calc.zero_mean <- function(a, post.op = eval) {
#' Calculate the Zero-Mean
#'
#' \code{calc.zero_mean} subtracts the mean from the input
#'
#' This is an alias that calls \code{\link{calc.means}(a, "hm")}
#'
#' @param a (vector) A vector of numeric values
#' @param post.op See \code{\link{calc.means}}
#'
#' @family Central-tendency calculations
#'
#' @export

  calc.means(a, mean.type = "zm", post.op = post.op) |> unlist()
}
#
calc.rms <- function(a, post.op = eval) {
#' Calculate the Root-Mean-Square
#'
#' This is an alias that calls \code{\link{calc.means}(a, "rms")}
#'
#' @param a (vector) A vector of numeric values
#' @param post.op See \code{\link{calc.means}}
#'
#' @family Central-tendency calculations
#'
#' @export

  calc.means(a, mean.type = "rms", post.op = post.op) |> unlist()
}
#
calc.harmonic_mean <- function(a, post.op = eval) {
#' Calculate the Harmonic Mean
#'
#' This is an alias that calls \code{\link{calc.means}(a, "hm")}
#'
#' @param a (vector) A vector of numeric values
#' @param post.op See \code{\link{calc.means}}
#'
#' @family Central-tendency calculations
#'
#' @export

  calc.means(a, mean.type = "hm", post.op = post.op) |> unlist()
}
#
calc.geo_mean <- function(a, post.op = eval) {
#' Calculate the Geometric Mean
#'
#' This is an alias that calls \code{\link{calc.means}(a, "gm")}
#'
#' @param a (vector) A vector of numeric values
#' @param post.op See \code{\link{calc.means}}
#'
#' @family Central-tendency calculations
#'
#' @export

  calc.means(a, mean.type = "gm", post.op = post.op) |> unlist()
}
#
ratio <- function(i, j = i, type = NULL, decimals = 2){
#' Ratio Calculator
#'
#' \code{ratio} calculates one of the following ratio types:\cr
#' \enumerate{
#' \item{\code{"pareto"} (cumulative total vs. total)}
#' \item{\code{"of.max|of.min"} (relative to maximum/minimum value)}
#' \item{\code{NULL} (relative to total)}
#' }.\cr Using the related operator \code{\%ratio\%} assumes simple division by the total of \code{j}.
#'
#' @param i (vector) The vector of numeric scores in the numerator
#' @param j (vector) The number of numeric scores in the denominator
#' @param type (string[]) The types of ratio algorithms to use (see Details): a vector of values is supported
#' @param decimals (integer | 2) The number of decimal places to which the output should be rounded
#'
#' @family Calculators
#'
#' @export

	action = function(ii, jj){
		sub.fn = function(k, .i, .j, .d){
			if (!is.list(.j)){
				switch(
					k
					, "pareto" = cumsum(.i)/sum(.j, na.rm = TRUE)
					, "of.max" = .i/max(.j, na.rm = TRUE)
					, "of.min" = .i/min(.j, na.rm = TRUE)
					, .i/sum(.j)
					) |> round(.d)
			} else { purrr::map(.j, ~sub.fn(k = k, .i = .i, .j = .x)) }
		}
		if (!rlang::has_length(type, 1)){
			purrr::set_names(type) %>% purrr::map(sub.fn, ii, jj, decimals)
		} else {
			sub.fn(type, ii, jj, decimals)
		}
	}
	if (any(purrr::map_lgl(i, ~length(.x) > 1))){
		purrr::map2(i, j, action)
	} else {
		action(i, j)
	}
}
	#' @export
	`%ratio%` <- ratio;
	#' @export
	`%pareto%` <- {
			assign("x", ratio);
			formals(x) <- alist(i = , j = , type = "pareto");
			x
		};
#
ranking.algorithm <- function(
	scores, rank.size, poss.scores, test = `>=`, rescale = 1
	, map.only = FALSE, show.all = FALSE, use.futures = FALSE, dflt.fn = min
	, map.fn = (purrr::map)
	){
#' Ranking Algorithm
#'
#' \code{ranking.algorithm} ranks values across a vector of values
#'
#' @param scores (vector) The vector of numeric scores to rank
#' @param rank.size (integer) The number of ranks to allow
#' @param poss.scores (vector) The distinct values demarcating each rank threshold
#' @param test (object) A function or operator that takes in as input the scores, performs a conditional test, and returns a logical vector of equal length
#' @param rescale (numeric) A multiplier that scales the scores vector up or down
#' @param map.only (logical|FALSE) When \code{TRUE}, a \href{https://www.rdocumentation.org/packages/data.table/versions/1.12.8}{data.table} object of the rank and rank thresholds is returned
#' @param show.all (logical|FALSE) When \code{TRUE}, a \href{https://www.rdocumentation.org/packages/data.table/versions/1.12.8}{data.table} object of the scores, ranks thresholds, and ranks are returned
#' @param use.futures (logical|FALSE) When \code{TRUE}, the operation is parallelized using \href{https://www.rdocumentation.org/packages/future/versions/1.17.0}{future}s
#' @param dflt.fn (function) The default function to use when replacing \code{NA} values in ranks
#' @param map.fn (function) The function to iterate the scores when computing the output
#'
#' @family Calculators
#'
#' @export

  if (sign(rescale) < 1) {
    message("`rescale` is not positive or > 0: setting to 1");
    rescale == 1;
  }

  # :: Ensure atomic vectors are used
  scores		= rescale * unlist(scores);
  ranks 		= 1:rank.size - 1;
  increment = range_diff(poss.scores) / length(ranks);

  # ::  Create the vector of thresholds using the value of `rescale` to ensure correct results
  thresholds.list <- min(poss.scores, na.rm = TRUE) + (ranks * increment);

  # :: Put the ranks output on a 1-based scale
  ranks <- ranks + 1;

  if (map.only) {
  	return(data.table(ranks = ranks, thresholds = thresholds.list))
  } else {
    output <- map.fn(scores, ~ ranks[ test(.x, thresholds.list) |> which() |> max(na.rm = TRUE) ]) |>
    	unlist() |>
    	scrub.data(replacement = dflt.fn(ranks))

		if (show.all) {
			data.table(
				scores = scores / rescale
				, thresholds = list(c(thresholds.list / rescale))
				, rank = output
				, key = c("scores")
				)
			}
		else { output }
  }
}
#
