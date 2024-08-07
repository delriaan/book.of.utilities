calc.means <- function(data, mean.type = "am", post.op = eval, as.zscore = FALSE, use.population = FALSE, ...){
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
#' @param data (numeric): A numeric vector
#' @param mean.type (string): A character vector of the types of mean value operations to execute (see "Details")
#' @param post.op (call | \code{\link[base]{eval}}): A function that will process the output before returning
#' @param as.zscore (logical | \code{FALSE}) Should the output be transformed to Z-scores?
#' @param use.population (logical | \code{FALSE}) Should the population standard deviation be used (ignored when \code{as.zscore==FALSE})?
#' @param ... Additional arguments passed to other functions
#'
#' @importFrom magrittr %>% %<>%
#'
#' @return Arguments \code{data}, \code{mean.type}, and \code{post.op} determine the return type.
#'
#' @family Calculators
#'
#' @export

	check_z <- \(x, y){
			if (as.zscore & (y == "zm")){
		  	if (use.population){
		  		x/sqrt(mean(x^2, na.rm = TRUE))
		  	} else {
		  		x/sd(x, na.rm = TRUE)
		  	}
		  } else { x }
		};

  func.list <- list(
	    am = function(i, ...){
	    				i <- as.vector(i);

	    				mean(i, na.rm = TRUE, ...);
	    			}
			, zm = function(i, ...){
							i <- as.vector(i);

							(i - mean(i, na.rm = TRUE, ...));
						}
			, gm = function(i){
							i <- purrr::discard(as.vector(i), \(x) x == 0);

							purrr::modify_if(i, \(x) any(sign(x) == -1), as.complex) |>
								prod(na.rm = TRUE) %>%
								magrittr::raise_to_power(1/length(i))
						}
			, hm = function(i){
								i <- as.vector(i);

								i[!i == 0] %>%
									magrittr::raise_to_power(-1) |>
									mean(na.rm = TRUE) |>
									magrittr::raise_to_power(-1)
						}
			, rms = function(i, ...){
								i <- as.vector(i)

								mean(i^2, na.rm = TRUE, ...) |>
									purrr::modify_if(\(x) any(sign(x) == -1), \(x) as.complex(x, ...)) |>
									sqrt()
								}
  		);

  if (any(mean.type %in% c("*", "all"))){ mean.type <- names(func.list) }

	output <- if ("list" %in% class(data)){
			purrr::imap(func.list[mean.type], ~{
					f <- .x;
					nm <- .y
					purrr::map(data, ~f(.x) |> check_z(nm))
				})
		} else if (any(is.data.frame(data) || data.table::is.data.table(data) || is.matrix(data) || is.array(data))){
			purrr::imap(func.list[mean.type], \(x, y) apply(X = data, MARGIN = 2, FUN = x) |> check_z(y))
		} else {
			purrr::imap(func.list[mean.type], \(x, y) x(data) |> check_z(y))
		}

	post.op(if (rlang::has_length(output, 1)){ output[[1]] } else { output });
}
#
calc.zero_mean <- function(a, post.op = eval, as.zscore = FALSE, use.population = FALSE){
#' Calculate the Zero-Mean
#'
#' \code{calc.zero_mean} subtracts the mean from the input
#'
#' This is an alias that calls \code{\link{calc.means}(a, "hm")}
#'
#' @param a (vector) A vector of numeric values
#' @param post.op See \code{\link{calc.means}}
#' @param as.zscore (logical | \code{FALSE}) Should the output be transformed to Z-scores?
#' @param use.population (logical,numeric| \code{FALSE}) Should the population standard deviation be used (ignored when \code{as.zscore==FALSE}): defaults to a sampling distribution standard deviation. Providing a numeric value assumes \code{use.population=TRUE} and the value provided is used as the standard deviation.
#'
#' @family Calculators
#'
#' @export

	assertive::assert_any_are_true(c(is.numeric(use.population), is.logical(use.population)))

  .out <- calc.means(a, mean.type = "zm", post.op = post.op) |> unlist();

  if (as.zscore){
  	if (is.numeric(use.population)){ # Numeric branch
  		.sigma <- abs(use.population)
  	} else if (use.population){ # Logical branch
  		.sigma <- sd(a, na.rm = TRUE)
  	} else { # Default branch
  		.sigma <- sd(.out, na.rm = TRUE)/sqrt(length(.out))
  	}

  	return(.out/.sigma)
  } else {
  	return(.out)
  }
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
#' @family Calculators
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
#' @family Calculators
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
#' @family Calculators
#'
#' @export

  calc.means(a, mean.type = "gm", post.op = post.op) |> unlist()
}
#
ratio <- function(i, type = "of.sum", decimals = 2, as_density = FALSE, sort.type = c("none", "value", "label", "num_label")){
#' Ratio Calculator
#'
#' \code{ratio} calculates one of the following ratio types:\cr
#' \enumerate{
#' \item{\code{"of.sum"} (relative to the sum of \code{i})}
#' \item{\code{"of.max"} (relative to maximum value)}
#' \item{\code{"cumulative"} (cumulative total vs. total)}
#' }.\cr Using the related operator \code{\%ratio\%} assumes simple division by the total of \code{i}.
#'
#' @param i (vector) numeric vector
#' @param type (string[]) One or more types of ratio methods to use (see Details): a vector of supported values is supported
#' @param decimals (integer | 2) The number of decimal places to which the output should be rounded
#' @param as_density (logical) \code{TRUE} returns \code{x * p(1 - p)}, where \code{p} is a vector of cumulative proportions of \code{x}
#' @param sort.type (string,function) One of the following:\cr
#' \enumerate{
#'	\item{\code{"none"} to return along the original order (default)}
#'	\item{\code{"value"} to sort by value}
#'	\item{\code{"label"} to sort by name: all elements must be named}
#'	\item{\code{"num_label"} to sort by numeral names: all elements must be named}
#'	}\cr
#'	Sorting occurs \emph{before} the input is further processed.
#' @note Because this function produces values on a \emph{ratio} scale, all values are shifted such that all values are >= 0.
#'
#' @return A numeric vector
#'
#' @family Calculators
#'
#' @examples
#' # Argument "sort.type" = "none" (the default):
#' 
# ratio(print(x), type = pareto, as_density = FALSE) %>% {
#' 	plot(x = x, y = ., col = "blue", main = "cumulative")
#' }
#' ratio(x, type = pareto, as_density = TRUE) %>% {
#' 	plot(x = x, y = ., col = "red", main = "cumulative w/ density")
#' }
#' ratio(x, type = of.max, as_density = FALSE) %>% {
#' 	plot(x = x, y = ., col = "green", main = "of.max")
#' }
#' ratio(x, type = of.max, as_density = TRUE) %>% {
#' 	plot(x = x, y = ., col = "orange", main = "of.max w/density")
#' }
#'
#' # Argument "sort.type" (type = "of.sum", the default"):
#' (x2 <- sample(100, 5, TRUE) %>% rlang::set_names(. + 50))
#' list(
#' 	orig = x2
#' 	, none = ratio(x2, decimals = 8, sort.type = "none")
#' 	, value = ratio(x2, decimals = 8, sort.type = "value")
#' 	, label = ratio(x2, decimals = 8, sort.type = "label")
#' 	, num_label = ratio(x2, decimals = 8, sort.type = "num_label")
#' 	)
#' 
#' @export

	# 'type':
	type <- rlang::enexpr(type)

	if (is.call(type)){ type <- rlang::call_args(type) }
	type <- as.character(type)

	# ... legacy code ...
	if (type == "pareto"){ type <- "cumulative" }

	# 'sort.type':
	sort.type <- match.arg(sort.type)

	.i <- i
	
	# Set the origin of the vector to a minimum of zero because ratio levels of measurement require an absolute zero point. 
	if (any(.i < 0)){
		.i <- .i + abs(min(.i))
	}

	# Sort `.i` if type is "cumulative":
	if (type == "cumulative"){
		.i %<>% sort()
	}

	# Capture the names of the original vector `i`:
	names.i <- names(i) %||% paste("x", seq_along(i), sep = "_")

	sort.actions <- rlang::exprs(
		none = seq_along(i)
		, value = order(i)
		, label = order(names.i)
		, num_label = order(as.numeric(names.i))
		)

	if (!any(names.i == "")){
		if (sort.type == "num_label"){ 
			if (!all(is.numeric(as.numeric(names.i)))){
				sort.type <- "label"
			}
		}
	}
	
	ord.i <- eval(sort.actions[[sort.type]])

	# Functions selected by 'type'
	of.max <- \(.i) .i/max(.i, na.rm = TRUE);

	of.sum <- \(.i) .i/sum(.i, na.rm = TRUE);

	cumulative <- \(.i) cumsum(.i)/sum(.i, na.rm = TRUE);

	likelihood <- \(.i){
		res <- sapply(.i, \(k) mean(.i <= k, na.rm = TRUE)) %>% magrittr::multiply_by(1 - .)

		# Prevent the result from being zero:
		if (any(res == 0)){
			res[res == 0] <- mad(res[res == 0]) * 0.0001
		}

		# Return the result:
		return(res)
	}

	.out <- if (rlang::has_length(type, 1)){
			do.call(type, args = list(.i = .i))
		} else {
			purrr::set_names(type) |> purrr::map(\(k) do.call(k, args = list(.i = .i)))
		}

 if (grepl("cum", type)){
 	.prop <- cumulative(.out)
	.dens <- 1

	 if (as_density){
		.out <- .out * likelihood(.prop)
	 } else {
		 .out <- .out * .prop
	 }
 }

	# Return the result with the ordering specified by "sort.type":
 return(rlang::set_names(.out[ord.i], names.i[ord.i]))
}

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
  	return(data.table::data.table(ranks = ranks, thresholds = thresholds.list))
  } else {
    output <- map.fn(scores, ~ ranks[ test(.x, thresholds.list) |> which() |> max(na.rm = TRUE) ]) |>
    	unlist() |>
    	scrub.data(replacement = dflt.fn(ranks))

		if (show.all) {
			data.table::data.table(
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
radix <- function(x, ...){
#' Radix Conversion
#'
#' \code{radix} converts the \emph{representation} of the input(s) using a radix basis.  This is not to be confused with a conversion between \emph{scales}.
#'
#' @param x (numeric[],symbol[]) One or more whole numbers: coercion for non-numeric and fractional input will result in loss of precision. For hexadecimal values, be sure to use the \bold{\code{"0x"}} prefix.
#' @param ... (string,symbol,integer) The target radix to which the values in \code{x} will be converted.\cr
#' \itemize{
#'   \item{Length: If two values are provided, the output is the conversion from the first radix to the second}
#'   \item{String/symbol: If given as a strings or symbols, the following are supported (partial matching supported):\cr
#'   \itemize{
#'    \item{\code{binary}}
#'    \item{\code{octal}}
#'    \item{\code{decimal}}
#'    \item{\code{duodecimal}}
#'    \item{\code{hexadecimal}}
#'    \item{\code{vigesimal}}
#'    \item{\code{sexagesimal}}
#'    }
#'   }
#' }
#'
#' @examples
#' # As binary
#' radix(12, bin)
#' radix(c(12, 20), bin)
#'
#' # To hexadecimal
#' radix(c(12, 20), hex)
#'
#' # As hex to decimal
#' radix(c(18, 32), hex, dec)
#'
#' # Hex to binary vs. as hex to binary (equivalent outcomes)
#' radix(c(18, 32), hex, bin)
#' radix(!!radix(c(12, 20), hex), hex, bin)
#'
#' # Arbitrary radix
#' radix(c(10, 26), 3)
#'
#' # Arbitrary radix to hex
#' radix(c(10, 26), 3, h)
#'
#' # Mixed input types to binary
#' radix(c(100, "0110"), b)
#' radix(c(100, "1001", 0x1D), b)
#'
#' @return The \emph{decimal} representation of the input(s) using the supplied radix.
#'
#' @family Calculators
#'
#' @export
	r <- rlang::enexprs(...);
	if (rlang::is_empty(r)){ stop("No radix bases provided.") }

	if (missing(x)){
		stop("No values for 'x' provided.")
	} else {
		x <- rlang::enexpr(x) |> as.list();

		if (length(x) == 1){
			x <- as.character(x)
		} else {
			# A call was passed (e.g., x = c(...)).
			# Remove the first element as this is the call symbol (the rest are arguments):
			x <- sapply(x[-1], as.character);
		}

		x <- as.numeric(x) |> round();
	}

	# :: `.named` is a look-up list mapping names to bases:
	.named <- { rlang::set_names(
			c(2, 8, 10, 12, 16, 20, 60)
			, "binary",  "octal",  "decimal",  "duodecimal"
			,  "hexadecimal",  "vigesimal",  "sexagesimal"
			) |> as.list();
	}

	# :: Vector of values:
	if (!rlang::has_length(r, 1)){
		k <- purrr::map_int(r, \(i){
			ifelse(
				typeof(i) %in% c("character", "symbol", "call")
				, .named[[which(grepl(paste0("^", as.character(i)), names(.named)))]]
				, as.integer(i)
				)
		});

		# Recursive call to the function:
		.mod <- x %% k[[1]];

		radix(x = !!(10 * (x - .mod)/k[[1]] + .mod), !!r[[2]]);
	} else {
	# :: Single value:
		r <- ifelse(
				typeof(r[[1]]) %in% c("character", "symbol", "call")
				, .named[[which(grepl(paste0("^", as.character(r[[1]])), names(.named)))]]
				, as.integer(r[[1]])
				)

		stringi::stri_extract_all_regex(x, "\\d") |>
			purrr::map(\(i){
				n <- seq(stringi::stri_length(i)) |> rev()
				sum(as.integer(i) * r^(n-1))
			}) |> unlist(use.names = FALSE)
	}
}
#
range_diff <- function(...){
#' Magnitude of a Range of Values
#'
#' \code{range_diff} calculates the difference in the range of values given by \code{input}
#'
#' @param ... (\code{\link[rlang]{dots_list}}): A numeric vector for which the range and corresponding difference is calculated. If a list is provided, the calculation is executed over each element of the list.
#'
#' @return A scalar value of the inclusive difference of upper and lower boundaries of the range of the input vector
#'
#' @family Calculators
#'
#' @export

	action = function(i){ i = as.numeric(unlist(i)); max(i, na.rm = TRUE) - min(i, na.rm = TRUE); }

	i = rlang::list2(...);

	if (any(purrr::map_lgl(i, \(x) length(x) > 1))){ purrr::map(i, action) } else { action(i = i) }
}
#
odds2probs <- function(...){
	#' Calculate percentages from odds ratios
	#'
	#' \code{odds2probs} converts odds ratios to percentages.
	#'
	#' @param ... (\code{\link[rlang]{dots_list}}): Numeric vectors or "odds" strings (e.g., "x:y"). For vectors or string representations, the number of operands can be of any length but must represent positive real values.
	#'
	#' @note Length-1 arguments are assumed to be odds ratios and are converted to percentages via sigmoid function.
	#'
	#' @return A numeric list length as the input(s) representing the percentage of the odds ratio(s): values that cannot be converted will return \code{NULL}.
	#'
	#' @family Calculators
	#'
	#' @examples
	#' odds2probs(c(3, 4), c(3L, 4L), "3:4", c("3", "4"), c(4,5,6), "6:3:1", c(3,-1), "1:1", 1, 2/5, "2:3", 2)
	#'
	#' @export
  fun <- (\(i, j){
    res <- if (i$type == "character" && i$len == 1) {
      if (grepl(":", arg_list[[j]])) {
        strsplit(arg_list[[j]], ":") |> unlist() |> as.numeric()
      } else {
        as.numeric(arg_list[[j]])
      }
    } else if (i$type == "character" && i$len > 1) {
      arg_list[[j]] |> unlist() |> as.numeric()
    } else if (i$type %in% c("integer", "numeric", "double")) {
      if (i$len > 1){
        arg_list[[j]]
      } else {
        as.numeric(arg_list[[j]])
      }
    } else {
      numeric()
    }
    # browser()
    if (rlang::is_empty(res) || (any(res <= 0) & !all(res <= 0))){
      NULL
    } else if (length(unique(res)) == 1 || all(res <= 0)){
      res <- unique(res)
      # warning("Assuming input as odds ratio ...")
      exp(res)/(1 + exp(res))
    } else{
      res/sum(res)
    }
  });
  arg_list <- rlang::list2(...);
  arg_types <- purrr::map(arg_list, \(i) data.frame(type = typeof(i), len = length(i)));
  arg_types |> purrr::imap(purrr::possibly(fun, otherwise = NULL));
}
