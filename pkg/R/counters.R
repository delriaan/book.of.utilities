count_cycles <- function(cond, offset = 1, reset){
	#' Cycle Counter
	#'
	#' \code{count.cycles} detects and maps conditional occurrence frequency
	#'
	#' @param cond (logical[]) A logical vector
	#' @param offset (integer|1) An integer defining the series origin
	#' @param reset (logical[]) A logical vector of the same length as \code{cond} such that when \code{TRUE}, the cycle counter resets to the offset. Un-evaluated expressions are also accepted and should refer to \code{cond} in the definition.
	#'
	#' @return A one-based (default), conditionally-incremented series
	#'
	#' @family Counters
	#' @aliases count.cycles
	#'
	#' @examples
	#' sample(c(TRUE, FALSE), 50, TRUE) |>
	#' print() %>%
	#' count.cycles(reset = cumsum(.) %% 5 == 0)
	#'
	#' @export

	reset <- rlang::enexpr(reset);

	offset <- as.integer(offset)[1];

	cycle_idx <- offset;

	count_idx <- iterators::iter(1:length(cond));

	f_env <- environment();

	# :: Define the counter function
	counter = \(tf) {
		idx = iterators::nextElem(count_idx);

		if (tf) {
	  	# cycle_idx only resets to the offset value when the limiter function returns 'TRUE'; otherwise, it increments
			cycle_idx <<- cycle_idx + 1
	  }

	  # cycle_idx only resets to the offset value when the limiter function returns 'TRUE'; otherwise, it increments
		cycle_idx <<- ifelse((idx %in% which(eval(reset, envir = f_env))) | (idx == 1), offset, cycle_idx)

		# return the current value of 'cycle_idx'
		cycle_idx
	}

  # :: Call the counter function
  sapply(cond, counter);
}

factor_int <- function(i, ...){
	#' Factorization of Integers
	#'
	#' @param i (integer[]) One or more integers to factor
	#' @param ... \code{\link[rlang]{dots_list}}: Additional integers to factor (can be used in conjunction with \code{i})
	#'
	#' @return The factors of the input given as a vector or list of vectors
	#'
	#' @family Counters
	#' @aliases factor.int
	#'
	#' @examples
	#' factor.int(100, 90)
	#'
	#' @export

	if (missing(i) & ...length() == 0){ message("[ERROR] Please supply integer values to either 'i' or '...'"); return(0); }
	if (missing(i) & ...length() != 0){ i <- 0 }

	i <- c(i, rlang::list2(...)) |> unlist() |> purrr::keep(is.numeric) |> round() |> as.integer();

	i <- purrr::set_names(i);

	.out <- purrr::map(i, \(x) which(x %% sequence(x) == 0))

	if (rlang::has_length(.out, 1)){ .out[[1]] } else { .out }
}

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
		x <- rlang::enexpr(x) |> as.list()

		if (length(x) == 1){
			x <- as.character(x)
		} else {
			# A call was passed (e.g., x = c(...)).
			# Remove the first element as this is the call symbol (the rest are arguments):
			x <- sapply(x[-1], as.character)
		}

		.hex <- glue::glue("[{paste0(letters, LETTERS)[1:6]}]")
		x <- stringi::stri_replace_all_regex(x, .hex, 1:6, vectorize_all = FALSE) |> 
			as.numeric() |> 
			round()
	}

	# [.named] : A lookup list mapping names to bases:
	.named <- { 
		rlang::set_names(
			c(2, 8, 10, 12, 16, 20, 60)
			, "binary",  "octal",  "decimal",  "duodecimal"
			,  "hexadecimal",  "vigesimal",  "sexagesimal"
			) |> 
			as.list()
		}

	# :: Vector of values:
	if (!rlang::has_length(r, 1)){
		k <- purrr::map_int(r, \(i){
			ifelse(
				typeof(i) %in% c("character", "symbol", "call")
				, .named[[which(grepl(paste0("^", as.character(i)), names(.named)))]]
				, as.integer(i)
				)
		})

		# Recursive call to the function:
		.mod <- x %% k[[1]]

		radix(x = !!(10 * (x - .mod)/k[[1]] + .mod), !!r[[2]])
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
			}) |> 
			unlist(use.names = FALSE)
	}
}

#' @export
factor.int <- function(...) factor_int(...)

#' @export
count.cycles <- function(...) count_cycles(...)
