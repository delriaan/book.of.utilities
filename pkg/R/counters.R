count.cycles <- function(cond, offset = 1, reset){
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

factor.int <- function(i, ...){
	#' Factorization of Integers
	#'
	#' @param i (integer[]) One or more integers to factor
	#' @param ... \code{\link[rlang]{dots_list}}: Additional integers to factor (can be used in conjunction with \code{i})
	#'
	#' @return The factors of the input given as a vector or list of vectors
	#'
	#' @family Counters
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
