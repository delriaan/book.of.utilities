count.cycles <- function(cond, offset = 1, reset){
#' Cycle Counter
#'
#' \code{count.cycles} detects and maps conditional occurrence frequency
#'
#' @param cond (logical[]) A logical vector
#' @param offset (integer|1) An integer defining the series origin
#' @param reset (logical[]) A logical vector of the same length as \code{cond} such that when \code{TRUE}, the cycle counter resets to the offset
#'
#' @return A one-based (default), conditionally-incremented series
#'
#' @family Counters
#'
#' @export

	# :: cycle_idx MUST be initialized before session() (see below) is called iteratively
	offset = as.integer(offset)[1];
	cycle_idx = offset;

	# :: "counter()" is a function delegate and is the ESSENTIAL part of the routine
	# Note how it is contained WITHIN this specific code block and is designed for conditional increment
	# See http://adv-r.had.co.nz/Functional-programming.html, section "Mutable State"

	count_idx = iterators::iter(1:length(cond));

	# :: Define the counter function
	counter = function(tf) {
		idx = iterators::nextElem(count_idx);
	  if (tf) {
	  # cycle_idx only resets to the offset value when the limiter function returns 'TRUE'; otherwise, it increments
			cycle_idx <<- cycle_idx + 1
	  }

	  # cycle_idx only resets to the offset value when the limiter function returns 'TRUE'; otherwise, it increments
		cycle_idx <<- ifelse((idx %in% which(reset))|(idx == 1), offset, cycle_idx)

		# return the current value of 'cycle_idx'
		cycle_idx
	}

  # :: Call the counter function
  sapply(cond, counter);
}
#
factor.int <- function(i, ...){
#' Factorization of Integers
#'
#' @param i (integer[]) One or more integers to factor
#' @param ... Additional integers to factor (can be used in conjunction with \code{i})
#'
#' @return The factors of the input given as an vector or list of vectors
#'
#' @family Counters
#'
#' @export

	if (missing(i) & ...length() == 0){ message("[ERROR] Please supply integer values to either 'i' or '...'"); return(0); }
	if (missing(i) & ...length() != 0){ i <- 0 }
	i <- c(i, rlang::list2(...));

	if (is.list(i)){ i <- unlist(i, use.names = FALSE) }
	if (is.factor(i)){ i <- as.integer(i) }
	if (is.character(i)){ i <- as.integer(i) }
	if (is.numeric(i)){ i <- as.integer(i) }

	i <- purrr::set_names(i);
	.out <- purrr::map(i, ~{ .int = .x; purrr::keep(sequence(.int), ~.int %% .x == 0) });

	if (length(i) > 1){ distinct.list(.out[order(as.integer(names(.out)))]) } else { unlist(.out, use.names = FALSE) }
}
